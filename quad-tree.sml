structure QuadTree =
struct
  (* We have a 2D array/vector.
   * The first dimension represents the X axis of a 2D grid,
   * and the second dimension represents the Y axis. *)
  fun loopY (curX, curY, endX, endY, yAxis, col) =
    if curY > endY orelse curY >= Vector.length yAxis then
      (* return true if y position is out of range of the quadrant,
       * since we examined every pixel and found it to be the same. *)
      true
    else
      let
        val newCol = Vector.sub (yAxis, curY)
      in
        if col = newCol then
          (* we continue looping if the current colour is the same *)
          loopY (curX, curY + 1, endX, endY, yAxis, col)
        else
          false
      end

  fun loopX (curX, curY, endX, endY, grid, col) =
    if curX > endX orelse curX >= Vector.length grid then
      true
    else
      let
        val yAxis = Vector.sub (grid, curX)
      in
        if loopY (curX, curY, endX, endY, yAxis, col) then
          loopX (curX + 1, curY, endX, endY, grid, col)
        else
          false
      end

  fun quadHasSameColour (startX, startY, size, grid) =
    let
      val yAxis = Vector.sub (grid, startX)
      val col = Vector.sub (yAxis, startY)

      val endX = startX + size
      val endY = startY + size
    in
      loopX (startX, startY, endX, endY, grid, col)
    end

  (* returns half of the size of the current quadrant,
   * handling odd numbers too. *)
  fun getHalfSize size =
    let
      val halfSizeBefore = size div 2
      val halfSizeAfter = size - halfSizeBefore
    in
      {halfSizeBefore = halfSizeBefore, halfSizeAfter = halfSizeAfter}
    end

  (* datatype for a quad tree which has no overlapping items *)
  datatype 'a quad_tree =
    Empty
  | Leaf of {x: int, y: int, size: int, data: 'a}
  | Branch of
      { topLeft: 'a quad_tree
      , topRight: 'a quad_tree
      , bottomLeft: 'a quad_tree
      , bottomRight: 'a quad_tree
      }

  (* Building a quad tree *)
  fun buildTreeLoop (x, y, size, grid) =
    if x >= Vector.length grid orelse y >= Vector.length grid then
      (* we are out of range, so return Empty *)
      Empty
    else if quadHasSameColour (x, y, size, grid) then
      (* base csae for recursion: all colours in quadrant are the same *)
      let
        val yAxis = Vector.sub (grid, x)
        val data = Vector.sub (yAxis, y)
        val item = {x = x, y = y, size = size, data = data}
      in
        Leaf item
      end
    else
      (* quadrant has different colours, so divide further *)
      let
        val {halfSizeBefore, halfSizeAfter} = getHalfSize size

        val topLeft = buildTreeLoop (x, y, halfSizeAfter, grid)

        val topRight =
          buildTreeLoop (x + halfSizeBefore, y, halfSizeAfter, grid)

        val bottomLeft =
          buildTreeLoop (x, y + halfSizeBefore, halfSizeAfter, grid)

        val bottomRight =
          buildTreeLoop
            (x + halfSizeBefore, y + halfSizeBefore, halfSizeAfter, grid)
      in
        Branch
          { topLeft = topLeft
          , topRight = topRight
          , bottomLeft = bottomLeft
          , bottomRight = bottomRight
          }
      end

  fun buildTree (size, grid) = buildTreeLoop (0, 0, size, grid)

  (* Building a list using the quad tree decomposition algorithm,
   * but without actually creating a quad_tree type from the result. *)
  fun buildListLoop (x, y, size, grid, listAcc) =
    if x >= Vector.length grid orelse y >= Vector.length grid then
      (* we are out of range, so return list unmodified *)
      listAcc
    else if quadHasSameColour (x, y, size, grid) then
      (* base csae for recursion: all colours in quadrant are the same *)
      let
        val yAxis = Vector.sub (grid, x)
        val data = Vector.sub (yAxis, y)
        val item = {x = x, y = y, size = size, data = data}
      in
        item :: listAcc
      end
    else
      (* quadrant has different colours, so divide further *)
      let
        val {halfSizeBefore, halfSizeAfter} = getHalfSize size

        (* top left quadrant *)
        val listAcc = buildListLoop (x, y, halfSizeAfter, grid, listAcc)

        (* top right quadrant *)
        val listAcc = buildListLoop
          (x + halfSizeBefore, y, halfSizeAfter, grid, listAcc)

        (* bottom left quadrant *)
        val listAcc = buildListLoop
          (x, y + halfSizeBefore, halfSizeAfter, grid, listAcc)
      in
        (* bottom right quadrant *)
        buildListLoop
          (x + halfSizeBefore, y + halfSizeBefore, halfSizeAfter, grid, listAcc)
      end

  fun buildList (size, grid) =
    buildListLoop (0, 0, size, grid, [])
end
