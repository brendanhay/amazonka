{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeFileSystems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS file system if either the file system @CreationToken@ or the @FileSystemId@ is provided. Otherwise, it returns descriptions of all file systems owned by the caller's AWS account in the AWS Region of the endpoint that you're calling.
--
-- When retrieving all file system descriptions, you can optionally specify the @MaxItems@ parameter to limit the number of descriptions in a response. Currently, this number is automatically set to 10. If more file system descriptions remain, Amazon EFS returns a @NextMarker@ , an opaque token, in the response. In this case, you should send a subsequent request with the @Marker@ request parameter set to the value of @NextMarker@ .
-- To retrieve a list of your file system descriptions, this operation is used in an iterative process, where @DescribeFileSystems@ is called first without the @Marker@ and then the operation continues to call it with the @Marker@ parameter set to the value of the @NextMarker@ from the previous response until the response has no @NextMarker@ .
-- The order of file systems returned in the response of one @DescribeFileSystems@ call and the order of file systems returned across the responses of a multi-call iteration is unspecified.
-- This operation requires permissions for the @elasticfilesystem:DescribeFileSystems@ action.
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeFileSystems
  ( -- * Creating a request
    DescribeFileSystems (..),
    mkDescribeFileSystems,

    -- ** Request lenses
    dfsFileSystemId,
    dfsCreationToken,
    dfsMarker,
    dfsMaxItems,

    -- * Destructuring the response
    DescribeFileSystemsResponse (..),
    mkDescribeFileSystemsResponse,

    -- ** Response lenses
    dfsrsFileSystems,
    dfsrsMarker,
    dfsrsNextMarker,
    dfsrsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeFileSystems' smart constructor.
data DescribeFileSystems = DescribeFileSystems'
  { -- | (Optional) ID of the file system whose description you want to retrieve (String).
    fileSystemId :: Lude.Maybe Lude.Text,
    -- | (Optional) Restricts the list to the file system with this creation token (String). You specify a creation token when you create an Amazon EFS file system.
    creationToken :: Lude.Maybe Lude.Text,
    -- | (Optional) Opaque pagination token returned from a previous @DescribeFileSystems@ operation (String). If present, specifies to continue the list from where the returning call had left off.
    marker :: Lude.Maybe Lude.Text,
    -- | (Optional) Specifies the maximum number of file systems to return in the response (integer). This number is automatically set to 100. The response is paginated at 100 per page if you have more than 100 file systems.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFileSystems' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - (Optional) ID of the file system whose description you want to retrieve (String).
-- * 'creationToken' - (Optional) Restricts the list to the file system with this creation token (String). You specify a creation token when you create an Amazon EFS file system.
-- * 'marker' - (Optional) Opaque pagination token returned from a previous @DescribeFileSystems@ operation (String). If present, specifies to continue the list from where the returning call had left off.
-- * 'maxItems' - (Optional) Specifies the maximum number of file systems to return in the response (integer). This number is automatically set to 100. The response is paginated at 100 per page if you have more than 100 file systems.
mkDescribeFileSystems ::
  DescribeFileSystems
mkDescribeFileSystems =
  DescribeFileSystems'
    { fileSystemId = Lude.Nothing,
      creationToken = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | (Optional) ID of the file system whose description you want to retrieve (String).
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsFileSystemId :: Lens.Lens' DescribeFileSystems (Lude.Maybe Lude.Text)
dfsFileSystemId = Lens.lens (fileSystemId :: DescribeFileSystems -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeFileSystems)
{-# DEPRECATED dfsFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | (Optional) Restricts the list to the file system with this creation token (String). You specify a creation token when you create an Amazon EFS file system.
--
-- /Note:/ Consider using 'creationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsCreationToken :: Lens.Lens' DescribeFileSystems (Lude.Maybe Lude.Text)
dfsCreationToken = Lens.lens (creationToken :: DescribeFileSystems -> Lude.Maybe Lude.Text) (\s a -> s {creationToken = a} :: DescribeFileSystems)
{-# DEPRECATED dfsCreationToken "Use generic-lens or generic-optics with 'creationToken' instead." #-}

-- | (Optional) Opaque pagination token returned from a previous @DescribeFileSystems@ operation (String). If present, specifies to continue the list from where the returning call had left off.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsMarker :: Lens.Lens' DescribeFileSystems (Lude.Maybe Lude.Text)
dfsMarker = Lens.lens (marker :: DescribeFileSystems -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeFileSystems)
{-# DEPRECATED dfsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) Specifies the maximum number of file systems to return in the response (integer). This number is automatically set to 100. The response is paginated at 100 per page if you have more than 100 file systems.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsMaxItems :: Lens.Lens' DescribeFileSystems (Lude.Maybe Lude.Natural)
dfsMaxItems = Lens.lens (maxItems :: DescribeFileSystems -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: DescribeFileSystems)
{-# DEPRECATED dfsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager DescribeFileSystems where
  page rq rs
    | Page.stop (rs Lens.^. dfsrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dfsrsFileSystems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfsMarker Lens..~ rs Lens.^. dfsrsNextMarker

instance Lude.AWSRequest DescribeFileSystems where
  type Rs DescribeFileSystems = DescribeFileSystemsResponse
  request = Req.get efsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFileSystemsResponse'
            Lude.<$> (x Lude..?> "FileSystems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFileSystems where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFileSystems where
  toPath = Lude.const "/2015-02-01/file-systems"

instance Lude.ToQuery DescribeFileSystems where
  toQuery DescribeFileSystems' {..} =
    Lude.mconcat
      [ "FileSystemId" Lude.=: fileSystemId,
        "CreationToken" Lude.=: creationToken,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkDescribeFileSystemsResponse' smart constructor.
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'
  { -- | An array of file system descriptions.
    fileSystems :: Lude.Maybe [FileSystemDescription],
    -- | Present if provided by caller in the request (String).
    marker :: Lude.Maybe Lude.Text,
    -- | Present if there are more file systems than returned in the response (String). You can use the @NextMarker@ in the subsequent request to fetch the descriptions.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFileSystemsResponse' with the minimum fields required to make a request.
--
-- * 'fileSystems' - An array of file system descriptions.
-- * 'marker' - Present if provided by caller in the request (String).
-- * 'nextMarker' - Present if there are more file systems than returned in the response (String). You can use the @NextMarker@ in the subsequent request to fetch the descriptions.
-- * 'responseStatus' - The response status code.
mkDescribeFileSystemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFileSystemsResponse
mkDescribeFileSystemsResponse pResponseStatus_ =
  DescribeFileSystemsResponse'
    { fileSystems = Lude.Nothing,
      marker = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of file system descriptions.
--
-- /Note:/ Consider using 'fileSystems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsFileSystems :: Lens.Lens' DescribeFileSystemsResponse (Lude.Maybe [FileSystemDescription])
dfsrsFileSystems = Lens.lens (fileSystems :: DescribeFileSystemsResponse -> Lude.Maybe [FileSystemDescription]) (\s a -> s {fileSystems = a} :: DescribeFileSystemsResponse)
{-# DEPRECATED dfsrsFileSystems "Use generic-lens or generic-optics with 'fileSystems' instead." #-}

-- | Present if provided by caller in the request (String).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsMarker :: Lens.Lens' DescribeFileSystemsResponse (Lude.Maybe Lude.Text)
dfsrsMarker = Lens.lens (marker :: DescribeFileSystemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeFileSystemsResponse)
{-# DEPRECATED dfsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Present if there are more file systems than returned in the response (String). You can use the @NextMarker@ in the subsequent request to fetch the descriptions.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsNextMarker :: Lens.Lens' DescribeFileSystemsResponse (Lude.Maybe Lude.Text)
dfsrsNextMarker = Lens.lens (nextMarker :: DescribeFileSystemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeFileSystemsResponse)
{-# DEPRECATED dfsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsResponseStatus :: Lens.Lens' DescribeFileSystemsResponse Lude.Int
dfsrsResponseStatus = Lens.lens (responseStatus :: DescribeFileSystemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFileSystemsResponse)
{-# DEPRECATED dfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
