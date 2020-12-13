{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of all the current mount targets, or a specific mount target, for a file system. When requesting all of the current mount targets, the order of mount targets returned in the response is unspecified.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeMountTargets@ action, on either the file system ID that you specify in @FileSystemId@ , or on the file system of the mount target that you specify in @MountTargetId@ .
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeMountTargets
  ( -- * Creating a request
    DescribeMountTargets (..),
    mkDescribeMountTargets,

    -- ** Request lenses
    dmtsAccessPointId,
    dmtsFileSystemId,
    dmtsMarker,
    dmtsMaxItems,
    dmtsMountTargetId,

    -- * Destructuring the response
    DescribeMountTargetsResponse (..),
    mkDescribeMountTargetsResponse,

    -- ** Response lenses
    dmtrsMountTargets,
    dmtrsMarker,
    dmtrsNextMarker,
    dmtrsResponseStatus,
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
-- /See:/ 'mkDescribeMountTargets' smart constructor.
data DescribeMountTargets = DescribeMountTargets'
  { -- | (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
    accessPointId :: Lude.Maybe Lude.Text,
    -- | (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
    fileSystemId :: Lude.Maybe Lude.Text,
    -- | (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
    marker :: Lude.Maybe Lude.Text,
    -- | (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
    mountTargetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMountTargets' with the minimum fields required to make a request.
--
-- * 'accessPointId' - (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
-- * 'fileSystemId' - (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
-- * 'marker' - (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
-- * 'maxItems' - (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
-- * 'mountTargetId' - (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
mkDescribeMountTargets ::
  DescribeMountTargets
mkDescribeMountTargets =
  DescribeMountTargets'
    { accessPointId = Lude.Nothing,
      fileSystemId = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      mountTargetId = Lude.Nothing
    }

-- | (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsAccessPointId :: Lens.Lens' DescribeMountTargets (Lude.Maybe Lude.Text)
dmtsAccessPointId = Lens.lens (accessPointId :: DescribeMountTargets -> Lude.Maybe Lude.Text) (\s a -> s {accessPointId = a} :: DescribeMountTargets)
{-# DEPRECATED dmtsAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

-- | (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsFileSystemId :: Lens.Lens' DescribeMountTargets (Lude.Maybe Lude.Text)
dmtsFileSystemId = Lens.lens (fileSystemId :: DescribeMountTargets -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeMountTargets)
{-# DEPRECATED dmtsFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsMarker :: Lens.Lens' DescribeMountTargets (Lude.Maybe Lude.Text)
dmtsMarker = Lens.lens (marker :: DescribeMountTargets -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeMountTargets)
{-# DEPRECATED dmtsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsMaxItems :: Lens.Lens' DescribeMountTargets (Lude.Maybe Lude.Natural)
dmtsMaxItems = Lens.lens (maxItems :: DescribeMountTargets -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: DescribeMountTargets)
{-# DEPRECATED dmtsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsMountTargetId :: Lens.Lens' DescribeMountTargets (Lude.Maybe Lude.Text)
dmtsMountTargetId = Lens.lens (mountTargetId :: DescribeMountTargets -> Lude.Maybe Lude.Text) (\s a -> s {mountTargetId = a} :: DescribeMountTargets)
{-# DEPRECATED dmtsMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

instance Page.AWSPager DescribeMountTargets where
  page rq rs
    | Page.stop (rs Lens.^. dmtrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dmtrsMountTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmtsMarker Lens..~ rs Lens.^. dmtrsNextMarker

instance Lude.AWSRequest DescribeMountTargets where
  type Rs DescribeMountTargets = DescribeMountTargetsResponse
  request = Req.get efsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMountTargetsResponse'
            Lude.<$> (x Lude..?> "MountTargets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMountTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeMountTargets where
  toPath = Lude.const "/2015-02-01/mount-targets"

instance Lude.ToQuery DescribeMountTargets where
  toQuery DescribeMountTargets' {..} =
    Lude.mconcat
      [ "AccessPointId" Lude.=: accessPointId,
        "FileSystemId" Lude.=: fileSystemId,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "MountTargetId" Lude.=: mountTargetId
      ]

-- |
--
-- /See:/ 'mkDescribeMountTargetsResponse' smart constructor.
data DescribeMountTargetsResponse = DescribeMountTargetsResponse'
  { -- | Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
    mountTargets :: Lude.Maybe [MountTargetDescription],
    -- | If the request included the @Marker@ , the response returns that value in this field.
    marker :: Lude.Maybe Lude.Text,
    -- | If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMountTargetsResponse' with the minimum fields required to make a request.
--
-- * 'mountTargets' - Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
-- * 'marker' - If the request included the @Marker@ , the response returns that value in this field.
-- * 'nextMarker' - If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
-- * 'responseStatus' - The response status code.
mkDescribeMountTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMountTargetsResponse
mkDescribeMountTargetsResponse pResponseStatus_ =
  DescribeMountTargetsResponse'
    { mountTargets = Lude.Nothing,
      marker = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
--
-- /Note:/ Consider using 'mountTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsMountTargets :: Lens.Lens' DescribeMountTargetsResponse (Lude.Maybe [MountTargetDescription])
dmtrsMountTargets = Lens.lens (mountTargets :: DescribeMountTargetsResponse -> Lude.Maybe [MountTargetDescription]) (\s a -> s {mountTargets = a} :: DescribeMountTargetsResponse)
{-# DEPRECATED dmtrsMountTargets "Use generic-lens or generic-optics with 'mountTargets' instead." #-}

-- | If the request included the @Marker@ , the response returns that value in this field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsMarker :: Lens.Lens' DescribeMountTargetsResponse (Lude.Maybe Lude.Text)
dmtrsMarker = Lens.lens (marker :: DescribeMountTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeMountTargetsResponse)
{-# DEPRECATED dmtrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsNextMarker :: Lens.Lens' DescribeMountTargetsResponse (Lude.Maybe Lude.Text)
dmtrsNextMarker = Lens.lens (nextMarker :: DescribeMountTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeMountTargetsResponse)
{-# DEPRECATED dmtrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsResponseStatus :: Lens.Lens' DescribeMountTargetsResponse Lude.Int
dmtrsResponseStatus = Lens.lens (responseStatus :: DescribeMountTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMountTargetsResponse)
{-# DEPRECATED dmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
