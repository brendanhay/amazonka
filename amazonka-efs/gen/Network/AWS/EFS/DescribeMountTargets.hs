{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of all the current mount targets, or a specific
-- mount target, for a file system. When requesting all of the current
-- mount targets, the order of mount targets returned in the response is
-- unspecified.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeMountTargets@ action, on either the file
-- system ID that you specify in @FileSystemId@, or on the file system of
-- the mount target that you specify in @MountTargetId@.
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeMountTargets
  ( -- * Creating a Request
    DescribeMountTargets (..),
    newDescribeMountTargets,

    -- * Request Lenses
    describeMountTargets_mountTargetId,
    describeMountTargets_accessPointId,
    describeMountTargets_fileSystemId,
    describeMountTargets_maxItems,
    describeMountTargets_marker,

    -- * Destructuring the Response
    DescribeMountTargetsResponse (..),
    newDescribeMountTargetsResponse,

    -- * Response Lenses
    describeMountTargetsResponse_nextMarker,
    describeMountTargetsResponse_mountTargets,
    describeMountTargetsResponse_marker,
    describeMountTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeMountTargets' smart constructor.
data DescribeMountTargets = DescribeMountTargets'
  { -- | (Optional) ID of the mount target that you want to have described
    -- (String). It must be included in your request if @FileSystemId@ is not
    -- included. Accepts either a mount target ID or ARN as input.
    mountTargetId :: Core.Maybe Core.Text,
    -- | (Optional) The ID of the access point whose mount targets that you want
    -- to list. It must be included in your request if a @FileSystemId@ or
    -- @MountTargetId@ is not included in your request. Accepts either an
    -- access point ID or ARN as input.
    accessPointId :: Core.Maybe Core.Text,
    -- | (Optional) ID of the file system whose mount targets you want to list
    -- (String). It must be included in your request if an @AccessPointId@ or
    -- @MountTargetId@ is not included. Accepts either a file system ID or ARN
    -- as input.
    fileSystemId :: Core.Maybe Core.Text,
    -- | (Optional) Maximum number of mount targets to return in the response.
    -- Currently, this number is automatically set to 10, and other values are
    -- ignored. The response is paginated at 100 per page if you have more than
    -- 100 mount targets.
    maxItems :: Core.Maybe Core.Natural,
    -- | (Optional) Opaque pagination token returned from a previous
    -- @DescribeMountTargets@ operation (String). If present, it specifies to
    -- continue the list from where the previous returning call left off.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMountTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountTargetId', 'describeMountTargets_mountTargetId' - (Optional) ID of the mount target that you want to have described
-- (String). It must be included in your request if @FileSystemId@ is not
-- included. Accepts either a mount target ID or ARN as input.
--
-- 'accessPointId', 'describeMountTargets_accessPointId' - (Optional) The ID of the access point whose mount targets that you want
-- to list. It must be included in your request if a @FileSystemId@ or
-- @MountTargetId@ is not included in your request. Accepts either an
-- access point ID or ARN as input.
--
-- 'fileSystemId', 'describeMountTargets_fileSystemId' - (Optional) ID of the file system whose mount targets you want to list
-- (String). It must be included in your request if an @AccessPointId@ or
-- @MountTargetId@ is not included. Accepts either a file system ID or ARN
-- as input.
--
-- 'maxItems', 'describeMountTargets_maxItems' - (Optional) Maximum number of mount targets to return in the response.
-- Currently, this number is automatically set to 10, and other values are
-- ignored. The response is paginated at 100 per page if you have more than
-- 100 mount targets.
--
-- 'marker', 'describeMountTargets_marker' - (Optional) Opaque pagination token returned from a previous
-- @DescribeMountTargets@ operation (String). If present, it specifies to
-- continue the list from where the previous returning call left off.
newDescribeMountTargets ::
  DescribeMountTargets
newDescribeMountTargets =
  DescribeMountTargets'
    { mountTargetId = Core.Nothing,
      accessPointId = Core.Nothing,
      fileSystemId = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | (Optional) ID of the mount target that you want to have described
-- (String). It must be included in your request if @FileSystemId@ is not
-- included. Accepts either a mount target ID or ARN as input.
describeMountTargets_mountTargetId :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Text)
describeMountTargets_mountTargetId = Lens.lens (\DescribeMountTargets' {mountTargetId} -> mountTargetId) (\s@DescribeMountTargets' {} a -> s {mountTargetId = a} :: DescribeMountTargets)

-- | (Optional) The ID of the access point whose mount targets that you want
-- to list. It must be included in your request if a @FileSystemId@ or
-- @MountTargetId@ is not included in your request. Accepts either an
-- access point ID or ARN as input.
describeMountTargets_accessPointId :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Text)
describeMountTargets_accessPointId = Lens.lens (\DescribeMountTargets' {accessPointId} -> accessPointId) (\s@DescribeMountTargets' {} a -> s {accessPointId = a} :: DescribeMountTargets)

-- | (Optional) ID of the file system whose mount targets you want to list
-- (String). It must be included in your request if an @AccessPointId@ or
-- @MountTargetId@ is not included. Accepts either a file system ID or ARN
-- as input.
describeMountTargets_fileSystemId :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Text)
describeMountTargets_fileSystemId = Lens.lens (\DescribeMountTargets' {fileSystemId} -> fileSystemId) (\s@DescribeMountTargets' {} a -> s {fileSystemId = a} :: DescribeMountTargets)

-- | (Optional) Maximum number of mount targets to return in the response.
-- Currently, this number is automatically set to 10, and other values are
-- ignored. The response is paginated at 100 per page if you have more than
-- 100 mount targets.
describeMountTargets_maxItems :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Natural)
describeMountTargets_maxItems = Lens.lens (\DescribeMountTargets' {maxItems} -> maxItems) (\s@DescribeMountTargets' {} a -> s {maxItems = a} :: DescribeMountTargets)

-- | (Optional) Opaque pagination token returned from a previous
-- @DescribeMountTargets@ operation (String). If present, it specifies to
-- continue the list from where the previous returning call left off.
describeMountTargets_marker :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Text)
describeMountTargets_marker = Lens.lens (\DescribeMountTargets' {marker} -> marker) (\s@DescribeMountTargets' {} a -> s {marker = a} :: DescribeMountTargets)

instance Core.AWSPager DescribeMountTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMountTargetsResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMountTargetsResponse_mountTargets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMountTargets_marker
          Lens..~ rs
          Lens.^? describeMountTargetsResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest DescribeMountTargets where
  type
    AWSResponse DescribeMountTargets =
      DescribeMountTargetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMountTargetsResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "MountTargets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMountTargets

instance Core.NFData DescribeMountTargets

instance Core.ToHeaders DescribeMountTargets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeMountTargets where
  toPath = Core.const "/2015-02-01/mount-targets"

instance Core.ToQuery DescribeMountTargets where
  toQuery DescribeMountTargets' {..} =
    Core.mconcat
      [ "MountTargetId" Core.=: mountTargetId,
        "AccessPointId" Core.=: accessPointId,
        "FileSystemId" Core.=: fileSystemId,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- |
--
-- /See:/ 'newDescribeMountTargetsResponse' smart constructor.
data DescribeMountTargetsResponse = DescribeMountTargetsResponse'
  { -- | If a value is present, there are more mount targets to return. In a
    -- subsequent request, you can provide @Marker@ in your request with this
    -- value to retrieve the next set of mount targets.
    nextMarker :: Core.Maybe Core.Text,
    -- | Returns the file system\'s mount targets as an array of
    -- @MountTargetDescription@ objects.
    mountTargets :: Core.Maybe [MountTargetDescription],
    -- | If the request included the @Marker@, the response returns that value in
    -- this field.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMountTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeMountTargetsResponse_nextMarker' - If a value is present, there are more mount targets to return. In a
-- subsequent request, you can provide @Marker@ in your request with this
-- value to retrieve the next set of mount targets.
--
-- 'mountTargets', 'describeMountTargetsResponse_mountTargets' - Returns the file system\'s mount targets as an array of
-- @MountTargetDescription@ objects.
--
-- 'marker', 'describeMountTargetsResponse_marker' - If the request included the @Marker@, the response returns that value in
-- this field.
--
-- 'httpStatus', 'describeMountTargetsResponse_httpStatus' - The response's http status code.
newDescribeMountTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMountTargetsResponse
newDescribeMountTargetsResponse pHttpStatus_ =
  DescribeMountTargetsResponse'
    { nextMarker =
        Core.Nothing,
      mountTargets = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a value is present, there are more mount targets to return. In a
-- subsequent request, you can provide @Marker@ in your request with this
-- value to retrieve the next set of mount targets.
describeMountTargetsResponse_nextMarker :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe Core.Text)
describeMountTargetsResponse_nextMarker = Lens.lens (\DescribeMountTargetsResponse' {nextMarker} -> nextMarker) (\s@DescribeMountTargetsResponse' {} a -> s {nextMarker = a} :: DescribeMountTargetsResponse)

-- | Returns the file system\'s mount targets as an array of
-- @MountTargetDescription@ objects.
describeMountTargetsResponse_mountTargets :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe [MountTargetDescription])
describeMountTargetsResponse_mountTargets = Lens.lens (\DescribeMountTargetsResponse' {mountTargets} -> mountTargets) (\s@DescribeMountTargetsResponse' {} a -> s {mountTargets = a} :: DescribeMountTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | If the request included the @Marker@, the response returns that value in
-- this field.
describeMountTargetsResponse_marker :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe Core.Text)
describeMountTargetsResponse_marker = Lens.lens (\DescribeMountTargetsResponse' {marker} -> marker) (\s@DescribeMountTargetsResponse' {} a -> s {marker = a} :: DescribeMountTargetsResponse)

-- | The response's http status code.
describeMountTargetsResponse_httpStatus :: Lens.Lens' DescribeMountTargetsResponse Core.Int
describeMountTargetsResponse_httpStatus = Lens.lens (\DescribeMountTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeMountTargetsResponse' {} a -> s {httpStatus = a} :: DescribeMountTargetsResponse)

instance Core.NFData DescribeMountTargetsResponse
