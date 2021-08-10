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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeMountTargets' smart constructor.
data DescribeMountTargets = DescribeMountTargets'
  { -- | (Optional) ID of the mount target that you want to have described
    -- (String). It must be included in your request if @FileSystemId@ is not
    -- included. Accepts either a mount target ID or ARN as input.
    mountTargetId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The ID of the access point whose mount targets that you want
    -- to list. It must be included in your request if a @FileSystemId@ or
    -- @MountTargetId@ is not included in your request. Accepts either an
    -- access point ID or ARN as input.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) ID of the file system whose mount targets you want to list
    -- (String). It must be included in your request if an @AccessPointId@ or
    -- @MountTargetId@ is not included. Accepts either a file system ID or ARN
    -- as input.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Maximum number of mount targets to return in the response.
    -- Currently, this number is automatically set to 10, and other values are
    -- ignored. The response is paginated at 100 per page if you have more than
    -- 100 mount targets.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Opaque pagination token returned from a previous
    -- @DescribeMountTargets@ operation (String). If present, it specifies to
    -- continue the list from where the previous returning call left off.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { mountTargetId =
        Prelude.Nothing,
      accessPointId = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | (Optional) ID of the mount target that you want to have described
-- (String). It must be included in your request if @FileSystemId@ is not
-- included. Accepts either a mount target ID or ARN as input.
describeMountTargets_mountTargetId :: Lens.Lens' DescribeMountTargets (Prelude.Maybe Prelude.Text)
describeMountTargets_mountTargetId = Lens.lens (\DescribeMountTargets' {mountTargetId} -> mountTargetId) (\s@DescribeMountTargets' {} a -> s {mountTargetId = a} :: DescribeMountTargets)

-- | (Optional) The ID of the access point whose mount targets that you want
-- to list. It must be included in your request if a @FileSystemId@ or
-- @MountTargetId@ is not included in your request. Accepts either an
-- access point ID or ARN as input.
describeMountTargets_accessPointId :: Lens.Lens' DescribeMountTargets (Prelude.Maybe Prelude.Text)
describeMountTargets_accessPointId = Lens.lens (\DescribeMountTargets' {accessPointId} -> accessPointId) (\s@DescribeMountTargets' {} a -> s {accessPointId = a} :: DescribeMountTargets)

-- | (Optional) ID of the file system whose mount targets you want to list
-- (String). It must be included in your request if an @AccessPointId@ or
-- @MountTargetId@ is not included. Accepts either a file system ID or ARN
-- as input.
describeMountTargets_fileSystemId :: Lens.Lens' DescribeMountTargets (Prelude.Maybe Prelude.Text)
describeMountTargets_fileSystemId = Lens.lens (\DescribeMountTargets' {fileSystemId} -> fileSystemId) (\s@DescribeMountTargets' {} a -> s {fileSystemId = a} :: DescribeMountTargets)

-- | (Optional) Maximum number of mount targets to return in the response.
-- Currently, this number is automatically set to 10, and other values are
-- ignored. The response is paginated at 100 per page if you have more than
-- 100 mount targets.
describeMountTargets_maxItems :: Lens.Lens' DescribeMountTargets (Prelude.Maybe Prelude.Natural)
describeMountTargets_maxItems = Lens.lens (\DescribeMountTargets' {maxItems} -> maxItems) (\s@DescribeMountTargets' {} a -> s {maxItems = a} :: DescribeMountTargets)

-- | (Optional) Opaque pagination token returned from a previous
-- @DescribeMountTargets@ operation (String). If present, it specifies to
-- continue the list from where the previous returning call left off.
describeMountTargets_marker :: Lens.Lens' DescribeMountTargets (Prelude.Maybe Prelude.Text)
describeMountTargets_marker = Lens.lens (\DescribeMountTargets' {marker} -> marker) (\s@DescribeMountTargets' {} a -> s {marker = a} :: DescribeMountTargets)

instance Core.AWSPager DescribeMountTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMountTargetsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMountTargetsResponse_mountTargets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMountTargets_marker
          Lens..~ rs
          Lens.^? describeMountTargetsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeMountTargets where
  type
    AWSResponse DescribeMountTargets =
      DescribeMountTargetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMountTargetsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "MountTargets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMountTargets

instance Prelude.NFData DescribeMountTargets

instance Core.ToHeaders DescribeMountTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeMountTargets where
  toPath = Prelude.const "/2015-02-01/mount-targets"

instance Core.ToQuery DescribeMountTargets where
  toQuery DescribeMountTargets' {..} =
    Prelude.mconcat
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
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Returns the file system\'s mount targets as an array of
    -- @MountTargetDescription@ objects.
    mountTargets :: Prelude.Maybe [MountTargetDescription],
    -- | If the request included the @Marker@, the response returns that value in
    -- this field.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMountTargetsResponse
newDescribeMountTargetsResponse pHttpStatus_ =
  DescribeMountTargetsResponse'
    { nextMarker =
        Prelude.Nothing,
      mountTargets = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a value is present, there are more mount targets to return. In a
-- subsequent request, you can provide @Marker@ in your request with this
-- value to retrieve the next set of mount targets.
describeMountTargetsResponse_nextMarker :: Lens.Lens' DescribeMountTargetsResponse (Prelude.Maybe Prelude.Text)
describeMountTargetsResponse_nextMarker = Lens.lens (\DescribeMountTargetsResponse' {nextMarker} -> nextMarker) (\s@DescribeMountTargetsResponse' {} a -> s {nextMarker = a} :: DescribeMountTargetsResponse)

-- | Returns the file system\'s mount targets as an array of
-- @MountTargetDescription@ objects.
describeMountTargetsResponse_mountTargets :: Lens.Lens' DescribeMountTargetsResponse (Prelude.Maybe [MountTargetDescription])
describeMountTargetsResponse_mountTargets = Lens.lens (\DescribeMountTargetsResponse' {mountTargets} -> mountTargets) (\s@DescribeMountTargetsResponse' {} a -> s {mountTargets = a} :: DescribeMountTargetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If the request included the @Marker@, the response returns that value in
-- this field.
describeMountTargetsResponse_marker :: Lens.Lens' DescribeMountTargetsResponse (Prelude.Maybe Prelude.Text)
describeMountTargetsResponse_marker = Lens.lens (\DescribeMountTargetsResponse' {marker} -> marker) (\s@DescribeMountTargetsResponse' {} a -> s {marker = a} :: DescribeMountTargetsResponse)

-- | The response's http status code.
describeMountTargetsResponse_httpStatus :: Lens.Lens' DescribeMountTargetsResponse Prelude.Int
describeMountTargetsResponse_httpStatus = Lens.lens (\DescribeMountTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeMountTargetsResponse' {} a -> s {httpStatus = a} :: DescribeMountTargetsResponse)

instance Prelude.NFData DescribeMountTargetsResponse
