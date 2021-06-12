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
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an instance\'s Amazon EBS volumes.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeVolumes
  ( -- * Creating a Request
    DescribeVolumes (..),
    newDescribeVolumes,

    -- * Request Lenses
    describeVolumes_instanceId,
    describeVolumes_volumeIds,
    describeVolumes_stackId,
    describeVolumes_raidArrayId,

    -- * Destructuring the Response
    DescribeVolumesResponse (..),
    newDescribeVolumesResponse,

    -- * Response Lenses
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
    -- descriptions of the volumes associated with the specified instance.
    instanceId :: Core.Maybe Core.Text,
    -- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
    -- returns descriptions of the specified volumes. Otherwise, it returns a
    -- description of every volume.
    volumeIds :: Core.Maybe [Core.Text],
    -- | A stack ID. The action describes the stack\'s registered Amazon EBS
    -- volumes.
    stackId :: Core.Maybe Core.Text,
    -- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
    -- descriptions of the volumes associated with the specified RAID array.
    raidArrayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeVolumes_instanceId' - The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
--
-- 'volumeIds', 'describeVolumes_volumeIds' - Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
--
-- 'stackId', 'describeVolumes_stackId' - A stack ID. The action describes the stack\'s registered Amazon EBS
-- volumes.
--
-- 'raidArrayId', 'describeVolumes_raidArrayId' - The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
newDescribeVolumes ::
  DescribeVolumes
newDescribeVolumes =
  DescribeVolumes'
    { instanceId = Core.Nothing,
      volumeIds = Core.Nothing,
      stackId = Core.Nothing,
      raidArrayId = Core.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
describeVolumes_instanceId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
describeVolumes_instanceId = Lens.lens (\DescribeVolumes' {instanceId} -> instanceId) (\s@DescribeVolumes' {} a -> s {instanceId = a} :: DescribeVolumes)

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
describeVolumes_volumeIds :: Lens.Lens' DescribeVolumes (Core.Maybe [Core.Text])
describeVolumes_volumeIds = Lens.lens (\DescribeVolumes' {volumeIds} -> volumeIds) (\s@DescribeVolumes' {} a -> s {volumeIds = a} :: DescribeVolumes) Core.. Lens.mapping Lens._Coerce

-- | A stack ID. The action describes the stack\'s registered Amazon EBS
-- volumes.
describeVolumes_stackId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
describeVolumes_stackId = Lens.lens (\DescribeVolumes' {stackId} -> stackId) (\s@DescribeVolumes' {} a -> s {stackId = a} :: DescribeVolumes)

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
describeVolumes_raidArrayId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
describeVolumes_raidArrayId = Lens.lens (\DescribeVolumes' {raidArrayId} -> raidArrayId) (\s@DescribeVolumes' {} a -> s {raidArrayId = a} :: DescribeVolumes)

instance Core.AWSRequest DescribeVolumes where
  type
    AWSResponse DescribeVolumes =
      DescribeVolumesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVolumesResponse'
            Core.<$> (x Core..?> "Volumes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVolumes

instance Core.NFData DescribeVolumes

instance Core.ToHeaders DescribeVolumes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeVolumes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVolumes where
  toJSON DescribeVolumes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("VolumeIds" Core..=) Core.<$> volumeIds,
            ("StackId" Core..=) Core.<$> stackId,
            ("RaidArrayId" Core..=) Core.<$> raidArrayId
          ]
      )

instance Core.ToPath DescribeVolumes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVolumes where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'newDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | An array of volume IDs.
    volumes :: Core.Maybe [Volume],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumes', 'describeVolumesResponse_volumes' - An array of volume IDs.
--
-- 'httpStatus', 'describeVolumesResponse_httpStatus' - The response's http status code.
newDescribeVolumesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVolumesResponse
newDescribeVolumesResponse pHttpStatus_ =
  DescribeVolumesResponse'
    { volumes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of volume IDs.
describeVolumesResponse_volumes :: Lens.Lens' DescribeVolumesResponse (Core.Maybe [Volume])
describeVolumesResponse_volumes = Lens.lens (\DescribeVolumesResponse' {volumes} -> volumes) (\s@DescribeVolumesResponse' {} a -> s {volumes = a} :: DescribeVolumesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVolumesResponse_httpStatus :: Lens.Lens' DescribeVolumesResponse Core.Int
describeVolumesResponse_httpStatus = Lens.lens (\DescribeVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesResponse' {} a -> s {httpStatus = a} :: DescribeVolumesResponse)

instance Core.NFData DescribeVolumesResponse
