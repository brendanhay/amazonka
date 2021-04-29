{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
    -- descriptions of the volumes associated with the specified instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
    -- returns descriptions of the specified volumes. Otherwise, it returns a
    -- description of every volume.
    volumeIds :: Prelude.Maybe [Prelude.Text],
    -- | A stack ID. The action describes the stack\'s registered Amazon EBS
    -- volumes.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
    -- descriptions of the volumes associated with the specified RAID array.
    raidArrayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      volumeIds = Prelude.Nothing,
      stackId = Prelude.Nothing,
      raidArrayId = Prelude.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
describeVolumes_instanceId :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Text)
describeVolumes_instanceId = Lens.lens (\DescribeVolumes' {instanceId} -> instanceId) (\s@DescribeVolumes' {} a -> s {instanceId = a} :: DescribeVolumes)

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
describeVolumes_volumeIds :: Lens.Lens' DescribeVolumes (Prelude.Maybe [Prelude.Text])
describeVolumes_volumeIds = Lens.lens (\DescribeVolumes' {volumeIds} -> volumeIds) (\s@DescribeVolumes' {} a -> s {volumeIds = a} :: DescribeVolumes) Prelude.. Lens.mapping Prelude._Coerce

-- | A stack ID. The action describes the stack\'s registered Amazon EBS
-- volumes.
describeVolumes_stackId :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Text)
describeVolumes_stackId = Lens.lens (\DescribeVolumes' {stackId} -> stackId) (\s@DescribeVolumes' {} a -> s {stackId = a} :: DescribeVolumes)

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
describeVolumes_raidArrayId :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Text)
describeVolumes_raidArrayId = Lens.lens (\DescribeVolumes' {raidArrayId} -> raidArrayId) (\s@DescribeVolumes' {} a -> s {raidArrayId = a} :: DescribeVolumes)

instance Prelude.AWSRequest DescribeVolumes where
  type Rs DescribeVolumes = DescribeVolumesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVolumesResponse'
            Prelude.<$> (x Prelude..?> "Volumes" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVolumes

instance Prelude.NFData DescribeVolumes

instance Prelude.ToHeaders DescribeVolumes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DescribeVolumes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeVolumes where
  toJSON DescribeVolumes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceId" Prelude..=) Prelude.<$> instanceId,
            ("VolumeIds" Prelude..=) Prelude.<$> volumeIds,
            ("StackId" Prelude..=) Prelude.<$> stackId,
            ("RaidArrayId" Prelude..=) Prelude.<$> raidArrayId
          ]
      )

instance Prelude.ToPath DescribeVolumes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeVolumes where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'newDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | An array of volume IDs.
    volumes :: Prelude.Maybe [Volume],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVolumesResponse
newDescribeVolumesResponse pHttpStatus_ =
  DescribeVolumesResponse'
    { volumes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of volume IDs.
describeVolumesResponse_volumes :: Lens.Lens' DescribeVolumesResponse (Prelude.Maybe [Volume])
describeVolumesResponse_volumes = Lens.lens (\DescribeVolumesResponse' {volumes} -> volumes) (\s@DescribeVolumesResponse' {} a -> s {volumes = a} :: DescribeVolumesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeVolumesResponse_httpStatus :: Lens.Lens' DescribeVolumesResponse Prelude.Int
describeVolumesResponse_httpStatus = Lens.lens (\DescribeVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesResponse' {} a -> s {httpStatus = a} :: DescribeVolumesResponse)

instance Prelude.NFData DescribeVolumesResponse
