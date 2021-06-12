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
-- Module      : Network.AWS.EC2.ModifyVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can modify several parameters of an existing EBS volume, including
-- volume size, volume type, and IOPS capacity. If your EBS volume is
-- attached to a current-generation EC2 instance type, you might be able to
-- apply these changes without stopping the instance or detaching the
-- volume from it. For more information about modifying an EBS volume
-- running Linux, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Linux>.
-- For more information about modifying an EBS volume running Windows, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Windows>.
--
-- When you complete a resize operation on your volume, you need to extend
-- the volume\'s file-system size to take advantage of the new storage
-- capacity. For information about extending a Linux file system, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux Extending a Linux file system>.
-- For information about extending a Windows file system, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows Extending a Windows file system>.
--
-- You can use CloudWatch Events to check the status of a modification to
-- an EBS volume. For information about CloudWatch Events, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide>.
-- You can also track the status of a modification using
-- DescribeVolumesModifications. For information about tracking status
-- changes using either method, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring volume modifications>.
--
-- With previous-generation instance types, resizing an EBS volume might
-- require detaching and reattaching the volume or stopping and restarting
-- the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes>
-- (Linux) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes>
-- (Windows).
--
-- If you reach the maximum volume modification rate per volume limit, you
-- will need to wait at least six hours before applying further
-- modifications to the affected EBS volume.
module Network.AWS.EC2.ModifyVolume
  ( -- * Creating a Request
    ModifyVolume (..),
    newModifyVolume,

    -- * Request Lenses
    modifyVolume_multiAttachEnabled,
    modifyVolume_dryRun,
    modifyVolume_throughput,
    modifyVolume_volumeType,
    modifyVolume_iops,
    modifyVolume_size,
    modifyVolume_volumeId,

    -- * Destructuring the Response
    ModifyVolumeResponse (..),
    newModifyVolumeResponse,

    -- * Response Lenses
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { -- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable
    -- Multi-Attach, you can attach the volume to up to 16
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
    -- in the same Availability Zone. This parameter is supported with @io1@
    -- and @io2@ volumes only. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    multiAttachEnabled :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The target throughput of the volume, in MiB\/s. This parameter is valid
    -- only for @gp3@ volumes. The maximum value is 1,000.
    --
    -- Default: If no throughput value is specified, the existing value is
    -- retained.
    --
    -- Valid Range: Minimum value of 125. Maximum value of 1000.
    throughput :: Core.Maybe Core.Int,
    -- | The target EBS volume type of the volume. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- Default: If no type is specified, the existing type is retained.
    volumeType :: Core.Maybe VolumeType,
    -- | The target IOPS rate of the volume. This parameter is valid only for
    -- @gp3@, @io1@, and @io2@ volumes.
    --
    -- The following are the supported values for each volume type:
    --
    -- -   @gp3@: 3,000-16,000 IOPS
    --
    -- -   @io1@: 100-64,000 IOPS
    --
    -- -   @io2@: 100-64,000 IOPS
    --
    -- Default: If no IOPS value is specified, the existing value is retained.
    iops :: Core.Maybe Core.Int,
    -- | The target size of the volume, in GiB. The target volume size must be
    -- greater than or equal to the existing size of the volume.
    --
    -- The following are the supported volumes sizes for each volume type:
    --
    -- -   @gp2@ and @gp3@: 1-16,384
    --
    -- -   @io1@ and @io2@: 4-16,384
    --
    -- -   @st1@ and @sc1@: 125-16,384
    --
    -- -   @standard@: 1-1,024
    --
    -- Default: If no size is specified, the existing size is retained.
    size :: Core.Maybe Core.Int,
    -- | The ID of the volume.
    volumeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiAttachEnabled', 'modifyVolume_multiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'dryRun', 'modifyVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'throughput', 'modifyVolume_throughput' - The target throughput of the volume, in MiB\/s. This parameter is valid
-- only for @gp3@ volumes. The maximum value is 1,000.
--
-- Default: If no throughput value is specified, the existing value is
-- retained.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- 'volumeType', 'modifyVolume_volumeType' - The target EBS volume type of the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: If no type is specified, the existing type is retained.
--
-- 'iops', 'modifyVolume_iops' - The target IOPS rate of the volume. This parameter is valid only for
-- @gp3@, @io1@, and @io2@ volumes.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- Default: If no IOPS value is specified, the existing value is retained.
--
-- 'size', 'modifyVolume_size' - The target size of the volume, in GiB. The target volume size must be
-- greater than or equal to the existing size of the volume.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- Default: If no size is specified, the existing size is retained.
--
-- 'volumeId', 'modifyVolume_volumeId' - The ID of the volume.
newModifyVolume ::
  -- | 'volumeId'
  Core.Text ->
  ModifyVolume
newModifyVolume pVolumeId_ =
  ModifyVolume'
    { multiAttachEnabled = Core.Nothing,
      dryRun = Core.Nothing,
      throughput = Core.Nothing,
      volumeType = Core.Nothing,
      iops = Core.Nothing,
      size = Core.Nothing,
      volumeId = pVolumeId_
    }

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
modifyVolume_multiAttachEnabled :: Lens.Lens' ModifyVolume (Core.Maybe Core.Bool)
modifyVolume_multiAttachEnabled = Lens.lens (\ModifyVolume' {multiAttachEnabled} -> multiAttachEnabled) (\s@ModifyVolume' {} a -> s {multiAttachEnabled = a} :: ModifyVolume)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVolume_dryRun :: Lens.Lens' ModifyVolume (Core.Maybe Core.Bool)
modifyVolume_dryRun = Lens.lens (\ModifyVolume' {dryRun} -> dryRun) (\s@ModifyVolume' {} a -> s {dryRun = a} :: ModifyVolume)

-- | The target throughput of the volume, in MiB\/s. This parameter is valid
-- only for @gp3@ volumes. The maximum value is 1,000.
--
-- Default: If no throughput value is specified, the existing value is
-- retained.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
modifyVolume_throughput :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
modifyVolume_throughput = Lens.lens (\ModifyVolume' {throughput} -> throughput) (\s@ModifyVolume' {} a -> s {throughput = a} :: ModifyVolume)

-- | The target EBS volume type of the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: If no type is specified, the existing type is retained.
modifyVolume_volumeType :: Lens.Lens' ModifyVolume (Core.Maybe VolumeType)
modifyVolume_volumeType = Lens.lens (\ModifyVolume' {volumeType} -> volumeType) (\s@ModifyVolume' {} a -> s {volumeType = a} :: ModifyVolume)

-- | The target IOPS rate of the volume. This parameter is valid only for
-- @gp3@, @io1@, and @io2@ volumes.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- Default: If no IOPS value is specified, the existing value is retained.
modifyVolume_iops :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
modifyVolume_iops = Lens.lens (\ModifyVolume' {iops} -> iops) (\s@ModifyVolume' {} a -> s {iops = a} :: ModifyVolume)

-- | The target size of the volume, in GiB. The target volume size must be
-- greater than or equal to the existing size of the volume.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- Default: If no size is specified, the existing size is retained.
modifyVolume_size :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
modifyVolume_size = Lens.lens (\ModifyVolume' {size} -> size) (\s@ModifyVolume' {} a -> s {size = a} :: ModifyVolume)

-- | The ID of the volume.
modifyVolume_volumeId :: Lens.Lens' ModifyVolume Core.Text
modifyVolume_volumeId = Lens.lens (\ModifyVolume' {volumeId} -> volumeId) (\s@ModifyVolume' {} a -> s {volumeId = a} :: ModifyVolume)

instance Core.AWSRequest ModifyVolume where
  type AWSResponse ModifyVolume = ModifyVolumeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVolumeResponse'
            Core.<$> (x Core..@? "volumeModification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyVolume

instance Core.NFData ModifyVolume

instance Core.ToHeaders ModifyVolume where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyVolume where
  toPath = Core.const "/"

instance Core.ToQuery ModifyVolume where
  toQuery ModifyVolume' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyVolume" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "MultiAttachEnabled" Core.=: multiAttachEnabled,
        "DryRun" Core.=: dryRun,
        "Throughput" Core.=: throughput,
        "VolumeType" Core.=: volumeType,
        "Iops" Core.=: iops,
        "Size" Core.=: size,
        "VolumeId" Core.=: volumeId
      ]

-- | /See:/ 'newModifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { -- | Information about the volume modification.
    volumeModification :: Core.Maybe VolumeModification,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeModification', 'modifyVolumeResponse_volumeModification' - Information about the volume modification.
--
-- 'httpStatus', 'modifyVolumeResponse_httpStatus' - The response's http status code.
newModifyVolumeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyVolumeResponse
newModifyVolumeResponse pHttpStatus_ =
  ModifyVolumeResponse'
    { volumeModification =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the volume modification.
modifyVolumeResponse_volumeModification :: Lens.Lens' ModifyVolumeResponse (Core.Maybe VolumeModification)
modifyVolumeResponse_volumeModification = Lens.lens (\ModifyVolumeResponse' {volumeModification} -> volumeModification) (\s@ModifyVolumeResponse' {} a -> s {volumeModification = a} :: ModifyVolumeResponse)

-- | The response's http status code.
modifyVolumeResponse_httpStatus :: Lens.Lens' ModifyVolumeResponse Core.Int
modifyVolumeResponse_httpStatus = Lens.lens (\ModifyVolumeResponse' {httpStatus} -> httpStatus) (\s@ModifyVolumeResponse' {} a -> s {httpStatus = a} :: ModifyVolumeResponse)

instance Core.NFData ModifyVolumeResponse
