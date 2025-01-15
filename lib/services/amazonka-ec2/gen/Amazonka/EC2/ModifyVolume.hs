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
-- Module      : Amazonka.EC2.ModifyVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can modify several parameters of an existing EBS volume, including
-- volume size, volume type, and IOPS capacity. If your EBS volume is
-- attached to a current-generation EC2 instance type, you might be able to
-- apply these changes without stopping the instance or detaching the
-- volume from it. For more information about modifying EBS volumes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes>
-- (Linux instances) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes>
-- (Windows instances).
--
-- When you complete a resize operation on your volume, you need to extend
-- the volume\'s file-system size to take advantage of the new storage
-- capacity. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux Extend a Linux file system>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows Extend a Windows file system>.
--
-- You can use CloudWatch Events to check the status of a modification to
-- an EBS volume. For information about CloudWatch Events, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide>.
-- You can also track the status of a modification using
-- DescribeVolumesModifications. For information about tracking status
-- changes using either method, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-modifications.html Monitor the progress of volume modifications>.
--
-- With previous-generation instance types, resizing an EBS volume might
-- require detaching and reattaching the volume or stopping and restarting
-- the instance.
--
-- After modifying a volume, you must wait at least six hours and ensure
-- that the volume is in the @in-use@ or @available@ state before you can
-- modify the same volume. This is sometimes referred to as a cooldown
-- period.
module Amazonka.EC2.ModifyVolume
  ( -- * Creating a Request
    ModifyVolume (..),
    newModifyVolume,

    -- * Request Lenses
    modifyVolume_dryRun,
    modifyVolume_iops,
    modifyVolume_multiAttachEnabled,
    modifyVolume_size,
    modifyVolume_throughput,
    modifyVolume_volumeType,
    modifyVolume_volumeId,

    -- * Destructuring the Response
    ModifyVolumeResponse (..),
    newModifyVolumeResponse,

    -- * Response Lenses
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- Default: The existing value is retained if you keep the same volume
    -- type. If you change the volume type to @io1@, @io2@, or @gp3@, the
    -- default is 3,000.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable
    -- Multi-Attach, you can attach the volume to up to 16
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
    -- in the same Availability Zone. This parameter is supported with @io1@
    -- and @io2@ volumes only. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    multiAttachEnabled :: Prelude.Maybe Prelude.Bool,
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
    -- Default: The existing size is retained.
    size :: Prelude.Maybe Prelude.Int,
    -- | The target throughput of the volume, in MiB\/s. This parameter is valid
    -- only for @gp3@ volumes. The maximum value is 1,000.
    --
    -- Default: The existing value is retained if the source and target volume
    -- type is @gp3@. Otherwise, the default value is 125.
    --
    -- Valid Range: Minimum value of 125. Maximum value of 1000.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The target EBS volume type of the volume. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- Default: The existing type is retained.
    volumeType :: Prelude.Maybe VolumeType,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- Default: The existing value is retained if you keep the same volume
-- type. If you change the volume type to @io1@, @io2@, or @gp3@, the
-- default is 3,000.
--
-- 'multiAttachEnabled', 'modifyVolume_multiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
-- Default: The existing size is retained.
--
-- 'throughput', 'modifyVolume_throughput' - The target throughput of the volume, in MiB\/s. This parameter is valid
-- only for @gp3@ volumes. The maximum value is 1,000.
--
-- Default: The existing value is retained if the source and target volume
-- type is @gp3@. Otherwise, the default value is 125.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- 'volumeType', 'modifyVolume_volumeType' - The target EBS volume type of the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: The existing type is retained.
--
-- 'volumeId', 'modifyVolume_volumeId' - The ID of the volume.
newModifyVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  ModifyVolume
newModifyVolume pVolumeId_ =
  ModifyVolume'
    { dryRun = Prelude.Nothing,
      iops = Prelude.Nothing,
      multiAttachEnabled = Prelude.Nothing,
      size = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVolume_dryRun :: Lens.Lens' ModifyVolume (Prelude.Maybe Prelude.Bool)
modifyVolume_dryRun = Lens.lens (\ModifyVolume' {dryRun} -> dryRun) (\s@ModifyVolume' {} a -> s {dryRun = a} :: ModifyVolume)

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
-- Default: The existing value is retained if you keep the same volume
-- type. If you change the volume type to @io1@, @io2@, or @gp3@, the
-- default is 3,000.
modifyVolume_iops :: Lens.Lens' ModifyVolume (Prelude.Maybe Prelude.Int)
modifyVolume_iops = Lens.lens (\ModifyVolume' {iops} -> iops) (\s@ModifyVolume' {} a -> s {iops = a} :: ModifyVolume)

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
modifyVolume_multiAttachEnabled :: Lens.Lens' ModifyVolume (Prelude.Maybe Prelude.Bool)
modifyVolume_multiAttachEnabled = Lens.lens (\ModifyVolume' {multiAttachEnabled} -> multiAttachEnabled) (\s@ModifyVolume' {} a -> s {multiAttachEnabled = a} :: ModifyVolume)

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
-- Default: The existing size is retained.
modifyVolume_size :: Lens.Lens' ModifyVolume (Prelude.Maybe Prelude.Int)
modifyVolume_size = Lens.lens (\ModifyVolume' {size} -> size) (\s@ModifyVolume' {} a -> s {size = a} :: ModifyVolume)

-- | The target throughput of the volume, in MiB\/s. This parameter is valid
-- only for @gp3@ volumes. The maximum value is 1,000.
--
-- Default: The existing value is retained if the source and target volume
-- type is @gp3@. Otherwise, the default value is 125.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
modifyVolume_throughput :: Lens.Lens' ModifyVolume (Prelude.Maybe Prelude.Int)
modifyVolume_throughput = Lens.lens (\ModifyVolume' {throughput} -> throughput) (\s@ModifyVolume' {} a -> s {throughput = a} :: ModifyVolume)

-- | The target EBS volume type of the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: The existing type is retained.
modifyVolume_volumeType :: Lens.Lens' ModifyVolume (Prelude.Maybe VolumeType)
modifyVolume_volumeType = Lens.lens (\ModifyVolume' {volumeType} -> volumeType) (\s@ModifyVolume' {} a -> s {volumeType = a} :: ModifyVolume)

-- | The ID of the volume.
modifyVolume_volumeId :: Lens.Lens' ModifyVolume Prelude.Text
modifyVolume_volumeId = Lens.lens (\ModifyVolume' {volumeId} -> volumeId) (\s@ModifyVolume' {} a -> s {volumeId = a} :: ModifyVolume)

instance Core.AWSRequest ModifyVolume where
  type AWSResponse ModifyVolume = ModifyVolumeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVolumeResponse'
            Prelude.<$> (x Data..@? "volumeModification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVolume where
  hashWithSalt _salt ModifyVolume' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` multiAttachEnabled
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData ModifyVolume where
  rnf ModifyVolume' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf iops `Prelude.seq`
        Prelude.rnf multiAttachEnabled `Prelude.seq`
          Prelude.rnf size `Prelude.seq`
            Prelude.rnf throughput `Prelude.seq`
              Prelude.rnf volumeType `Prelude.seq`
                Prelude.rnf volumeId

instance Data.ToHeaders ModifyVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVolume where
  toQuery ModifyVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVolume" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Iops" Data.=: iops,
        "MultiAttachEnabled" Data.=: multiAttachEnabled,
        "Size" Data.=: size,
        "Throughput" Data.=: throughput,
        "VolumeType" Data.=: volumeType,
        "VolumeId" Data.=: volumeId
      ]

-- | /See:/ 'newModifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { -- | Information about the volume modification.
    volumeModification :: Prelude.Maybe VolumeModification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyVolumeResponse
newModifyVolumeResponse pHttpStatus_ =
  ModifyVolumeResponse'
    { volumeModification =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the volume modification.
modifyVolumeResponse_volumeModification :: Lens.Lens' ModifyVolumeResponse (Prelude.Maybe VolumeModification)
modifyVolumeResponse_volumeModification = Lens.lens (\ModifyVolumeResponse' {volumeModification} -> volumeModification) (\s@ModifyVolumeResponse' {} a -> s {volumeModification = a} :: ModifyVolumeResponse)

-- | The response's http status code.
modifyVolumeResponse_httpStatus :: Lens.Lens' ModifyVolumeResponse Prelude.Int
modifyVolumeResponse_httpStatus = Lens.lens (\ModifyVolumeResponse' {httpStatus} -> httpStatus) (\s@ModifyVolumeResponse' {} a -> s {httpStatus = a} :: ModifyVolumeResponse)

instance Prelude.NFData ModifyVolumeResponse where
  rnf ModifyVolumeResponse' {..} =
    Prelude.rnf volumeModification `Prelude.seq`
      Prelude.rnf httpStatus
