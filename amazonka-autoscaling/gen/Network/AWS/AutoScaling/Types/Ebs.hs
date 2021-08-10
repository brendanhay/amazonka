{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Ebs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Ebs where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information used to set up an Amazon EBS volume specified in a
-- block device mapping.
--
-- /See:/ 'newEbs' smart constructor.
data Ebs = Ebs'
  { -- | Specifies whether the volume should be encrypted. Encrypted EBS volumes
    -- can only be attached to instances that support Amazon EBS encryption.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types>.
    -- If your AMI uses encrypted volumes, you can also only launch it on
    -- supported instance types.
    --
    -- If you are creating a volume from a snapshot, you cannot specify an
    -- encryption value. Volumes that are created from encrypted snapshots are
    -- automatically encrypted, and volumes that are created from unencrypted
    -- snapshots are automatically unencrypted. By default, encrypted snapshots
    -- use the AWS managed CMK that is used for EBS encryption, but you can
    -- specify a custom CMK when you create the snapshot. The ability to
    -- encrypt a snapshot during copying also allows you to apply a new CMK to
    -- an already-encrypted snapshot. Volumes restored from the resulting copy
    -- are only accessible using the new CMK.
    --
    -- Enabling
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>
    -- results in all EBS volumes being encrypted with the AWS managed CMK or a
    -- customer managed CMK, whether or not the snapshot was encrypted.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs>
    -- in the /Amazon EC2 User Guide for Linux Instances/ and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the volume is deleted on instance termination. For
    -- Amazon EC2 Auto Scaling, the default value is @true@.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The snapshot ID of the volume to use.
    --
    -- You must specify either a @VolumeSize@ or a @SnapshotId@.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume type, which can be @standard@ for Magnetic, @io1@ for
    -- Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for
    -- Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The volume size, in Gibibytes (GiB).
    --
    -- This can be a number from 1-1,024 for @standard@, 4-16,384 for @io1@,
    -- 1-16,384 for @gp2@, and 500-16,384 for @st1@ and @sc1@. If you specify a
    -- snapshot, the volume size must be equal to or larger than the snapshot
    -- size.
    --
    -- Default: If you create a volume from a snapshot and you don\'t specify a
    -- volume size, the default is the snapshot size.
    --
    -- You must specify either a @VolumeSize@ or a @SnapshotId@. If you specify
    -- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
    -- greater than the size of the snapshot.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | The number of I\/O operations per second (IOPS) to provision for the
    -- volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Required when the volume type is @io1@. (Not used with @standard@,
    -- @gp2@, @st1@, or @sc1@ volumes.)
    iops :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ebs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'ebs_encrypted' - Specifies whether the volume should be encrypted. Encrypted EBS volumes
-- can only be attached to instances that support Amazon EBS encryption.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types>.
-- If your AMI uses encrypted volumes, you can also only launch it on
-- supported instance types.
--
-- If you are creating a volume from a snapshot, you cannot specify an
-- encryption value. Volumes that are created from encrypted snapshots are
-- automatically encrypted, and volumes that are created from unencrypted
-- snapshots are automatically unencrypted. By default, encrypted snapshots
-- use the AWS managed CMK that is used for EBS encryption, but you can
-- specify a custom CMK when you create the snapshot. The ability to
-- encrypt a snapshot during copying also allows you to apply a new CMK to
-- an already-encrypted snapshot. Volumes restored from the resulting copy
-- are only accessible using the new CMK.
--
-- Enabling
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>
-- results in all EBS volumes being encrypted with the AWS managed CMK or a
-- customer managed CMK, whether or not the snapshot was encrypted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'deleteOnTermination', 'ebs_deleteOnTermination' - Indicates whether the volume is deleted on instance termination. For
-- Amazon EC2 Auto Scaling, the default value is @true@.
--
-- 'snapshotId', 'ebs_snapshotId' - The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@.
--
-- 'volumeType', 'ebs_volumeType' - The volume type, which can be @standard@ for Magnetic, @io1@ for
-- Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for
-- Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
--
-- 'volumeSize', 'ebs_volumeSize' - The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@, 4-16,384 for @io1@,
-- 1-16,384 for @gp2@, and 500-16,384 for @st1@ and @sc1@. If you specify a
-- snapshot, the volume size must be equal to or larger than the snapshot
-- size.
--
-- Default: If you create a volume from a snapshot and you don\'t specify a
-- volume size, the default is the snapshot size.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
--
-- 'iops', 'ebs_iops' - The number of I\/O operations per second (IOPS) to provision for the
-- volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Required when the volume type is @io1@. (Not used with @standard@,
-- @gp2@, @st1@, or @sc1@ volumes.)
newEbs ::
  Ebs
newEbs =
  Ebs'
    { encrypted = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing
    }

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes
-- can only be attached to instances that support Amazon EBS encryption.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types>.
-- If your AMI uses encrypted volumes, you can also only launch it on
-- supported instance types.
--
-- If you are creating a volume from a snapshot, you cannot specify an
-- encryption value. Volumes that are created from encrypted snapshots are
-- automatically encrypted, and volumes that are created from unencrypted
-- snapshots are automatically unencrypted. By default, encrypted snapshots
-- use the AWS managed CMK that is used for EBS encryption, but you can
-- specify a custom CMK when you create the snapshot. The ability to
-- encrypt a snapshot during copying also allows you to apply a new CMK to
-- an already-encrypted snapshot. Volumes restored from the resulting copy
-- are only accessible using the new CMK.
--
-- Enabling
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>
-- results in all EBS volumes being encrypted with the AWS managed CMK or a
-- customer managed CMK, whether or not the snapshot was encrypted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
ebs_encrypted :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Bool)
ebs_encrypted = Lens.lens (\Ebs' {encrypted} -> encrypted) (\s@Ebs' {} a -> s {encrypted = a} :: Ebs)

-- | Indicates whether the volume is deleted on instance termination. For
-- Amazon EC2 Auto Scaling, the default value is @true@.
ebs_deleteOnTermination :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Bool)
ebs_deleteOnTermination = Lens.lens (\Ebs' {deleteOnTermination} -> deleteOnTermination) (\s@Ebs' {} a -> s {deleteOnTermination = a} :: Ebs)

-- | The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@.
ebs_snapshotId :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Text)
ebs_snapshotId = Lens.lens (\Ebs' {snapshotId} -> snapshotId) (\s@Ebs' {} a -> s {snapshotId = a} :: Ebs)

-- | The volume type, which can be @standard@ for Magnetic, @io1@ for
-- Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for
-- Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
ebs_volumeType :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Text)
ebs_volumeType = Lens.lens (\Ebs' {volumeType} -> volumeType) (\s@Ebs' {} a -> s {volumeType = a} :: Ebs)

-- | The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@, 4-16,384 for @io1@,
-- 1-16,384 for @gp2@, and 500-16,384 for @st1@ and @sc1@. If you specify a
-- snapshot, the volume size must be equal to or larger than the snapshot
-- size.
--
-- Default: If you create a volume from a snapshot and you don\'t specify a
-- volume size, the default is the snapshot size.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
ebs_volumeSize :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Natural)
ebs_volumeSize = Lens.lens (\Ebs' {volumeSize} -> volumeSize) (\s@Ebs' {} a -> s {volumeSize = a} :: Ebs)

-- | The number of I\/O operations per second (IOPS) to provision for the
-- volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Required when the volume type is @io1@. (Not used with @standard@,
-- @gp2@, @st1@, or @sc1@ volumes.)
ebs_iops :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Natural)
ebs_iops = Lens.lens (\Ebs' {iops} -> iops) (\s@Ebs' {} a -> s {iops = a} :: Ebs)

instance Core.FromXML Ebs where
  parseXML x =
    Ebs'
      Prelude.<$> (x Core..@? "Encrypted")
      Prelude.<*> (x Core..@? "DeleteOnTermination")
      Prelude.<*> (x Core..@? "SnapshotId")
      Prelude.<*> (x Core..@? "VolumeType")
      Prelude.<*> (x Core..@? "VolumeSize")
      Prelude.<*> (x Core..@? "Iops")

instance Prelude.Hashable Ebs

instance Prelude.NFData Ebs

instance Core.ToQuery Ebs where
  toQuery Ebs' {..} =
    Prelude.mconcat
      [ "Encrypted" Core.=: encrypted,
        "DeleteOnTermination" Core.=: deleteOnTermination,
        "SnapshotId" Core.=: snapshotId,
        "VolumeType" Core.=: volumeType,
        "VolumeSize" Core.=: volumeSize,
        "Iops" Core.=: iops
      ]
