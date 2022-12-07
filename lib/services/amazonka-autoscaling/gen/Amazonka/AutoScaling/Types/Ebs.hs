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
-- Module      : Amazonka.AutoScaling.Types.Ebs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Ebs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes information used to set up an Amazon EBS volume specified in a
-- block device mapping.
--
-- /See:/ 'newEbs' smart constructor.
data Ebs = Ebs'
  { -- | Indicates whether the volume is deleted on instance termination. For
    -- Amazon EC2 Auto Scaling, the default value is @true@.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The snapshot ID of the volume to use.
    --
    -- You must specify either a @VolumeSize@ or a @SnapshotId@.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Valid values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@ | @gp3@
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The volume size, in GiBs. The following are the supported volumes sizes
    -- for each volume type:
    --
    -- -   @gp2@ and @gp3@: 1-16,384
    --
    -- -   @io1@: 4-16,384
    --
    -- -   @st1@ and @sc1@: 125-16,384
    --
    -- -   @standard@: 1-1,024
    --
    -- You must specify either a @SnapshotId@ or a @VolumeSize@. If you specify
    -- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
    -- greater than the size of the snapshot.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the volume should be encrypted. Encrypted EBS volumes
    -- can only be attached to instances that support Amazon EBS encryption.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
    -- If your AMI uses encrypted volumes, you can also only launch it on
    -- supported instance types.
    --
    -- If you are creating a volume from a snapshot, you cannot create an
    -- unencrypted volume from an encrypted snapshot. Also, you cannot specify
    -- a KMS key ID when using a launch configuration.
    --
    -- If you enable encryption by default, the EBS volumes that you create are
    -- always encrypted, either using the Amazon Web Services managed KMS key
    -- or a customer-managed KMS key, regardless of whether the snapshot was
    -- encrypted.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-data-protection.html#encryption Use Amazon Web Services KMS keys to encrypt Amazon EBS volumes>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The throughput (MiBps) to provision for a @gp3@ volume.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | The number of input\/output (I\/O) operations per second (IOPS) to
    -- provision for the volume. For @gp3@ and @io1@ volumes, this represents
    -- the number of IOPS that are provisioned for the volume. For @gp2@
    -- volumes, this represents the baseline performance of the volume and the
    -- rate at which the volume accumulates I\/O credits for bursting.
    --
    -- The following are the supported values for each volume type:
    --
    -- -   @gp3@: 3,000-16,000 IOPS
    --
    -- -   @io1@: 100-64,000 IOPS
    --
    -- For @io1@ volumes, we guarantee 64,000 IOPS only for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
    -- Other instance families guarantee performance up to 32,000 IOPS.
    --
    -- @Iops@ is supported when the volume type is @gp3@ or @io1@ and required
    -- only when the volume type is @io1@. (Not used with @standard@, @gp2@,
    -- @st1@, or @sc1@ volumes.)
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
-- 'deleteOnTermination', 'ebs_deleteOnTermination' - Indicates whether the volume is deleted on instance termination. For
-- Amazon EC2 Auto Scaling, the default value is @true@.
--
-- 'snapshotId', 'ebs_snapshotId' - The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@.
--
-- 'volumeType', 'ebs_volumeType' - The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Valid values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@ | @gp3@
--
-- 'volumeSize', 'ebs_volumeSize' - The volume size, in GiBs. The following are the supported volumes sizes
-- for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- You must specify either a @SnapshotId@ or a @VolumeSize@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
--
-- 'encrypted', 'ebs_encrypted' - Specifies whether the volume should be encrypted. Encrypted EBS volumes
-- can only be attached to instances that support Amazon EBS encryption.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
-- If your AMI uses encrypted volumes, you can also only launch it on
-- supported instance types.
--
-- If you are creating a volume from a snapshot, you cannot create an
-- unencrypted volume from an encrypted snapshot. Also, you cannot specify
-- a KMS key ID when using a launch configuration.
--
-- If you enable encryption by default, the EBS volumes that you create are
-- always encrypted, either using the Amazon Web Services managed KMS key
-- or a customer-managed KMS key, regardless of whether the snapshot was
-- encrypted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-data-protection.html#encryption Use Amazon Web Services KMS keys to encrypt Amazon EBS volumes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'throughput', 'ebs_throughput' - The throughput (MiBps) to provision for a @gp3@ volume.
--
-- 'iops', 'ebs_iops' - The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume. For @gp3@ and @io1@ volumes, this represents
-- the number of IOPS that are provisioned for the volume. For @gp2@
-- volumes, this represents the baseline performance of the volume and the
-- rate at which the volume accumulates I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- For @io1@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- @Iops@ is supported when the volume type is @gp3@ or @io1@ and required
-- only when the volume type is @io1@. (Not used with @standard@, @gp2@,
-- @st1@, or @sc1@ volumes.)
newEbs ::
  Ebs
newEbs =
  Ebs'
    { deleteOnTermination = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      throughput = Prelude.Nothing,
      iops = Prelude.Nothing
    }

-- | Indicates whether the volume is deleted on instance termination. For
-- Amazon EC2 Auto Scaling, the default value is @true@.
ebs_deleteOnTermination :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Bool)
ebs_deleteOnTermination = Lens.lens (\Ebs' {deleteOnTermination} -> deleteOnTermination) (\s@Ebs' {} a -> s {deleteOnTermination = a} :: Ebs)

-- | The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@.
ebs_snapshotId :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Text)
ebs_snapshotId = Lens.lens (\Ebs' {snapshotId} -> snapshotId) (\s@Ebs' {} a -> s {snapshotId = a} :: Ebs)

-- | The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Valid values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@ | @gp3@
ebs_volumeType :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Text)
ebs_volumeType = Lens.lens (\Ebs' {volumeType} -> volumeType) (\s@Ebs' {} a -> s {volumeType = a} :: Ebs)

-- | The volume size, in GiBs. The following are the supported volumes sizes
-- for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- You must specify either a @SnapshotId@ or a @VolumeSize@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
ebs_volumeSize :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Natural)
ebs_volumeSize = Lens.lens (\Ebs' {volumeSize} -> volumeSize) (\s@Ebs' {} a -> s {volumeSize = a} :: Ebs)

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes
-- can only be attached to instances that support Amazon EBS encryption.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
-- If your AMI uses encrypted volumes, you can also only launch it on
-- supported instance types.
--
-- If you are creating a volume from a snapshot, you cannot create an
-- unencrypted volume from an encrypted snapshot. Also, you cannot specify
-- a KMS key ID when using a launch configuration.
--
-- If you enable encryption by default, the EBS volumes that you create are
-- always encrypted, either using the Amazon Web Services managed KMS key
-- or a customer-managed KMS key, regardless of whether the snapshot was
-- encrypted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-data-protection.html#encryption Use Amazon Web Services KMS keys to encrypt Amazon EBS volumes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
ebs_encrypted :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Bool)
ebs_encrypted = Lens.lens (\Ebs' {encrypted} -> encrypted) (\s@Ebs' {} a -> s {encrypted = a} :: Ebs)

-- | The throughput (MiBps) to provision for a @gp3@ volume.
ebs_throughput :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Natural)
ebs_throughput = Lens.lens (\Ebs' {throughput} -> throughput) (\s@Ebs' {} a -> s {throughput = a} :: Ebs)

-- | The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume. For @gp3@ and @io1@ volumes, this represents
-- the number of IOPS that are provisioned for the volume. For @gp2@
-- volumes, this represents the baseline performance of the volume and the
-- rate at which the volume accumulates I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- For @io1@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- @Iops@ is supported when the volume type is @gp3@ or @io1@ and required
-- only when the volume type is @io1@. (Not used with @standard@, @gp2@,
-- @st1@, or @sc1@ volumes.)
ebs_iops :: Lens.Lens' Ebs (Prelude.Maybe Prelude.Natural)
ebs_iops = Lens.lens (\Ebs' {iops} -> iops) (\s@Ebs' {} a -> s {iops = a} :: Ebs)

instance Data.FromXML Ebs where
  parseXML x =
    Ebs'
      Prelude.<$> (x Data..@? "DeleteOnTermination")
      Prelude.<*> (x Data..@? "SnapshotId")
      Prelude.<*> (x Data..@? "VolumeType")
      Prelude.<*> (x Data..@? "VolumeSize")
      Prelude.<*> (x Data..@? "Encrypted")
      Prelude.<*> (x Data..@? "Throughput")
      Prelude.<*> (x Data..@? "Iops")

instance Prelude.Hashable Ebs where
  hashWithSalt _salt Ebs' {..} =
    _salt `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` iops

instance Prelude.NFData Ebs where
  rnf Ebs' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf iops

instance Data.ToQuery Ebs where
  toQuery Ebs' {..} =
    Prelude.mconcat
      [ "DeleteOnTermination" Data.=: deleteOnTermination,
        "SnapshotId" Data.=: snapshotId,
        "VolumeType" Data.=: volumeType,
        "VolumeSize" Data.=: volumeSize,
        "Encrypted" Data.=: encrypted,
        "Throughput" Data.=: throughput,
        "Iops" Data.=: iops
      ]
