{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesEbs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesEbs where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EBS volume for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesEbs' smart constructor.
data ScheduledInstancesEbs = ScheduledInstancesEbs'
  { -- | Indicates whether the volume is encrypted. You can attached encrypted
    -- volumes only to instances that support them.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume type. @gp2@ for General Purpose SSD, @io1@ or @ io2@ for
    -- Provisioned IOPS SSD, Throughput Optimized HDD for @st1@, Cold HDD for
    -- @sc1@, or @standard@ for Magnetic.
    --
    -- Default: @gp2@
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume, in GiB.
    --
    -- Default: If you\'re creating the volume from a snapshot and don\'t
    -- specify a volume size, the default is the snapshot size.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The number of I\/O operations per second (IOPS) to provision for an
    -- @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS\/GiB for @io1@,
    -- and 500 IOPS\/GiB for @io2@. Range is 100 to 64,000 IOPS for volumes in
    -- most Regions. Maximum IOPS of 64,000 is guaranteed only on
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances instances built on the Nitro System>.
    -- Other instance families guarantee performance up to 32,000 IOPS. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon EC2 User Guide/.
    --
    -- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@)
    -- volumes.
    iops :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesEbs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'scheduledInstancesEbs_encrypted' - Indicates whether the volume is encrypted. You can attached encrypted
-- volumes only to instances that support them.
--
-- 'deleteOnTermination', 'scheduledInstancesEbs_deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- 'snapshotId', 'scheduledInstancesEbs_snapshotId' - The ID of the snapshot.
--
-- 'volumeType', 'scheduledInstancesEbs_volumeType' - The volume type. @gp2@ for General Purpose SSD, @io1@ or @ io2@ for
-- Provisioned IOPS SSD, Throughput Optimized HDD for @st1@, Cold HDD for
-- @sc1@, or @standard@ for Magnetic.
--
-- Default: @gp2@
--
-- 'volumeSize', 'scheduledInstancesEbs_volumeSize' - The size of the volume, in GiB.
--
-- Default: If you\'re creating the volume from a snapshot and don\'t
-- specify a volume size, the default is the snapshot size.
--
-- 'iops', 'scheduledInstancesEbs_iops' - The number of I\/O operations per second (IOPS) to provision for an
-- @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS\/GiB for @io1@,
-- and 500 IOPS\/GiB for @io2@. Range is 100 to 64,000 IOPS for volumes in
-- most Regions. Maximum IOPS of 64,000 is guaranteed only on
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide/.
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@)
-- volumes.
newScheduledInstancesEbs ::
  ScheduledInstancesEbs
newScheduledInstancesEbs =
  ScheduledInstancesEbs'
    { encrypted = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing
    }

-- | Indicates whether the volume is encrypted. You can attached encrypted
-- volumes only to instances that support them.
scheduledInstancesEbs_encrypted :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Bool)
scheduledInstancesEbs_encrypted = Lens.lens (\ScheduledInstancesEbs' {encrypted} -> encrypted) (\s@ScheduledInstancesEbs' {} a -> s {encrypted = a} :: ScheduledInstancesEbs)

-- | Indicates whether the volume is deleted on instance termination.
scheduledInstancesEbs_deleteOnTermination :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Bool)
scheduledInstancesEbs_deleteOnTermination = Lens.lens (\ScheduledInstancesEbs' {deleteOnTermination} -> deleteOnTermination) (\s@ScheduledInstancesEbs' {} a -> s {deleteOnTermination = a} :: ScheduledInstancesEbs)

-- | The ID of the snapshot.
scheduledInstancesEbs_snapshotId :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Text)
scheduledInstancesEbs_snapshotId = Lens.lens (\ScheduledInstancesEbs' {snapshotId} -> snapshotId) (\s@ScheduledInstancesEbs' {} a -> s {snapshotId = a} :: ScheduledInstancesEbs)

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ or @ io2@ for
-- Provisioned IOPS SSD, Throughput Optimized HDD for @st1@, Cold HDD for
-- @sc1@, or @standard@ for Magnetic.
--
-- Default: @gp2@
scheduledInstancesEbs_volumeType :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Text)
scheduledInstancesEbs_volumeType = Lens.lens (\ScheduledInstancesEbs' {volumeType} -> volumeType) (\s@ScheduledInstancesEbs' {} a -> s {volumeType = a} :: ScheduledInstancesEbs)

-- | The size of the volume, in GiB.
--
-- Default: If you\'re creating the volume from a snapshot and don\'t
-- specify a volume size, the default is the snapshot size.
scheduledInstancesEbs_volumeSize :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Int)
scheduledInstancesEbs_volumeSize = Lens.lens (\ScheduledInstancesEbs' {volumeSize} -> volumeSize) (\s@ScheduledInstancesEbs' {} a -> s {volumeSize = a} :: ScheduledInstancesEbs)

-- | The number of I\/O operations per second (IOPS) to provision for an
-- @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS\/GiB for @io1@,
-- and 500 IOPS\/GiB for @io2@. Range is 100 to 64,000 IOPS for volumes in
-- most Regions. Maximum IOPS of 64,000 is guaranteed only on
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide/.
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@)
-- volumes.
scheduledInstancesEbs_iops :: Lens.Lens' ScheduledInstancesEbs (Prelude.Maybe Prelude.Int)
scheduledInstancesEbs_iops = Lens.lens (\ScheduledInstancesEbs' {iops} -> iops) (\s@ScheduledInstancesEbs' {} a -> s {iops = a} :: ScheduledInstancesEbs)

instance Prelude.Hashable ScheduledInstancesEbs

instance Prelude.NFData ScheduledInstancesEbs

instance Prelude.ToQuery ScheduledInstancesEbs where
  toQuery ScheduledInstancesEbs' {..} =
    Prelude.mconcat
      [ "Encrypted" Prelude.=: encrypted,
        "DeleteOnTermination" Prelude.=: deleteOnTermination,
        "SnapshotId" Prelude.=: snapshotId,
        "VolumeType" Prelude.=: volumeType,
        "VolumeSize" Prelude.=: volumeSize,
        "Iops" Prelude.=: iops
      ]
