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
-- Module      : Network.AWS.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters that are used to automatically set up EBS volumes when an
-- instance is launched.
--
-- /See:/ 'newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' smart constructor.
data AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails = AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
  { -- | Whether to delete the volume when the instance is terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The volume size, in GiBs. The following are the supported volumes sizes
    -- for each volume type:
    --
    -- -   gp2 and gp3: 1-16,384
    --
    -- -   io1: 4-16,384
    --
    -- -   st1 and sc1: 125-16,384
    --
    -- -   standard: 1-1,024
    --
    -- You must specify either @SnapshotId@ or @VolumeSize@. If you specify
    -- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
    -- greater than the size of the snapshot.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The number of input\/output (I\/O) operations per second (IOPS) to
    -- provision for the volume.
    --
    -- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
    -- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Whether to encrypt the volume.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The volume type.
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The snapshot ID of the volume to use.
    --
    -- You must specify either @VolumeSize@ or @SnapshotId@.
    snapshotId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination' - Whether to delete the volume when the instance is terminated.
--
-- 'volumeSize', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize' - The volume size, in GiBs. The following are the supported volumes sizes
-- for each volume type:
--
-- -   gp2 and gp3: 1-16,384
--
-- -   io1: 4-16,384
--
-- -   st1 and sc1: 125-16,384
--
-- -   standard: 1-1,024
--
-- You must specify either @SnapshotId@ or @VolumeSize@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
--
-- 'iops', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops' - The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume.
--
-- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
-- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
--
-- 'encrypted', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted' - Whether to encrypt the volume.
--
-- 'volumeType', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType' - The volume type.
--
-- 'snapshotId', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId' - The snapshot ID of the volume to use.
--
-- You must specify either @VolumeSize@ or @SnapshotId@.
newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails ::
  AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails =
  AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
    { deleteOnTermination =
        Prelude.Nothing,
      volumeSize =
        Prelude.Nothing,
      iops =
        Prelude.Nothing,
      encrypted =
        Prelude.Nothing,
      volumeType =
        Prelude.Nothing,
      snapshotId =
        Prelude.Nothing
    }

-- | Whether to delete the volume when the instance is terminated.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {deleteOnTermination} -> deleteOnTermination) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {deleteOnTermination = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The volume size, in GiBs. The following are the supported volumes sizes
-- for each volume type:
--
-- -   gp2 and gp3: 1-16,384
--
-- -   io1: 4-16,384
--
-- -   st1 and sc1: 125-16,384
--
-- -   standard: 1-1,024
--
-- You must specify either @SnapshotId@ or @VolumeSize@. If you specify
-- both @SnapshotId@ and @VolumeSize@, the volume size must be equal or
-- greater than the size of the snapshot.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {volumeSize} -> volumeSize) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {volumeSize = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume.
--
-- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
-- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {iops} -> iops) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {iops = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | Whether to encrypt the volume.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {encrypted} -> encrypted) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {encrypted = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The volume type.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {volumeType} -> volumeType) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {volumeType = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The snapshot ID of the volume to use.
--
-- You must specify either @VolumeSize@ or @SnapshotId@.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {snapshotId} -> snapshotId) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {snapshotId = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

instance
  Core.FromJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  parseJSON =
    Core.withObject
      "AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails"
      ( \x ->
          AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
            Prelude.<$> (x Core..:? "DeleteOnTermination")
              Prelude.<*> (x Core..:? "VolumeSize")
              Prelude.<*> (x Core..:? "Iops")
              Prelude.<*> (x Core..:? "Encrypted")
              Prelude.<*> (x Core..:? "VolumeType")
              Prelude.<*> (x Core..:? "SnapshotId")
      )

instance
  Prelude.Hashable
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails

instance
  Prelude.NFData
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails

instance
  Core.ToJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  toJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("DeleteOnTermination" Core..=)
                Prelude.<$> deleteOnTermination,
              ("VolumeSize" Core..=) Prelude.<$> volumeSize,
              ("Iops" Core..=) Prelude.<$> iops,
              ("Encrypted" Core..=) Prelude.<$> encrypted,
              ("VolumeType" Core..=) Prelude.<$> volumeType,
              ("SnapshotId" Core..=) Prelude.<$> snapshotId
            ]
        )
