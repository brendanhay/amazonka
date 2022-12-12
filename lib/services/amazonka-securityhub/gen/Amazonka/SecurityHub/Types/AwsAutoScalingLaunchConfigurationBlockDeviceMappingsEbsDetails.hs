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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are used to automatically set up EBS volumes when an
-- instance is launched.
--
-- /See:/ 'newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' smart constructor.
data AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails = AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
  { -- | Whether to delete the volume when the instance is terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Whether to encrypt the volume.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of input\/output (I\/O) operations per second (IOPS) to
    -- provision for the volume.
    --
    -- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
    -- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The snapshot ID of the volume to use.
    --
    -- You must specify either @VolumeSize@ or @SnapshotId@.
    snapshotId :: Prelude.Maybe Prelude.Text,
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
    -- | The volume type. Valid values are as follows:
    --
    -- -   @gp2@
    --
    -- -   @gp3@
    --
    -- -   @io1@
    --
    -- -   @sc1@
    --
    -- -   @st1@
    --
    -- -   @standard@
    volumeType :: Prelude.Maybe Prelude.Text
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
-- 'encrypted', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted' - Whether to encrypt the volume.
--
-- 'iops', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops' - The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume.
--
-- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
-- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
--
-- 'snapshotId', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId' - The snapshot ID of the volume to use.
--
-- You must specify either @VolumeSize@ or @SnapshotId@.
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
-- 'volumeType', 'awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType' - The volume type. Valid values are as follows:
--
-- -   @gp2@
--
-- -   @gp3@
--
-- -   @io1@
--
-- -   @sc1@
--
-- -   @st1@
--
-- -   @standard@
newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails ::
  AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails =
  AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
    { deleteOnTermination =
        Prelude.Nothing,
      encrypted =
        Prelude.Nothing,
      iops =
        Prelude.Nothing,
      snapshotId =
        Prelude.Nothing,
      volumeSize =
        Prelude.Nothing,
      volumeType =
        Prelude.Nothing
    }

-- | Whether to delete the volume when the instance is terminated.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {deleteOnTermination} -> deleteOnTermination) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {deleteOnTermination = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | Whether to encrypt the volume.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {encrypted} -> encrypted) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {encrypted = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The number of input\/output (I\/O) operations per second (IOPS) to
-- provision for the volume.
--
-- Only supported for @gp3@ or @io1@ volumes. Required for @io1@ volumes.
-- Not used with @standard@, @gp2@, @st1@, or @sc1@ volumes.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {iops} -> iops) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {iops = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

-- | The snapshot ID of the volume to use.
--
-- You must specify either @VolumeSize@ or @SnapshotId@.
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {snapshotId} -> snapshotId) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {snapshotId = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

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

-- | The volume type. Valid values are as follows:
--
-- -   @gp2@
--
-- -   @gp3@
--
-- -   @io1@
--
-- -   @sc1@
--
-- -   @st1@
--
-- -   @standard@
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType :: Lens.Lens' AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType = Lens.lens (\AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {volumeType} -> volumeType) (\s@AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {} a -> s {volumeType = a} :: AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails)

instance
  Data.FromJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails"
      ( \x ->
          AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'
            Prelude.<$> (x Data..:? "DeleteOnTermination")
              Prelude.<*> (x Data..:? "Encrypted")
              Prelude.<*> (x Data..:? "Iops")
              Prelude.<*> (x Data..:? "SnapshotId")
              Prelude.<*> (x Data..:? "VolumeSize")
              Prelude.<*> (x Data..:? "VolumeType")
      )

instance
  Prelude.Hashable
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {..} =
      _salt `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` encrypted
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` volumeSize
        `Prelude.hashWithSalt` volumeType

instance
  Prelude.NFData
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  rnf
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {..} =
      Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf encrypted
        `Prelude.seq` Prelude.rnf iops
        `Prelude.seq` Prelude.rnf snapshotId
        `Prelude.seq` Prelude.rnf volumeSize
        `Prelude.seq` Prelude.rnf volumeType

instance
  Data.ToJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
  where
  toJSON
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeleteOnTermination" Data..=)
                Prelude.<$> deleteOnTermination,
              ("Encrypted" Data..=) Prelude.<$> encrypted,
              ("Iops" Data..=) Prelude.<$> iops,
              ("SnapshotId" Data..=) Prelude.<$> snapshotId,
              ("VolumeSize" Data..=) Prelude.<$> volumeSize,
              ("VolumeType" Data..=) Prelude.<$> volumeType
            ]
        )
