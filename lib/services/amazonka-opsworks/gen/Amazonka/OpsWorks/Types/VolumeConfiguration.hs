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
-- Module      : Amazonka.OpsWorks.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.VolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'newVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | For PIOPS volumes, the IOPS per disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The volume
    -- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether an Amazon EBS volume is encrypted. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The volume type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
    --
    -- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
    --     1 GiB and a maximum size of 1024 GiB.
    --
    -- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
    --     size of 4 GiB and a maximum size of 16384 GiB.
    --
    -- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
    --     minimum size of 1 GiB and a maximum size of 16384 GiB.
    --
    -- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
    --     optimized HDD volumes must have a minimum size of 500 GiB and a
    --     maximum size of 16384 GiB.
    --
    -- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
    --     GiB and a maximum size of 16384 GiB.
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The volume mount point. For example \"\/dev\/sdh\".
    mountPoint :: Prelude.Text,
    -- | The number of disks in the volume.
    numberOfDisks :: Prelude.Int,
    -- | The volume size.
    size :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'volumeConfiguration_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'raidLevel', 'volumeConfiguration_raidLevel' - The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'encrypted', 'volumeConfiguration_encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
--
-- 'volumeType', 'volumeConfiguration_volumeType' - The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
--
-- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
--     1 GiB and a maximum size of 1024 GiB.
--
-- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
--     size of 4 GiB and a maximum size of 16384 GiB.
--
-- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
--     minimum size of 1 GiB and a maximum size of 16384 GiB.
--
-- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
--     optimized HDD volumes must have a minimum size of 500 GiB and a
--     maximum size of 16384 GiB.
--
-- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
--     GiB and a maximum size of 16384 GiB.
--
-- 'mountPoint', 'volumeConfiguration_mountPoint' - The volume mount point. For example \"\/dev\/sdh\".
--
-- 'numberOfDisks', 'volumeConfiguration_numberOfDisks' - The number of disks in the volume.
--
-- 'size', 'volumeConfiguration_size' - The volume size.
newVolumeConfiguration ::
  -- | 'mountPoint'
  Prelude.Text ->
  -- | 'numberOfDisks'
  Prelude.Int ->
  -- | 'size'
  Prelude.Int ->
  VolumeConfiguration
newVolumeConfiguration
  pMountPoint_
  pNumberOfDisks_
  pSize_ =
    VolumeConfiguration'
      { iops = Prelude.Nothing,
        raidLevel = Prelude.Nothing,
        encrypted = Prelude.Nothing,
        volumeType = Prelude.Nothing,
        mountPoint = pMountPoint_,
        numberOfDisks = pNumberOfDisks_,
        size = pSize_
      }

-- | For PIOPS volumes, the IOPS per disk.
volumeConfiguration_iops :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_iops = Lens.lens (\VolumeConfiguration' {iops} -> iops) (\s@VolumeConfiguration' {} a -> s {iops = a} :: VolumeConfiguration)

-- | The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
volumeConfiguration_raidLevel :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_raidLevel = Lens.lens (\VolumeConfiguration' {raidLevel} -> raidLevel) (\s@VolumeConfiguration' {} a -> s {raidLevel = a} :: VolumeConfiguration)

-- | Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
volumeConfiguration_encrypted :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Bool)
volumeConfiguration_encrypted = Lens.lens (\VolumeConfiguration' {encrypted} -> encrypted) (\s@VolumeConfiguration' {} a -> s {encrypted = a} :: VolumeConfiguration)

-- | The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
--
-- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
--     1 GiB and a maximum size of 1024 GiB.
--
-- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
--     size of 4 GiB and a maximum size of 16384 GiB.
--
-- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
--     minimum size of 1 GiB and a maximum size of 16384 GiB.
--
-- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
--     optimized HDD volumes must have a minimum size of 500 GiB and a
--     maximum size of 16384 GiB.
--
-- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
--     GiB and a maximum size of 16384 GiB.
volumeConfiguration_volumeType :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Text)
volumeConfiguration_volumeType = Lens.lens (\VolumeConfiguration' {volumeType} -> volumeType) (\s@VolumeConfiguration' {} a -> s {volumeType = a} :: VolumeConfiguration)

-- | The volume mount point. For example \"\/dev\/sdh\".
volumeConfiguration_mountPoint :: Lens.Lens' VolumeConfiguration Prelude.Text
volumeConfiguration_mountPoint = Lens.lens (\VolumeConfiguration' {mountPoint} -> mountPoint) (\s@VolumeConfiguration' {} a -> s {mountPoint = a} :: VolumeConfiguration)

-- | The number of disks in the volume.
volumeConfiguration_numberOfDisks :: Lens.Lens' VolumeConfiguration Prelude.Int
volumeConfiguration_numberOfDisks = Lens.lens (\VolumeConfiguration' {numberOfDisks} -> numberOfDisks) (\s@VolumeConfiguration' {} a -> s {numberOfDisks = a} :: VolumeConfiguration)

-- | The volume size.
volumeConfiguration_size :: Lens.Lens' VolumeConfiguration Prelude.Int
volumeConfiguration_size = Lens.lens (\VolumeConfiguration' {size} -> size) (\s@VolumeConfiguration' {} a -> s {size = a} :: VolumeConfiguration)

instance Core.FromJSON VolumeConfiguration where
  parseJSON =
    Core.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Prelude.<$> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "RaidLevel")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "VolumeType")
            Prelude.<*> (x Core..: "MountPoint")
            Prelude.<*> (x Core..: "NumberOfDisks")
            Prelude.<*> (x Core..: "Size")
      )

instance Prelude.Hashable VolumeConfiguration where
  hashWithSalt salt' VolumeConfiguration' {..} =
    salt' `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` numberOfDisks
      `Prelude.hashWithSalt` mountPoint
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` raidLevel
      `Prelude.hashWithSalt` iops

instance Prelude.NFData VolumeConfiguration where
  rnf VolumeConfiguration' {..} =
    Prelude.rnf iops `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf numberOfDisks
      `Prelude.seq` Prelude.rnf mountPoint
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf raidLevel

instance Core.ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Iops" Core..=) Prelude.<$> iops,
            ("RaidLevel" Core..=) Prelude.<$> raidLevel,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("VolumeType" Core..=) Prelude.<$> volumeType,
            Prelude.Just ("MountPoint" Core..= mountPoint),
            Prelude.Just ("NumberOfDisks" Core..= numberOfDisks),
            Prelude.Just ("Size" Core..= size)
          ]
      )
