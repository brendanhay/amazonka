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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.VolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'newVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | The volume type. For more information, see
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
    -- | Specifies whether an Amazon EBS volume is encrypted. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The volume
    -- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Prelude.Maybe Prelude.Int,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Prelude.Maybe Prelude.Int,
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
-- 'encrypted', 'volumeConfiguration_encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
--
-- 'raidLevel', 'volumeConfiguration_raidLevel' - The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'iops', 'volumeConfiguration_iops' - For PIOPS volumes, the IOPS per disk.
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
      { volumeType = Prelude.Nothing,
        encrypted = Prelude.Nothing,
        raidLevel = Prelude.Nothing,
        iops = Prelude.Nothing,
        mountPoint = pMountPoint_,
        numberOfDisks = pNumberOfDisks_,
        size = pSize_
      }

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

-- | Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
volumeConfiguration_encrypted :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Bool)
volumeConfiguration_encrypted = Lens.lens (\VolumeConfiguration' {encrypted} -> encrypted) (\s@VolumeConfiguration' {} a -> s {encrypted = a} :: VolumeConfiguration)

-- | The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
volumeConfiguration_raidLevel :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_raidLevel = Lens.lens (\VolumeConfiguration' {raidLevel} -> raidLevel) (\s@VolumeConfiguration' {} a -> s {raidLevel = a} :: VolumeConfiguration)

-- | For PIOPS volumes, the IOPS per disk.
volumeConfiguration_iops :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_iops = Lens.lens (\VolumeConfiguration' {iops} -> iops) (\s@VolumeConfiguration' {} a -> s {iops = a} :: VolumeConfiguration)

-- | The volume mount point. For example \"\/dev\/sdh\".
volumeConfiguration_mountPoint :: Lens.Lens' VolumeConfiguration Prelude.Text
volumeConfiguration_mountPoint = Lens.lens (\VolumeConfiguration' {mountPoint} -> mountPoint) (\s@VolumeConfiguration' {} a -> s {mountPoint = a} :: VolumeConfiguration)

-- | The number of disks in the volume.
volumeConfiguration_numberOfDisks :: Lens.Lens' VolumeConfiguration Prelude.Int
volumeConfiguration_numberOfDisks = Lens.lens (\VolumeConfiguration' {numberOfDisks} -> numberOfDisks) (\s@VolumeConfiguration' {} a -> s {numberOfDisks = a} :: VolumeConfiguration)

-- | The volume size.
volumeConfiguration_size :: Lens.Lens' VolumeConfiguration Prelude.Int
volumeConfiguration_size = Lens.lens (\VolumeConfiguration' {size} -> size) (\s@VolumeConfiguration' {} a -> s {size = a} :: VolumeConfiguration)

instance Data.FromJSON VolumeConfiguration where
  parseJSON =
    Data.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Prelude.<$> (x Data..:? "VolumeType")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "RaidLevel")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..: "MountPoint")
            Prelude.<*> (x Data..: "NumberOfDisks")
            Prelude.<*> (x Data..: "Size")
      )

instance Prelude.Hashable VolumeConfiguration where
  hashWithSalt _salt VolumeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` raidLevel
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` mountPoint
      `Prelude.hashWithSalt` numberOfDisks
      `Prelude.hashWithSalt` size

instance Prelude.NFData VolumeConfiguration where
  rnf VolumeConfiguration' {..} =
    Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf raidLevel
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf mountPoint
      `Prelude.seq` Prelude.rnf numberOfDisks
      `Prelude.seq` Prelude.rnf size

instance Data.ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VolumeType" Data..=) Prelude.<$> volumeType,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("RaidLevel" Data..=) Prelude.<$> raidLevel,
            ("Iops" Data..=) Prelude.<$> iops,
            Prelude.Just ("MountPoint" Data..= mountPoint),
            Prelude.Just ("NumberOfDisks" Data..= numberOfDisks),
            Prelude.Just ("Size" Data..= size)
          ]
      )
