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
-- Module      : Network.AWS.EC2.Types.ImportVolumeTaskDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportVolumeTaskDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an import volume task.
--
-- /See:/ 'newImportVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { -- | The number of bytes converted so far.
    bytesConverted :: Prelude.Maybe Prelude.Integer,
    -- | The volume.
    volume :: Prelude.Maybe DiskImageVolumeDescription,
    -- | The image.
    image :: Prelude.Maybe DiskImageDescription,
    -- | The Availability Zone where the resulting volume will reside.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The description you provided when starting the import volume task.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportVolumeTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesConverted', 'importVolumeTaskDetails_bytesConverted' - The number of bytes converted so far.
--
-- 'volume', 'importVolumeTaskDetails_volume' - The volume.
--
-- 'image', 'importVolumeTaskDetails_image' - The image.
--
-- 'availabilityZone', 'importVolumeTaskDetails_availabilityZone' - The Availability Zone where the resulting volume will reside.
--
-- 'description', 'importVolumeTaskDetails_description' - The description you provided when starting the import volume task.
newImportVolumeTaskDetails ::
  ImportVolumeTaskDetails
newImportVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { bytesConverted =
        Prelude.Nothing,
      volume = Prelude.Nothing,
      image = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The number of bytes converted so far.
importVolumeTaskDetails_bytesConverted :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Integer)
importVolumeTaskDetails_bytesConverted = Lens.lens (\ImportVolumeTaskDetails' {bytesConverted} -> bytesConverted) (\s@ImportVolumeTaskDetails' {} a -> s {bytesConverted = a} :: ImportVolumeTaskDetails)

-- | The volume.
importVolumeTaskDetails_volume :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe DiskImageVolumeDescription)
importVolumeTaskDetails_volume = Lens.lens (\ImportVolumeTaskDetails' {volume} -> volume) (\s@ImportVolumeTaskDetails' {} a -> s {volume = a} :: ImportVolumeTaskDetails)

-- | The image.
importVolumeTaskDetails_image :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe DiskImageDescription)
importVolumeTaskDetails_image = Lens.lens (\ImportVolumeTaskDetails' {image} -> image) (\s@ImportVolumeTaskDetails' {} a -> s {image = a} :: ImportVolumeTaskDetails)

-- | The Availability Zone where the resulting volume will reside.
importVolumeTaskDetails_availabilityZone :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Text)
importVolumeTaskDetails_availabilityZone = Lens.lens (\ImportVolumeTaskDetails' {availabilityZone} -> availabilityZone) (\s@ImportVolumeTaskDetails' {} a -> s {availabilityZone = a} :: ImportVolumeTaskDetails)

-- | The description you provided when starting the import volume task.
importVolumeTaskDetails_description :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Text)
importVolumeTaskDetails_description = Lens.lens (\ImportVolumeTaskDetails' {description} -> description) (\s@ImportVolumeTaskDetails' {} a -> s {description = a} :: ImportVolumeTaskDetails)

instance Prelude.FromXML ImportVolumeTaskDetails where
  parseXML x =
    ImportVolumeTaskDetails'
      Prelude.<$> (x Prelude..@? "bytesConverted")
      Prelude.<*> (x Prelude..@? "volume")
      Prelude.<*> (x Prelude..@? "image")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable ImportVolumeTaskDetails

instance Prelude.NFData ImportVolumeTaskDetails
