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
-- Module      : Amazonka.EC2.Types.ImportVolumeTaskDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportVolumeTaskDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DiskImageDescription
import Amazonka.EC2.Types.DiskImageVolumeDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes an import volume task.
--
-- /See:/ 'newImportVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { -- | The Availability Zone where the resulting volume will reside.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes converted so far.
    bytesConverted :: Prelude.Maybe Prelude.Integer,
    -- | The description you provided when starting the import volume task.
    description :: Prelude.Maybe Prelude.Text,
    -- | The image.
    image :: Prelude.Maybe DiskImageDescription,
    -- | The volume.
    volume :: Prelude.Maybe DiskImageVolumeDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportVolumeTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'importVolumeTaskDetails_availabilityZone' - The Availability Zone where the resulting volume will reside.
--
-- 'bytesConverted', 'importVolumeTaskDetails_bytesConverted' - The number of bytes converted so far.
--
-- 'description', 'importVolumeTaskDetails_description' - The description you provided when starting the import volume task.
--
-- 'image', 'importVolumeTaskDetails_image' - The image.
--
-- 'volume', 'importVolumeTaskDetails_volume' - The volume.
newImportVolumeTaskDetails ::
  ImportVolumeTaskDetails
newImportVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { availabilityZone =
        Prelude.Nothing,
      bytesConverted = Prelude.Nothing,
      description = Prelude.Nothing,
      image = Prelude.Nothing,
      volume = Prelude.Nothing
    }

-- | The Availability Zone where the resulting volume will reside.
importVolumeTaskDetails_availabilityZone :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Text)
importVolumeTaskDetails_availabilityZone = Lens.lens (\ImportVolumeTaskDetails' {availabilityZone} -> availabilityZone) (\s@ImportVolumeTaskDetails' {} a -> s {availabilityZone = a} :: ImportVolumeTaskDetails)

-- | The number of bytes converted so far.
importVolumeTaskDetails_bytesConverted :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Integer)
importVolumeTaskDetails_bytesConverted = Lens.lens (\ImportVolumeTaskDetails' {bytesConverted} -> bytesConverted) (\s@ImportVolumeTaskDetails' {} a -> s {bytesConverted = a} :: ImportVolumeTaskDetails)

-- | The description you provided when starting the import volume task.
importVolumeTaskDetails_description :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe Prelude.Text)
importVolumeTaskDetails_description = Lens.lens (\ImportVolumeTaskDetails' {description} -> description) (\s@ImportVolumeTaskDetails' {} a -> s {description = a} :: ImportVolumeTaskDetails)

-- | The image.
importVolumeTaskDetails_image :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe DiskImageDescription)
importVolumeTaskDetails_image = Lens.lens (\ImportVolumeTaskDetails' {image} -> image) (\s@ImportVolumeTaskDetails' {} a -> s {image = a} :: ImportVolumeTaskDetails)

-- | The volume.
importVolumeTaskDetails_volume :: Lens.Lens' ImportVolumeTaskDetails (Prelude.Maybe DiskImageVolumeDescription)
importVolumeTaskDetails_volume = Lens.lens (\ImportVolumeTaskDetails' {volume} -> volume) (\s@ImportVolumeTaskDetails' {} a -> s {volume = a} :: ImportVolumeTaskDetails)

instance Data.FromXML ImportVolumeTaskDetails where
  parseXML x =
    ImportVolumeTaskDetails'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "bytesConverted")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "image")
      Prelude.<*> (x Data..@? "volume")

instance Prelude.Hashable ImportVolumeTaskDetails where
  hashWithSalt _salt ImportVolumeTaskDetails' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` bytesConverted
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` volume

instance Prelude.NFData ImportVolumeTaskDetails where
  rnf ImportVolumeTaskDetails' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf bytesConverted
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf volume
