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
-- Module      : Amazonka.EC2.Types.ImportInstanceVolumeDetailItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportInstanceVolumeDetailItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DiskImageDescription
import Amazonka.EC2.Types.DiskImageVolumeDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes an import volume task.
--
-- /See:/ 'newImportInstanceVolumeDetailItem' smart constructor.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem'
  { -- | The Availability Zone where the resulting instance will reside.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes converted so far.
    bytesConverted :: Prelude.Maybe Prelude.Integer,
    -- | A description of the task.
    description :: Prelude.Maybe Prelude.Text,
    -- | The image.
    image :: Prelude.Maybe DiskImageDescription,
    -- | The status of the import of this particular disk image.
    status :: Prelude.Maybe Prelude.Text,
    -- | The status information or errors related to the disk image.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The volume.
    volume :: Prelude.Maybe DiskImageVolumeDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportInstanceVolumeDetailItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'importInstanceVolumeDetailItem_availabilityZone' - The Availability Zone where the resulting instance will reside.
--
-- 'bytesConverted', 'importInstanceVolumeDetailItem_bytesConverted' - The number of bytes converted so far.
--
-- 'description', 'importInstanceVolumeDetailItem_description' - A description of the task.
--
-- 'image', 'importInstanceVolumeDetailItem_image' - The image.
--
-- 'status', 'importInstanceVolumeDetailItem_status' - The status of the import of this particular disk image.
--
-- 'statusMessage', 'importInstanceVolumeDetailItem_statusMessage' - The status information or errors related to the disk image.
--
-- 'volume', 'importInstanceVolumeDetailItem_volume' - The volume.
newImportInstanceVolumeDetailItem ::
  ImportInstanceVolumeDetailItem
newImportInstanceVolumeDetailItem =
  ImportInstanceVolumeDetailItem'
    { availabilityZone =
        Prelude.Nothing,
      bytesConverted = Prelude.Nothing,
      description = Prelude.Nothing,
      image = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      volume = Prelude.Nothing
    }

-- | The Availability Zone where the resulting instance will reside.
importInstanceVolumeDetailItem_availabilityZone :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe Prelude.Text)
importInstanceVolumeDetailItem_availabilityZone = Lens.lens (\ImportInstanceVolumeDetailItem' {availabilityZone} -> availabilityZone) (\s@ImportInstanceVolumeDetailItem' {} a -> s {availabilityZone = a} :: ImportInstanceVolumeDetailItem)

-- | The number of bytes converted so far.
importInstanceVolumeDetailItem_bytesConverted :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe Prelude.Integer)
importInstanceVolumeDetailItem_bytesConverted = Lens.lens (\ImportInstanceVolumeDetailItem' {bytesConverted} -> bytesConverted) (\s@ImportInstanceVolumeDetailItem' {} a -> s {bytesConverted = a} :: ImportInstanceVolumeDetailItem)

-- | A description of the task.
importInstanceVolumeDetailItem_description :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe Prelude.Text)
importInstanceVolumeDetailItem_description = Lens.lens (\ImportInstanceVolumeDetailItem' {description} -> description) (\s@ImportInstanceVolumeDetailItem' {} a -> s {description = a} :: ImportInstanceVolumeDetailItem)

-- | The image.
importInstanceVolumeDetailItem_image :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe DiskImageDescription)
importInstanceVolumeDetailItem_image = Lens.lens (\ImportInstanceVolumeDetailItem' {image} -> image) (\s@ImportInstanceVolumeDetailItem' {} a -> s {image = a} :: ImportInstanceVolumeDetailItem)

-- | The status of the import of this particular disk image.
importInstanceVolumeDetailItem_status :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe Prelude.Text)
importInstanceVolumeDetailItem_status = Lens.lens (\ImportInstanceVolumeDetailItem' {status} -> status) (\s@ImportInstanceVolumeDetailItem' {} a -> s {status = a} :: ImportInstanceVolumeDetailItem)

-- | The status information or errors related to the disk image.
importInstanceVolumeDetailItem_statusMessage :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe Prelude.Text)
importInstanceVolumeDetailItem_statusMessage = Lens.lens (\ImportInstanceVolumeDetailItem' {statusMessage} -> statusMessage) (\s@ImportInstanceVolumeDetailItem' {} a -> s {statusMessage = a} :: ImportInstanceVolumeDetailItem)

-- | The volume.
importInstanceVolumeDetailItem_volume :: Lens.Lens' ImportInstanceVolumeDetailItem (Prelude.Maybe DiskImageVolumeDescription)
importInstanceVolumeDetailItem_volume = Lens.lens (\ImportInstanceVolumeDetailItem' {volume} -> volume) (\s@ImportInstanceVolumeDetailItem' {} a -> s {volume = a} :: ImportInstanceVolumeDetailItem)

instance Data.FromXML ImportInstanceVolumeDetailItem where
  parseXML x =
    ImportInstanceVolumeDetailItem'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "bytesConverted")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "image")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "volume")

instance
  Prelude.Hashable
    ImportInstanceVolumeDetailItem
  where
  hashWithSalt
    _salt
    ImportInstanceVolumeDetailItem' {..} =
      _salt `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` bytesConverted
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` image
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusMessage
        `Prelude.hashWithSalt` volume

instance
  Prelude.NFData
    ImportInstanceVolumeDetailItem
  where
  rnf ImportInstanceVolumeDetailItem' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf bytesConverted
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf volume
