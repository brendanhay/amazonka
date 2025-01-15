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
-- Module      : Amazonka.CloudTrail.Types.ImportsListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ImportsListItem where

import Amazonka.CloudTrail.Types.ImportStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an import that was returned by a lookup
-- request.
--
-- /See:/ 'newImportsListItem' smart constructor.
data ImportsListItem = ImportsListItem'
  { -- | The timestamp of the import\'s creation.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the destination event data store.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The status of the import.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The timestamp of the import\'s last update.
    updatedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'importsListItem_createdTimestamp' - The timestamp of the import\'s creation.
--
-- 'destinations', 'importsListItem_destinations' - The ARN of the destination event data store.
--
-- 'importId', 'importsListItem_importId' - The ID of the import.
--
-- 'importStatus', 'importsListItem_importStatus' - The status of the import.
--
-- 'updatedTimestamp', 'importsListItem_updatedTimestamp' - The timestamp of the import\'s last update.
newImportsListItem ::
  ImportsListItem
newImportsListItem =
  ImportsListItem'
    { createdTimestamp =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      importId = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The timestamp of the import\'s creation.
importsListItem_createdTimestamp :: Lens.Lens' ImportsListItem (Prelude.Maybe Prelude.UTCTime)
importsListItem_createdTimestamp = Lens.lens (\ImportsListItem' {createdTimestamp} -> createdTimestamp) (\s@ImportsListItem' {} a -> s {createdTimestamp = a} :: ImportsListItem) Prelude.. Lens.mapping Data._Time

-- | The ARN of the destination event data store.
importsListItem_destinations :: Lens.Lens' ImportsListItem (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
importsListItem_destinations = Lens.lens (\ImportsListItem' {destinations} -> destinations) (\s@ImportsListItem' {} a -> s {destinations = a} :: ImportsListItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the import.
importsListItem_importId :: Lens.Lens' ImportsListItem (Prelude.Maybe Prelude.Text)
importsListItem_importId = Lens.lens (\ImportsListItem' {importId} -> importId) (\s@ImportsListItem' {} a -> s {importId = a} :: ImportsListItem)

-- | The status of the import.
importsListItem_importStatus :: Lens.Lens' ImportsListItem (Prelude.Maybe ImportStatus)
importsListItem_importStatus = Lens.lens (\ImportsListItem' {importStatus} -> importStatus) (\s@ImportsListItem' {} a -> s {importStatus = a} :: ImportsListItem)

-- | The timestamp of the import\'s last update.
importsListItem_updatedTimestamp :: Lens.Lens' ImportsListItem (Prelude.Maybe Prelude.UTCTime)
importsListItem_updatedTimestamp = Lens.lens (\ImportsListItem' {updatedTimestamp} -> updatedTimestamp) (\s@ImportsListItem' {} a -> s {updatedTimestamp = a} :: ImportsListItem) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ImportsListItem where
  parseJSON =
    Data.withObject
      "ImportsListItem"
      ( \x ->
          ImportsListItem'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Destinations")
            Prelude.<*> (x Data..:? "ImportId")
            Prelude.<*> (x Data..:? "ImportStatus")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable ImportsListItem where
  hashWithSalt _salt ImportsListItem' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` importId
      `Prelude.hashWithSalt` importStatus
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData ImportsListItem where
  rnf ImportsListItem' {..} =
    Prelude.rnf createdTimestamp `Prelude.seq`
      Prelude.rnf destinations `Prelude.seq`
        Prelude.rnf importId `Prelude.seq`
          Prelude.rnf importStatus `Prelude.seq`
            Prelude.rnf updatedTimestamp
