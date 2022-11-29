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
-- Module      : Amazonka.CloudTrail.Types.ImportFailureListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ImportFailureListItem where

import Amazonka.CloudTrail.Types.ImportFailureStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an import failure.
--
-- /See:/ 'newImportFailureListItem' smart constructor.
data ImportFailureListItem = ImportFailureListItem'
  { -- | Provides the reason the import failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the import.
    status :: Prelude.Maybe ImportFailureStatus,
    -- | When the import was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The location of the failure in the S3 bucket.
    location :: Prelude.Maybe Prelude.Text,
    -- | The type of import error.
    errorType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportFailureListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'importFailureListItem_errorMessage' - Provides the reason the import failed.
--
-- 'status', 'importFailureListItem_status' - The status of the import.
--
-- 'lastUpdatedTime', 'importFailureListItem_lastUpdatedTime' - When the import was last updated.
--
-- 'location', 'importFailureListItem_location' - The location of the failure in the S3 bucket.
--
-- 'errorType', 'importFailureListItem_errorType' - The type of import error.
newImportFailureListItem ::
  ImportFailureListItem
newImportFailureListItem =
  ImportFailureListItem'
    { errorMessage =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      location = Prelude.Nothing,
      errorType = Prelude.Nothing
    }

-- | Provides the reason the import failed.
importFailureListItem_errorMessage :: Lens.Lens' ImportFailureListItem (Prelude.Maybe Prelude.Text)
importFailureListItem_errorMessage = Lens.lens (\ImportFailureListItem' {errorMessage} -> errorMessage) (\s@ImportFailureListItem' {} a -> s {errorMessage = a} :: ImportFailureListItem)

-- | The status of the import.
importFailureListItem_status :: Lens.Lens' ImportFailureListItem (Prelude.Maybe ImportFailureStatus)
importFailureListItem_status = Lens.lens (\ImportFailureListItem' {status} -> status) (\s@ImportFailureListItem' {} a -> s {status = a} :: ImportFailureListItem)

-- | When the import was last updated.
importFailureListItem_lastUpdatedTime :: Lens.Lens' ImportFailureListItem (Prelude.Maybe Prelude.UTCTime)
importFailureListItem_lastUpdatedTime = Lens.lens (\ImportFailureListItem' {lastUpdatedTime} -> lastUpdatedTime) (\s@ImportFailureListItem' {} a -> s {lastUpdatedTime = a} :: ImportFailureListItem) Prelude.. Lens.mapping Core._Time

-- | The location of the failure in the S3 bucket.
importFailureListItem_location :: Lens.Lens' ImportFailureListItem (Prelude.Maybe Prelude.Text)
importFailureListItem_location = Lens.lens (\ImportFailureListItem' {location} -> location) (\s@ImportFailureListItem' {} a -> s {location = a} :: ImportFailureListItem)

-- | The type of import error.
importFailureListItem_errorType :: Lens.Lens' ImportFailureListItem (Prelude.Maybe Prelude.Text)
importFailureListItem_errorType = Lens.lens (\ImportFailureListItem' {errorType} -> errorType) (\s@ImportFailureListItem' {} a -> s {errorType = a} :: ImportFailureListItem)

instance Core.FromJSON ImportFailureListItem where
  parseJSON =
    Core.withObject
      "ImportFailureListItem"
      ( \x ->
          ImportFailureListItem'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "ErrorType")
      )

instance Prelude.Hashable ImportFailureListItem where
  hashWithSalt _salt ImportFailureListItem' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` errorType

instance Prelude.NFData ImportFailureListItem where
  rnf ImportFailureListItem' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf errorType
