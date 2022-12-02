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
-- Module      : Amazonka.HealthLake.Types.DatastoreFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.DatastoreFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.DatastoreStatus
import qualified Amazonka.Prelude as Prelude

-- | The filters applied to Data Store query.
--
-- /See:/ 'newDatastoreFilter' smart constructor.
data DatastoreFilter = DatastoreFilter'
  { -- | A filter that allows the user to set cutoff dates for records. All Data
    -- Stores created before the specified date will be included in the
    -- results.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | Allows the user to filter Data Store results by name.
    datastoreName :: Prelude.Maybe Prelude.Text,
    -- | A filter that allows the user to set cutoff dates for records. All Data
    -- Stores created after the specified date will be included in the results.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | Allows the user to filter Data Store results by status.
    datastoreStatus :: Prelude.Maybe DatastoreStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBefore', 'datastoreFilter_createdBefore' - A filter that allows the user to set cutoff dates for records. All Data
-- Stores created before the specified date will be included in the
-- results.
--
-- 'datastoreName', 'datastoreFilter_datastoreName' - Allows the user to filter Data Store results by name.
--
-- 'createdAfter', 'datastoreFilter_createdAfter' - A filter that allows the user to set cutoff dates for records. All Data
-- Stores created after the specified date will be included in the results.
--
-- 'datastoreStatus', 'datastoreFilter_datastoreStatus' - Allows the user to filter Data Store results by status.
newDatastoreFilter ::
  DatastoreFilter
newDatastoreFilter =
  DatastoreFilter'
    { createdBefore = Prelude.Nothing,
      datastoreName = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      datastoreStatus = Prelude.Nothing
    }

-- | A filter that allows the user to set cutoff dates for records. All Data
-- Stores created before the specified date will be included in the
-- results.
datastoreFilter_createdBefore :: Lens.Lens' DatastoreFilter (Prelude.Maybe Prelude.UTCTime)
datastoreFilter_createdBefore = Lens.lens (\DatastoreFilter' {createdBefore} -> createdBefore) (\s@DatastoreFilter' {} a -> s {createdBefore = a} :: DatastoreFilter) Prelude.. Lens.mapping Data._Time

-- | Allows the user to filter Data Store results by name.
datastoreFilter_datastoreName :: Lens.Lens' DatastoreFilter (Prelude.Maybe Prelude.Text)
datastoreFilter_datastoreName = Lens.lens (\DatastoreFilter' {datastoreName} -> datastoreName) (\s@DatastoreFilter' {} a -> s {datastoreName = a} :: DatastoreFilter)

-- | A filter that allows the user to set cutoff dates for records. All Data
-- Stores created after the specified date will be included in the results.
datastoreFilter_createdAfter :: Lens.Lens' DatastoreFilter (Prelude.Maybe Prelude.UTCTime)
datastoreFilter_createdAfter = Lens.lens (\DatastoreFilter' {createdAfter} -> createdAfter) (\s@DatastoreFilter' {} a -> s {createdAfter = a} :: DatastoreFilter) Prelude.. Lens.mapping Data._Time

-- | Allows the user to filter Data Store results by status.
datastoreFilter_datastoreStatus :: Lens.Lens' DatastoreFilter (Prelude.Maybe DatastoreStatus)
datastoreFilter_datastoreStatus = Lens.lens (\DatastoreFilter' {datastoreStatus} -> datastoreStatus) (\s@DatastoreFilter' {} a -> s {datastoreStatus = a} :: DatastoreFilter)

instance Prelude.Hashable DatastoreFilter where
  hashWithSalt _salt DatastoreFilter' {..} =
    _salt `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` datastoreName
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` datastoreStatus

instance Prelude.NFData DatastoreFilter where
  rnf DatastoreFilter' {..} =
    Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf datastoreName
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf datastoreStatus

instance Data.ToJSON DatastoreFilter where
  toJSON DatastoreFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("DatastoreName" Data..=) Prelude.<$> datastoreName,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("DatastoreStatus" Data..=)
              Prelude.<$> datastoreStatus
          ]
      )
