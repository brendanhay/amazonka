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
-- Module      : Amazonka.AppFlow.Types.SourceFieldProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SourceFieldProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that can be applied to a field when the connector is
-- being used as a source.
--
-- /See:/ 'newSourceFieldProperties' smart constructor.
data SourceFieldProperties = SourceFieldProperties'
  { -- | Indicates if the field can be queried.
    isQueryable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the field can be returned in a search result.
    isRetrievable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if this timestamp field can be used for incremental queries.
    isTimestampFieldForIncrementalQueries :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceFieldProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isQueryable', 'sourceFieldProperties_isQueryable' - Indicates if the field can be queried.
--
-- 'isRetrievable', 'sourceFieldProperties_isRetrievable' - Indicates whether the field can be returned in a search result.
--
-- 'isTimestampFieldForIncrementalQueries', 'sourceFieldProperties_isTimestampFieldForIncrementalQueries' - Indicates if this timestamp field can be used for incremental queries.
newSourceFieldProperties ::
  SourceFieldProperties
newSourceFieldProperties =
  SourceFieldProperties'
    { isQueryable =
        Prelude.Nothing,
      isRetrievable = Prelude.Nothing,
      isTimestampFieldForIncrementalQueries =
        Prelude.Nothing
    }

-- | Indicates if the field can be queried.
sourceFieldProperties_isQueryable :: Lens.Lens' SourceFieldProperties (Prelude.Maybe Prelude.Bool)
sourceFieldProperties_isQueryable = Lens.lens (\SourceFieldProperties' {isQueryable} -> isQueryable) (\s@SourceFieldProperties' {} a -> s {isQueryable = a} :: SourceFieldProperties)

-- | Indicates whether the field can be returned in a search result.
sourceFieldProperties_isRetrievable :: Lens.Lens' SourceFieldProperties (Prelude.Maybe Prelude.Bool)
sourceFieldProperties_isRetrievable = Lens.lens (\SourceFieldProperties' {isRetrievable} -> isRetrievable) (\s@SourceFieldProperties' {} a -> s {isRetrievable = a} :: SourceFieldProperties)

-- | Indicates if this timestamp field can be used for incremental queries.
sourceFieldProperties_isTimestampFieldForIncrementalQueries :: Lens.Lens' SourceFieldProperties (Prelude.Maybe Prelude.Bool)
sourceFieldProperties_isTimestampFieldForIncrementalQueries = Lens.lens (\SourceFieldProperties' {isTimestampFieldForIncrementalQueries} -> isTimestampFieldForIncrementalQueries) (\s@SourceFieldProperties' {} a -> s {isTimestampFieldForIncrementalQueries = a} :: SourceFieldProperties)

instance Data.FromJSON SourceFieldProperties where
  parseJSON =
    Data.withObject
      "SourceFieldProperties"
      ( \x ->
          SourceFieldProperties'
            Prelude.<$> (x Data..:? "isQueryable")
            Prelude.<*> (x Data..:? "isRetrievable")
            Prelude.<*> (x Data..:? "isTimestampFieldForIncrementalQueries")
      )

instance Prelude.Hashable SourceFieldProperties where
  hashWithSalt _salt SourceFieldProperties' {..} =
    _salt
      `Prelude.hashWithSalt` isQueryable
      `Prelude.hashWithSalt` isRetrievable
      `Prelude.hashWithSalt` isTimestampFieldForIncrementalQueries

instance Prelude.NFData SourceFieldProperties where
  rnf SourceFieldProperties' {..} =
    Prelude.rnf isQueryable `Prelude.seq`
      Prelude.rnf isRetrievable `Prelude.seq`
        Prelude.rnf isTimestampFieldForIncrementalQueries
