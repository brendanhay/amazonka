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
-- Module      : Amazonka.Schemas.Types.SearchSchemaVersionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SearchSchemaVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.Type

-- | /See:/ 'newSearchSchemaVersionSummary' smart constructor.
data SearchSchemaVersionSummary = SearchSchemaVersionSummary'
  { -- | The type of schema.
    type' :: Prelude.Maybe Type,
    -- | The version number of the schema
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the schema version was created.
    createdDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSchemaVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'searchSchemaVersionSummary_type' - The type of schema.
--
-- 'schemaVersion', 'searchSchemaVersionSummary_schemaVersion' - The version number of the schema
--
-- 'createdDate', 'searchSchemaVersionSummary_createdDate' - The date the schema version was created.
newSearchSchemaVersionSummary ::
  SearchSchemaVersionSummary
newSearchSchemaVersionSummary =
  SearchSchemaVersionSummary'
    { type' =
        Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      createdDate = Prelude.Nothing
    }

-- | The type of schema.
searchSchemaVersionSummary_type :: Lens.Lens' SearchSchemaVersionSummary (Prelude.Maybe Type)
searchSchemaVersionSummary_type = Lens.lens (\SearchSchemaVersionSummary' {type'} -> type') (\s@SearchSchemaVersionSummary' {} a -> s {type' = a} :: SearchSchemaVersionSummary)

-- | The version number of the schema
searchSchemaVersionSummary_schemaVersion :: Lens.Lens' SearchSchemaVersionSummary (Prelude.Maybe Prelude.Text)
searchSchemaVersionSummary_schemaVersion = Lens.lens (\SearchSchemaVersionSummary' {schemaVersion} -> schemaVersion) (\s@SearchSchemaVersionSummary' {} a -> s {schemaVersion = a} :: SearchSchemaVersionSummary)

-- | The date the schema version was created.
searchSchemaVersionSummary_createdDate :: Lens.Lens' SearchSchemaVersionSummary (Prelude.Maybe Prelude.UTCTime)
searchSchemaVersionSummary_createdDate = Lens.lens (\SearchSchemaVersionSummary' {createdDate} -> createdDate) (\s@SearchSchemaVersionSummary' {} a -> s {createdDate = a} :: SearchSchemaVersionSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SearchSchemaVersionSummary where
  parseJSON =
    Data.withObject
      "SearchSchemaVersionSummary"
      ( \x ->
          SearchSchemaVersionSummary'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "SchemaVersion")
            Prelude.<*> (x Data..:? "CreatedDate")
      )

instance Prelude.Hashable SearchSchemaVersionSummary where
  hashWithSalt _salt SearchSchemaVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` createdDate

instance Prelude.NFData SearchSchemaVersionSummary where
  rnf SearchSchemaVersionSummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf createdDate
