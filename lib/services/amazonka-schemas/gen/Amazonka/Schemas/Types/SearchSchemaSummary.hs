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
-- Module      : Amazonka.Schemas.Types.SearchSchemaSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SearchSchemaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.SearchSchemaVersionSummary

-- | /See:/ 'newSearchSchemaSummary' smart constructor.
data SearchSchemaSummary = SearchSchemaSummary'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | An array of schema version summaries.
    schemaVersions :: Prelude.Maybe [SearchSchemaVersionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSchemaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'searchSchemaSummary_registryName' - The name of the registry.
--
-- 'schemaArn', 'searchSchemaSummary_schemaArn' - The ARN of the schema.
--
-- 'schemaName', 'searchSchemaSummary_schemaName' - The name of the schema.
--
-- 'schemaVersions', 'searchSchemaSummary_schemaVersions' - An array of schema version summaries.
newSearchSchemaSummary ::
  SearchSchemaSummary
newSearchSchemaSummary =
  SearchSchemaSummary'
    { registryName =
        Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaVersions = Prelude.Nothing
    }

-- | The name of the registry.
searchSchemaSummary_registryName :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_registryName = Lens.lens (\SearchSchemaSummary' {registryName} -> registryName) (\s@SearchSchemaSummary' {} a -> s {registryName = a} :: SearchSchemaSummary)

-- | The ARN of the schema.
searchSchemaSummary_schemaArn :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_schemaArn = Lens.lens (\SearchSchemaSummary' {schemaArn} -> schemaArn) (\s@SearchSchemaSummary' {} a -> s {schemaArn = a} :: SearchSchemaSummary)

-- | The name of the schema.
searchSchemaSummary_schemaName :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_schemaName = Lens.lens (\SearchSchemaSummary' {schemaName} -> schemaName) (\s@SearchSchemaSummary' {} a -> s {schemaName = a} :: SearchSchemaSummary)

-- | An array of schema version summaries.
searchSchemaSummary_schemaVersions :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe [SearchSchemaVersionSummary])
searchSchemaSummary_schemaVersions = Lens.lens (\SearchSchemaSummary' {schemaVersions} -> schemaVersions) (\s@SearchSchemaSummary' {} a -> s {schemaVersions = a} :: SearchSchemaSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SearchSchemaSummary where
  parseJSON =
    Data.withObject
      "SearchSchemaSummary"
      ( \x ->
          SearchSchemaSummary'
            Prelude.<$> (x Data..:? "RegistryName")
            Prelude.<*> (x Data..:? "SchemaArn")
            Prelude.<*> (x Data..:? "SchemaName")
            Prelude.<*> ( x
                            Data..:? "SchemaVersions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SearchSchemaSummary where
  hashWithSalt _salt SearchSchemaSummary' {..} =
    _salt
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` schemaVersions

instance Prelude.NFData SearchSchemaSummary where
  rnf SearchSchemaSummary' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaVersions
