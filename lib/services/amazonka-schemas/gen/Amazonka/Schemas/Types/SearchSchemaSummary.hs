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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SearchSchemaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.SearchSchemaVersionSummary

-- | /See:/ 'newSearchSchemaSummary' smart constructor.
data SearchSchemaSummary = SearchSchemaSummary'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | An array of schema version summaries.
    schemaVersions :: Prelude.Maybe [SearchSchemaVersionSummary],
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text
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
-- 'schemaVersions', 'searchSchemaSummary_schemaVersions' - An array of schema version summaries.
--
-- 'schemaName', 'searchSchemaSummary_schemaName' - The name of the schema.
--
-- 'schemaArn', 'searchSchemaSummary_schemaArn' - The ARN of the schema.
newSearchSchemaSummary ::
  SearchSchemaSummary
newSearchSchemaSummary =
  SearchSchemaSummary'
    { registryName =
        Prelude.Nothing,
      schemaVersions = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The name of the registry.
searchSchemaSummary_registryName :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_registryName = Lens.lens (\SearchSchemaSummary' {registryName} -> registryName) (\s@SearchSchemaSummary' {} a -> s {registryName = a} :: SearchSchemaSummary)

-- | An array of schema version summaries.
searchSchemaSummary_schemaVersions :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe [SearchSchemaVersionSummary])
searchSchemaSummary_schemaVersions = Lens.lens (\SearchSchemaSummary' {schemaVersions} -> schemaVersions) (\s@SearchSchemaSummary' {} a -> s {schemaVersions = a} :: SearchSchemaSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the schema.
searchSchemaSummary_schemaName :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_schemaName = Lens.lens (\SearchSchemaSummary' {schemaName} -> schemaName) (\s@SearchSchemaSummary' {} a -> s {schemaName = a} :: SearchSchemaSummary)

-- | The ARN of the schema.
searchSchemaSummary_schemaArn :: Lens.Lens' SearchSchemaSummary (Prelude.Maybe Prelude.Text)
searchSchemaSummary_schemaArn = Lens.lens (\SearchSchemaSummary' {schemaArn} -> schemaArn) (\s@SearchSchemaSummary' {} a -> s {schemaArn = a} :: SearchSchemaSummary)

instance Core.FromJSON SearchSchemaSummary where
  parseJSON =
    Core.withObject
      "SearchSchemaSummary"
      ( \x ->
          SearchSchemaSummary'
            Prelude.<$> (x Core..:? "RegistryName")
            Prelude.<*> (x Core..:? "SchemaVersions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "SchemaArn")
      )

instance Prelude.Hashable SearchSchemaSummary where
  hashWithSalt _salt SearchSchemaSummary' {..} =
    _salt `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaVersions
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData SearchSchemaSummary where
  rnf SearchSchemaSummary' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaVersions
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaArn
