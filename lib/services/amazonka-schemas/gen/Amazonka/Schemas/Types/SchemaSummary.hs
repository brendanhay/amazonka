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
-- Module      : Amazonka.Schemas.Types.SchemaSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SchemaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of schema details.
--
-- /See:/ 'newSchemaSummary' smart constructor.
data SchemaSummary = SchemaSummary'
  { -- | The date and time that schema was modified.
    lastModified :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the schema.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of versions available for the schema.
    versionCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModified', 'schemaSummary_lastModified' - The date and time that schema was modified.
--
-- 'schemaArn', 'schemaSummary_schemaArn' - The ARN of the schema.
--
-- 'schemaName', 'schemaSummary_schemaName' - The name of the schema.
--
-- 'tags', 'schemaSummary_tags' - Tags associated with the schema.
--
-- 'versionCount', 'schemaSummary_versionCount' - The number of versions available for the schema.
newSchemaSummary ::
  SchemaSummary
newSchemaSummary =
  SchemaSummary'
    { lastModified = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      tags = Prelude.Nothing,
      versionCount = Prelude.Nothing
    }

-- | The date and time that schema was modified.
schemaSummary_lastModified :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.UTCTime)
schemaSummary_lastModified = Lens.lens (\SchemaSummary' {lastModified} -> lastModified) (\s@SchemaSummary' {} a -> s {lastModified = a} :: SchemaSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the schema.
schemaSummary_schemaArn :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Text)
schemaSummary_schemaArn = Lens.lens (\SchemaSummary' {schemaArn} -> schemaArn) (\s@SchemaSummary' {} a -> s {schemaArn = a} :: SchemaSummary)

-- | The name of the schema.
schemaSummary_schemaName :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Text)
schemaSummary_schemaName = Lens.lens (\SchemaSummary' {schemaName} -> schemaName) (\s@SchemaSummary' {} a -> s {schemaName = a} :: SchemaSummary)

-- | Tags associated with the schema.
schemaSummary_tags :: Lens.Lens' SchemaSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
schemaSummary_tags = Lens.lens (\SchemaSummary' {tags} -> tags) (\s@SchemaSummary' {} a -> s {tags = a} :: SchemaSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of versions available for the schema.
schemaSummary_versionCount :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Integer)
schemaSummary_versionCount = Lens.lens (\SchemaSummary' {versionCount} -> versionCount) (\s@SchemaSummary' {} a -> s {versionCount = a} :: SchemaSummary)

instance Data.FromJSON SchemaSummary where
  parseJSON =
    Data.withObject
      "SchemaSummary"
      ( \x ->
          SchemaSummary'
            Prelude.<$> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "SchemaArn")
            Prelude.<*> (x Data..:? "SchemaName")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VersionCount")
      )

instance Prelude.Hashable SchemaSummary where
  hashWithSalt _salt SchemaSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` versionCount

instance Prelude.NFData SchemaSummary where
  rnf SchemaSummary' {..} =
    Prelude.rnf lastModified `Prelude.seq`
      Prelude.rnf schemaArn `Prelude.seq`
        Prelude.rnf schemaName `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf versionCount
