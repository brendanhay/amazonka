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
-- Module      : Amazonka.Personalize.Types.DatasetSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the schema for a dataset. For more information on schemas, see
-- CreateSchema.
--
-- /See:/ 'newDatasetSchema' smart constructor.
data DatasetSchema = DatasetSchema'
  { -- | The date and time (in Unix time) that the schema was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The schema.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the schema was created.
    creationDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDateTime', 'datasetSchema_lastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
--
-- 'schema', 'datasetSchema_schema' - The schema.
--
-- 'schemaArn', 'datasetSchema_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'name', 'datasetSchema_name' - The name of the schema.
--
-- 'creationDateTime', 'datasetSchema_creationDateTime' - The date and time (in Unix time) that the schema was created.
newDatasetSchema ::
  DatasetSchema
newDatasetSchema =
  DatasetSchema'
    { lastUpdatedDateTime =
        Prelude.Nothing,
      schema = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the schema was last updated.
datasetSchema_lastUpdatedDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_lastUpdatedDateTime = Lens.lens (\DatasetSchema' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSchema' {} a -> s {lastUpdatedDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Core._Time

-- | The schema.
datasetSchema_schema :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schema = Lens.lens (\DatasetSchema' {schema} -> schema) (\s@DatasetSchema' {} a -> s {schema = a} :: DatasetSchema)

-- | The Amazon Resource Name (ARN) of the schema.
datasetSchema_schemaArn :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schemaArn = Lens.lens (\DatasetSchema' {schemaArn} -> schemaArn) (\s@DatasetSchema' {} a -> s {schemaArn = a} :: DatasetSchema)

-- | The name of the schema.
datasetSchema_name :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_name = Lens.lens (\DatasetSchema' {name} -> name) (\s@DatasetSchema' {} a -> s {name = a} :: DatasetSchema)

-- | The date and time (in Unix time) that the schema was created.
datasetSchema_creationDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_creationDateTime = Lens.lens (\DatasetSchema' {creationDateTime} -> creationDateTime) (\s@DatasetSchema' {} a -> s {creationDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSchema where
  parseJSON =
    Core.withObject
      "DatasetSchema"
      ( \x ->
          DatasetSchema'
            Prelude.<$> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "schemaArn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
      )

instance Prelude.Hashable DatasetSchema where
  hashWithSalt salt' DatasetSchema' {..} =
    salt' `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData DatasetSchema where
  rnf DatasetSchema' {..} =
    Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schema
