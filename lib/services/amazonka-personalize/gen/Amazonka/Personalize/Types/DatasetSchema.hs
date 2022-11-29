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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Describes the schema for a dataset. For more information on schemas, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSchema.html CreateSchema>.
--
-- /See:/ 'newDatasetSchema' smart constructor.
data DatasetSchema = DatasetSchema'
  { -- | The name of the schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the schema was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The domain of a schema that you created for a dataset in a Domain
    -- dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The schema.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the schema was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
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
-- 'name', 'datasetSchema_name' - The name of the schema.
--
-- 'creationDateTime', 'datasetSchema_creationDateTime' - The date and time (in Unix time) that the schema was created.
--
-- 'domain', 'datasetSchema_domain' - The domain of a schema that you created for a dataset in a Domain
-- dataset group.
--
-- 'schemaArn', 'datasetSchema_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'schema', 'datasetSchema_schema' - The schema.
--
-- 'lastUpdatedDateTime', 'datasetSchema_lastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
newDatasetSchema ::
  DatasetSchema
newDatasetSchema =
  DatasetSchema'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      schema = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the schema.
datasetSchema_name :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_name = Lens.lens (\DatasetSchema' {name} -> name) (\s@DatasetSchema' {} a -> s {name = a} :: DatasetSchema)

-- | The date and time (in Unix time) that the schema was created.
datasetSchema_creationDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_creationDateTime = Lens.lens (\DatasetSchema' {creationDateTime} -> creationDateTime) (\s@DatasetSchema' {} a -> s {creationDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Core._Time

-- | The domain of a schema that you created for a dataset in a Domain
-- dataset group.
datasetSchema_domain :: Lens.Lens' DatasetSchema (Prelude.Maybe Domain)
datasetSchema_domain = Lens.lens (\DatasetSchema' {domain} -> domain) (\s@DatasetSchema' {} a -> s {domain = a} :: DatasetSchema)

-- | The Amazon Resource Name (ARN) of the schema.
datasetSchema_schemaArn :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schemaArn = Lens.lens (\DatasetSchema' {schemaArn} -> schemaArn) (\s@DatasetSchema' {} a -> s {schemaArn = a} :: DatasetSchema)

-- | The schema.
datasetSchema_schema :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schema = Lens.lens (\DatasetSchema' {schema} -> schema) (\s@DatasetSchema' {} a -> s {schema = a} :: DatasetSchema)

-- | The date and time (in Unix time) that the schema was last updated.
datasetSchema_lastUpdatedDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_lastUpdatedDateTime = Lens.lens (\DatasetSchema' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSchema' {} a -> s {lastUpdatedDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSchema where
  parseJSON =
    Core.withObject
      "DatasetSchema"
      ( \x ->
          DatasetSchema'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "domain")
            Prelude.<*> (x Core..:? "schemaArn")
            Prelude.<*> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable DatasetSchema where
  hashWithSalt _salt DatasetSchema' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData DatasetSchema where
  rnf DatasetSchema' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
