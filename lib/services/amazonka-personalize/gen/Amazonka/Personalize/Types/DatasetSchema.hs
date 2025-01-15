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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Describes the schema for a dataset. For more information on schemas, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSchema.html CreateSchema>.
--
-- /See:/ 'newDatasetSchema' smart constructor.
data DatasetSchema = DatasetSchema'
  { -- | The date and time (in Unix time) that the schema was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The domain of a schema that you created for a dataset in a Domain
    -- dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The date and time (in Unix time) that the schema was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schema.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text
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
-- 'creationDateTime', 'datasetSchema_creationDateTime' - The date and time (in Unix time) that the schema was created.
--
-- 'domain', 'datasetSchema_domain' - The domain of a schema that you created for a dataset in a Domain
-- dataset group.
--
-- 'lastUpdatedDateTime', 'datasetSchema_lastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
--
-- 'name', 'datasetSchema_name' - The name of the schema.
--
-- 'schema', 'datasetSchema_schema' - The schema.
--
-- 'schemaArn', 'datasetSchema_schemaArn' - The Amazon Resource Name (ARN) of the schema.
newDatasetSchema ::
  DatasetSchema
newDatasetSchema =
  DatasetSchema'
    { creationDateTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      schema = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the schema was created.
datasetSchema_creationDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_creationDateTime = Lens.lens (\DatasetSchema' {creationDateTime} -> creationDateTime) (\s@DatasetSchema' {} a -> s {creationDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Data._Time

-- | The domain of a schema that you created for a dataset in a Domain
-- dataset group.
datasetSchema_domain :: Lens.Lens' DatasetSchema (Prelude.Maybe Domain)
datasetSchema_domain = Lens.lens (\DatasetSchema' {domain} -> domain) (\s@DatasetSchema' {} a -> s {domain = a} :: DatasetSchema)

-- | The date and time (in Unix time) that the schema was last updated.
datasetSchema_lastUpdatedDateTime :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.UTCTime)
datasetSchema_lastUpdatedDateTime = Lens.lens (\DatasetSchema' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSchema' {} a -> s {lastUpdatedDateTime = a} :: DatasetSchema) Prelude.. Lens.mapping Data._Time

-- | The name of the schema.
datasetSchema_name :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_name = Lens.lens (\DatasetSchema' {name} -> name) (\s@DatasetSchema' {} a -> s {name = a} :: DatasetSchema)

-- | The schema.
datasetSchema_schema :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schema = Lens.lens (\DatasetSchema' {schema} -> schema) (\s@DatasetSchema' {} a -> s {schema = a} :: DatasetSchema)

-- | The Amazon Resource Name (ARN) of the schema.
datasetSchema_schemaArn :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_schemaArn = Lens.lens (\DatasetSchema' {schemaArn} -> schemaArn) (\s@DatasetSchema' {} a -> s {schemaArn = a} :: DatasetSchema)

instance Data.FromJSON DatasetSchema where
  parseJSON =
    Data.withObject
      "DatasetSchema"
      ( \x ->
          DatasetSchema'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "domain")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "schema")
            Prelude.<*> (x Data..:? "schemaArn")
      )

instance Prelude.Hashable DatasetSchema where
  hashWithSalt _salt DatasetSchema' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData DatasetSchema where
  rnf DatasetSchema' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf domain `Prelude.seq`
        Prelude.rnf lastUpdatedDateTime `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf schema `Prelude.seq`
              Prelude.rnf schemaArn
