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
-- Module      : Amazonka.Personalize.Types.DatasetSchemaSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSchemaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset schema. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSchema.html DescribeSchema>
-- API.
--
-- /See:/ 'newDatasetSchemaSummary' smart constructor.
data DatasetSchemaSummary = DatasetSchemaSummary'
  { -- | The name of the schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the schema was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The domain of a schema that you created for a dataset in a Domain
    -- dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the schema was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSchemaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'datasetSchemaSummary_name' - The name of the schema.
--
-- 'creationDateTime', 'datasetSchemaSummary_creationDateTime' - The date and time (in Unix time) that the schema was created.
--
-- 'domain', 'datasetSchemaSummary_domain' - The domain of a schema that you created for a dataset in a Domain
-- dataset group.
--
-- 'schemaArn', 'datasetSchemaSummary_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'lastUpdatedDateTime', 'datasetSchemaSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
newDatasetSchemaSummary ::
  DatasetSchemaSummary
newDatasetSchemaSummary =
  DatasetSchemaSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the schema.
datasetSchemaSummary_name :: Lens.Lens' DatasetSchemaSummary (Prelude.Maybe Prelude.Text)
datasetSchemaSummary_name = Lens.lens (\DatasetSchemaSummary' {name} -> name) (\s@DatasetSchemaSummary' {} a -> s {name = a} :: DatasetSchemaSummary)

-- | The date and time (in Unix time) that the schema was created.
datasetSchemaSummary_creationDateTime :: Lens.Lens' DatasetSchemaSummary (Prelude.Maybe Prelude.UTCTime)
datasetSchemaSummary_creationDateTime = Lens.lens (\DatasetSchemaSummary' {creationDateTime} -> creationDateTime) (\s@DatasetSchemaSummary' {} a -> s {creationDateTime = a} :: DatasetSchemaSummary) Prelude.. Lens.mapping Core._Time

-- | The domain of a schema that you created for a dataset in a Domain
-- dataset group.
datasetSchemaSummary_domain :: Lens.Lens' DatasetSchemaSummary (Prelude.Maybe Domain)
datasetSchemaSummary_domain = Lens.lens (\DatasetSchemaSummary' {domain} -> domain) (\s@DatasetSchemaSummary' {} a -> s {domain = a} :: DatasetSchemaSummary)

-- | The Amazon Resource Name (ARN) of the schema.
datasetSchemaSummary_schemaArn :: Lens.Lens' DatasetSchemaSummary (Prelude.Maybe Prelude.Text)
datasetSchemaSummary_schemaArn = Lens.lens (\DatasetSchemaSummary' {schemaArn} -> schemaArn) (\s@DatasetSchemaSummary' {} a -> s {schemaArn = a} :: DatasetSchemaSummary)

-- | The date and time (in Unix time) that the schema was last updated.
datasetSchemaSummary_lastUpdatedDateTime :: Lens.Lens' DatasetSchemaSummary (Prelude.Maybe Prelude.UTCTime)
datasetSchemaSummary_lastUpdatedDateTime = Lens.lens (\DatasetSchemaSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSchemaSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetSchemaSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSchemaSummary where
  parseJSON =
    Core.withObject
      "DatasetSchemaSummary"
      ( \x ->
          DatasetSchemaSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "domain")
            Prelude.<*> (x Core..:? "schemaArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable DatasetSchemaSummary where
  hashWithSalt _salt DatasetSchemaSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData DatasetSchemaSummary where
  rnf DatasetSchemaSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
