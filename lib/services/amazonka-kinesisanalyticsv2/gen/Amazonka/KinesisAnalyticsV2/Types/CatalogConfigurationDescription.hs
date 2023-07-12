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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | The configuration parameters for the default Amazon Glue database. You
-- use this database for Apache Flink SQL queries and table API transforms
-- that you write in a Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newCatalogConfigurationDescription' smart constructor.
data CatalogConfigurationDescription = CatalogConfigurationDescription'
  { -- | The configuration parameters for the default Amazon Glue database. You
    -- use this database for SQL queries that you write in a Kinesis Data
    -- Analytics Studio notebook.
    glueDataCatalogConfigurationDescription :: GlueDataCatalogConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueDataCatalogConfigurationDescription', 'catalogConfigurationDescription_glueDataCatalogConfigurationDescription' - The configuration parameters for the default Amazon Glue database. You
-- use this database for SQL queries that you write in a Kinesis Data
-- Analytics Studio notebook.
newCatalogConfigurationDescription ::
  -- | 'glueDataCatalogConfigurationDescription'
  GlueDataCatalogConfigurationDescription ->
  CatalogConfigurationDescription
newCatalogConfigurationDescription
  pGlueDataCatalogConfigurationDescription_ =
    CatalogConfigurationDescription'
      { glueDataCatalogConfigurationDescription =
          pGlueDataCatalogConfigurationDescription_
      }

-- | The configuration parameters for the default Amazon Glue database. You
-- use this database for SQL queries that you write in a Kinesis Data
-- Analytics Studio notebook.
catalogConfigurationDescription_glueDataCatalogConfigurationDescription :: Lens.Lens' CatalogConfigurationDescription GlueDataCatalogConfigurationDescription
catalogConfigurationDescription_glueDataCatalogConfigurationDescription = Lens.lens (\CatalogConfigurationDescription' {glueDataCatalogConfigurationDescription} -> glueDataCatalogConfigurationDescription) (\s@CatalogConfigurationDescription' {} a -> s {glueDataCatalogConfigurationDescription = a} :: CatalogConfigurationDescription)

instance
  Data.FromJSON
    CatalogConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "CatalogConfigurationDescription"
      ( \x ->
          CatalogConfigurationDescription'
            Prelude.<$> ( x
                            Data..: "GlueDataCatalogConfigurationDescription"
                        )
      )

instance
  Prelude.Hashable
    CatalogConfigurationDescription
  where
  hashWithSalt
    _salt
    CatalogConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` glueDataCatalogConfigurationDescription

instance
  Prelude.NFData
    CatalogConfigurationDescription
  where
  rnf CatalogConfigurationDescription' {..} =
    Prelude.rnf glueDataCatalogConfigurationDescription
