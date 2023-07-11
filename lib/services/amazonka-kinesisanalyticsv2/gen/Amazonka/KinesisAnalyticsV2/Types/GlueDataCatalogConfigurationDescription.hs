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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the Glue Data Catalog that you use for Apache Flink
-- SQL queries and table API transforms that you write in an application.
--
-- /See:/ 'newGlueDataCatalogConfigurationDescription' smart constructor.
data GlueDataCatalogConfigurationDescription = GlueDataCatalogConfigurationDescription'
  { -- | The Amazon Resource Name (ARN) of the database.
    databaseARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueDataCatalogConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseARN', 'glueDataCatalogConfigurationDescription_databaseARN' - The Amazon Resource Name (ARN) of the database.
newGlueDataCatalogConfigurationDescription ::
  -- | 'databaseARN'
  Prelude.Text ->
  GlueDataCatalogConfigurationDescription
newGlueDataCatalogConfigurationDescription
  pDatabaseARN_ =
    GlueDataCatalogConfigurationDescription'
      { databaseARN =
          pDatabaseARN_
      }

-- | The Amazon Resource Name (ARN) of the database.
glueDataCatalogConfigurationDescription_databaseARN :: Lens.Lens' GlueDataCatalogConfigurationDescription Prelude.Text
glueDataCatalogConfigurationDescription_databaseARN = Lens.lens (\GlueDataCatalogConfigurationDescription' {databaseARN} -> databaseARN) (\s@GlueDataCatalogConfigurationDescription' {} a -> s {databaseARN = a} :: GlueDataCatalogConfigurationDescription)

instance
  Data.FromJSON
    GlueDataCatalogConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "GlueDataCatalogConfigurationDescription"
      ( \x ->
          GlueDataCatalogConfigurationDescription'
            Prelude.<$> (x Data..: "DatabaseARN")
      )

instance
  Prelude.Hashable
    GlueDataCatalogConfigurationDescription
  where
  hashWithSalt
    _salt
    GlueDataCatalogConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` databaseARN

instance
  Prelude.NFData
    GlueDataCatalogConfigurationDescription
  where
  rnf GlueDataCatalogConfigurationDescription' {..} =
    Prelude.rnf databaseARN
