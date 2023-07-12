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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates to the configuration of the Glue Data Catalog that you use for
-- SQL queries that you write in a Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newGlueDataCatalogConfigurationUpdate' smart constructor.
data GlueDataCatalogConfigurationUpdate = GlueDataCatalogConfigurationUpdate'
  { -- | The updated Amazon Resource Name (ARN) of the database.
    databaseARNUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueDataCatalogConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseARNUpdate', 'glueDataCatalogConfigurationUpdate_databaseARNUpdate' - The updated Amazon Resource Name (ARN) of the database.
newGlueDataCatalogConfigurationUpdate ::
  -- | 'databaseARNUpdate'
  Prelude.Text ->
  GlueDataCatalogConfigurationUpdate
newGlueDataCatalogConfigurationUpdate
  pDatabaseARNUpdate_ =
    GlueDataCatalogConfigurationUpdate'
      { databaseARNUpdate =
          pDatabaseARNUpdate_
      }

-- | The updated Amazon Resource Name (ARN) of the database.
glueDataCatalogConfigurationUpdate_databaseARNUpdate :: Lens.Lens' GlueDataCatalogConfigurationUpdate Prelude.Text
glueDataCatalogConfigurationUpdate_databaseARNUpdate = Lens.lens (\GlueDataCatalogConfigurationUpdate' {databaseARNUpdate} -> databaseARNUpdate) (\s@GlueDataCatalogConfigurationUpdate' {} a -> s {databaseARNUpdate = a} :: GlueDataCatalogConfigurationUpdate)

instance
  Prelude.Hashable
    GlueDataCatalogConfigurationUpdate
  where
  hashWithSalt
    _salt
    GlueDataCatalogConfigurationUpdate' {..} =
      _salt `Prelude.hashWithSalt` databaseARNUpdate

instance
  Prelude.NFData
    GlueDataCatalogConfigurationUpdate
  where
  rnf GlueDataCatalogConfigurationUpdate' {..} =
    Prelude.rnf databaseARNUpdate

instance
  Data.ToJSON
    GlueDataCatalogConfigurationUpdate
  where
  toJSON GlueDataCatalogConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DatabaseARNUpdate" Data..= databaseARNUpdate)
          ]
      )
