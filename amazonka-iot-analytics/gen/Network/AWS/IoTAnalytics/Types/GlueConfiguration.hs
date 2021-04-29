{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.GlueConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.GlueConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information for coordination with AWS Glue, a fully
-- managed extract, transform and load (ETL) service.
--
-- /See:/ 'newGlueConfiguration' smart constructor.
data GlueConfiguration = GlueConfiguration'
  { -- | The name of the table in your AWS Glue Data Catalog that is used to
    -- perform the ETL operations. An AWS Glue Data Catalog table contains
    -- partitioned data and descriptions of data sources and targets.
    tableName :: Prelude.Text,
    -- | The name of the database in your AWS Glue Data Catalog in which the
    -- table is located. An AWS Glue Data Catalog database contains metadata
    -- tables.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlueConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'glueConfiguration_tableName' - The name of the table in your AWS Glue Data Catalog that is used to
-- perform the ETL operations. An AWS Glue Data Catalog table contains
-- partitioned data and descriptions of data sources and targets.
--
-- 'databaseName', 'glueConfiguration_databaseName' - The name of the database in your AWS Glue Data Catalog in which the
-- table is located. An AWS Glue Data Catalog database contains metadata
-- tables.
newGlueConfiguration ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  GlueConfiguration
newGlueConfiguration pTableName_ pDatabaseName_ =
  GlueConfiguration'
    { tableName = pTableName_,
      databaseName = pDatabaseName_
    }

-- | The name of the table in your AWS Glue Data Catalog that is used to
-- perform the ETL operations. An AWS Glue Data Catalog table contains
-- partitioned data and descriptions of data sources and targets.
glueConfiguration_tableName :: Lens.Lens' GlueConfiguration Prelude.Text
glueConfiguration_tableName = Lens.lens (\GlueConfiguration' {tableName} -> tableName) (\s@GlueConfiguration' {} a -> s {tableName = a} :: GlueConfiguration)

-- | The name of the database in your AWS Glue Data Catalog in which the
-- table is located. An AWS Glue Data Catalog database contains metadata
-- tables.
glueConfiguration_databaseName :: Lens.Lens' GlueConfiguration Prelude.Text
glueConfiguration_databaseName = Lens.lens (\GlueConfiguration' {databaseName} -> databaseName) (\s@GlueConfiguration' {} a -> s {databaseName = a} :: GlueConfiguration)

instance Prelude.FromJSON GlueConfiguration where
  parseJSON =
    Prelude.withObject
      "GlueConfiguration"
      ( \x ->
          GlueConfiguration'
            Prelude.<$> (x Prelude..: "tableName")
            Prelude.<*> (x Prelude..: "databaseName")
      )

instance Prelude.Hashable GlueConfiguration

instance Prelude.NFData GlueConfiguration

instance Prelude.ToJSON GlueConfiguration where
  toJSON GlueConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tableName" Prelude..= tableName),
            Prelude.Just
              ("databaseName" Prelude..= databaseName)
          ]
      )
