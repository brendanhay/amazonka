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
-- Module      : Amazonka.AppFlow.Types.GlueDataCatalogConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.GlueDataCatalogConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- your data with the Glue Data Catalog. When Amazon AppFlow catalogs your
-- data, it stores metadata in Data Catalog tables. This metadata
-- represents the data that\'s transferred by the flow that you configure
-- with these settings.
--
-- You can configure a flow with these settings only when the flow
-- destination is Amazon S3.
--
-- /See:/ 'newGlueDataCatalogConfig' smart constructor.
data GlueDataCatalogConfig = GlueDataCatalogConfig'
  { -- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon AppFlow
    -- the permissions it needs to create Data Catalog tables, databases, and
    -- partitions.
    --
    -- For an example IAM policy that has the required permissions, see
    -- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_id-based-policy-examples.html Identity-based policy examples for Amazon AppFlow>.
    roleArn :: Prelude.Text,
    -- | The name of the Data Catalog database that stores the metadata tables
    -- that Amazon AppFlow creates in your Amazon Web Services account. These
    -- tables contain metadata for the data that\'s transferred by the flow
    -- that you configure with this parameter.
    --
    -- When you configure a new flow with this parameter, you must specify an
    -- existing database.
    databaseName :: Prelude.Text,
    -- | A naming prefix for each Data Catalog table that Amazon AppFlow creates
    -- for the flow that you configure with this setting. Amazon AppFlow adds
    -- the prefix to the beginning of the each table name.
    tablePrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueDataCatalogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'glueDataCatalogConfig_roleArn' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon AppFlow
-- the permissions it needs to create Data Catalog tables, databases, and
-- partitions.
--
-- For an example IAM policy that has the required permissions, see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_id-based-policy-examples.html Identity-based policy examples for Amazon AppFlow>.
--
-- 'databaseName', 'glueDataCatalogConfig_databaseName' - The name of the Data Catalog database that stores the metadata tables
-- that Amazon AppFlow creates in your Amazon Web Services account. These
-- tables contain metadata for the data that\'s transferred by the flow
-- that you configure with this parameter.
--
-- When you configure a new flow with this parameter, you must specify an
-- existing database.
--
-- 'tablePrefix', 'glueDataCatalogConfig_tablePrefix' - A naming prefix for each Data Catalog table that Amazon AppFlow creates
-- for the flow that you configure with this setting. Amazon AppFlow adds
-- the prefix to the beginning of the each table name.
newGlueDataCatalogConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tablePrefix'
  Prelude.Text ->
  GlueDataCatalogConfig
newGlueDataCatalogConfig
  pRoleArn_
  pDatabaseName_
  pTablePrefix_ =
    GlueDataCatalogConfig'
      { roleArn = pRoleArn_,
        databaseName = pDatabaseName_,
        tablePrefix = pTablePrefix_
      }

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon AppFlow
-- the permissions it needs to create Data Catalog tables, databases, and
-- partitions.
--
-- For an example IAM policy that has the required permissions, see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_id-based-policy-examples.html Identity-based policy examples for Amazon AppFlow>.
glueDataCatalogConfig_roleArn :: Lens.Lens' GlueDataCatalogConfig Prelude.Text
glueDataCatalogConfig_roleArn = Lens.lens (\GlueDataCatalogConfig' {roleArn} -> roleArn) (\s@GlueDataCatalogConfig' {} a -> s {roleArn = a} :: GlueDataCatalogConfig)

-- | The name of the Data Catalog database that stores the metadata tables
-- that Amazon AppFlow creates in your Amazon Web Services account. These
-- tables contain metadata for the data that\'s transferred by the flow
-- that you configure with this parameter.
--
-- When you configure a new flow with this parameter, you must specify an
-- existing database.
glueDataCatalogConfig_databaseName :: Lens.Lens' GlueDataCatalogConfig Prelude.Text
glueDataCatalogConfig_databaseName = Lens.lens (\GlueDataCatalogConfig' {databaseName} -> databaseName) (\s@GlueDataCatalogConfig' {} a -> s {databaseName = a} :: GlueDataCatalogConfig)

-- | A naming prefix for each Data Catalog table that Amazon AppFlow creates
-- for the flow that you configure with this setting. Amazon AppFlow adds
-- the prefix to the beginning of the each table name.
glueDataCatalogConfig_tablePrefix :: Lens.Lens' GlueDataCatalogConfig Prelude.Text
glueDataCatalogConfig_tablePrefix = Lens.lens (\GlueDataCatalogConfig' {tablePrefix} -> tablePrefix) (\s@GlueDataCatalogConfig' {} a -> s {tablePrefix = a} :: GlueDataCatalogConfig)

instance Data.FromJSON GlueDataCatalogConfig where
  parseJSON =
    Data.withObject
      "GlueDataCatalogConfig"
      ( \x ->
          GlueDataCatalogConfig'
            Prelude.<$> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "databaseName")
            Prelude.<*> (x Data..: "tablePrefix")
      )

instance Prelude.Hashable GlueDataCatalogConfig where
  hashWithSalt _salt GlueDataCatalogConfig' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tablePrefix

instance Prelude.NFData GlueDataCatalogConfig where
  rnf GlueDataCatalogConfig' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tablePrefix

instance Data.ToJSON GlueDataCatalogConfig where
  toJSON GlueDataCatalogConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("databaseName" Data..= databaseName),
            Prelude.Just ("tablePrefix" Data..= tablePrefix)
          ]
      )
