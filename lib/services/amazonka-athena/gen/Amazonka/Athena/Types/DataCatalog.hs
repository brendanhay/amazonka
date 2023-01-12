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
-- Module      : Amazonka.Athena.Types.DataCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.DataCatalog where

import Amazonka.Athena.Types.DataCatalogType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a data catalog in an Amazon Web Services
-- account.
--
-- /See:/ 'newDataCatalog' smart constructor.
data DataCatalog = DataCatalog'
  { -- | An optional description of the data catalog.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Lambda function or functions to use for the data catalog.
    -- This is a mapping whose values depend on the catalog type.
    --
    -- -   For the @HIVE@ data catalog type, use the following syntax. The
    --     @metadata-function@ parameter is required. @The sdk-version@
    --     parameter is optional and defaults to the currently supported
    --     version.
    --
    --     @metadata-function=lambda_arn, sdk-version=version_number @
    --
    -- -   For the @LAMBDA@ data catalog type, use one of the following sets of
    --     required parameters, but not both.
    --
    --     -   If you have one Lambda function that processes metadata and
    --         another for reading the actual data, use the following syntax.
    --         Both parameters are required.
    --
    --         @metadata-function=lambda_arn, record-function=lambda_arn @
    --
    --     -   If you have a composite Lambda function that processes both
    --         metadata and data, use the following syntax to specify your
    --         Lambda function.
    --
    --         @function=lambda_arn @
    --
    -- -   The @GLUE@ type takes a catalog ID parameter and is required. The
    --     @ catalog_id @ is the account ID of the Amazon Web Services account
    --     to which the Glue catalog belongs.
    --
    --     @catalog-id=catalog_id @
    --
    --     -   The @GLUE@ data catalog type also applies to the default
    --         @AwsDataCatalog@ that already exists in your account, of which
    --         you can have only one and cannot modify.
    --
    --     -   Queries that specify a Glue Data Catalog other than the default
    --         @AwsDataCatalog@ must be run on Athena engine version 2.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the data catalog. The catalog name must be unique for the
    -- Amazon Web Services account and can use a maximum of 127 alphanumeric,
    -- underscore, at sign, or hyphen characters. The remainder of the length
    -- constraint of 256 is reserved for use by Athena.
    name :: Prelude.Text,
    -- | The type of data catalog to create: @LAMBDA@ for a federated catalog,
    -- @HIVE@ for an external hive metastore, or @GLUE@ for an Glue Data
    -- Catalog.
    type' :: DataCatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'dataCatalog_description' - An optional description of the data catalog.
--
-- 'parameters', 'dataCatalog_parameters' - Specifies the Lambda function or functions to use for the data catalog.
-- This is a mapping whose values depend on the catalog type.
--
-- -   For the @HIVE@ data catalog type, use the following syntax. The
--     @metadata-function@ parameter is required. @The sdk-version@
--     parameter is optional and defaults to the currently supported
--     version.
--
--     @metadata-function=lambda_arn, sdk-version=version_number @
--
-- -   For the @LAMBDA@ data catalog type, use one of the following sets of
--     required parameters, but not both.
--
--     -   If you have one Lambda function that processes metadata and
--         another for reading the actual data, use the following syntax.
--         Both parameters are required.
--
--         @metadata-function=lambda_arn, record-function=lambda_arn @
--
--     -   If you have a composite Lambda function that processes both
--         metadata and data, use the following syntax to specify your
--         Lambda function.
--
--         @function=lambda_arn @
--
-- -   The @GLUE@ type takes a catalog ID parameter and is required. The
--     @ catalog_id @ is the account ID of the Amazon Web Services account
--     to which the Glue catalog belongs.
--
--     @catalog-id=catalog_id @
--
--     -   The @GLUE@ data catalog type also applies to the default
--         @AwsDataCatalog@ that already exists in your account, of which
--         you can have only one and cannot modify.
--
--     -   Queries that specify a Glue Data Catalog other than the default
--         @AwsDataCatalog@ must be run on Athena engine version 2.
--
-- 'name', 'dataCatalog_name' - The name of the data catalog. The catalog name must be unique for the
-- Amazon Web Services account and can use a maximum of 127 alphanumeric,
-- underscore, at sign, or hyphen characters. The remainder of the length
-- constraint of 256 is reserved for use by Athena.
--
-- 'type'', 'dataCatalog_type' - The type of data catalog to create: @LAMBDA@ for a federated catalog,
-- @HIVE@ for an external hive metastore, or @GLUE@ for an Glue Data
-- Catalog.
newDataCatalog ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataCatalogType ->
  DataCatalog
newDataCatalog pName_ pType_ =
  DataCatalog'
    { description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | An optional description of the data catalog.
dataCatalog_description :: Lens.Lens' DataCatalog (Prelude.Maybe Prelude.Text)
dataCatalog_description = Lens.lens (\DataCatalog' {description} -> description) (\s@DataCatalog' {} a -> s {description = a} :: DataCatalog)

-- | Specifies the Lambda function or functions to use for the data catalog.
-- This is a mapping whose values depend on the catalog type.
--
-- -   For the @HIVE@ data catalog type, use the following syntax. The
--     @metadata-function@ parameter is required. @The sdk-version@
--     parameter is optional and defaults to the currently supported
--     version.
--
--     @metadata-function=lambda_arn, sdk-version=version_number @
--
-- -   For the @LAMBDA@ data catalog type, use one of the following sets of
--     required parameters, but not both.
--
--     -   If you have one Lambda function that processes metadata and
--         another for reading the actual data, use the following syntax.
--         Both parameters are required.
--
--         @metadata-function=lambda_arn, record-function=lambda_arn @
--
--     -   If you have a composite Lambda function that processes both
--         metadata and data, use the following syntax to specify your
--         Lambda function.
--
--         @function=lambda_arn @
--
-- -   The @GLUE@ type takes a catalog ID parameter and is required. The
--     @ catalog_id @ is the account ID of the Amazon Web Services account
--     to which the Glue catalog belongs.
--
--     @catalog-id=catalog_id @
--
--     -   The @GLUE@ data catalog type also applies to the default
--         @AwsDataCatalog@ that already exists in your account, of which
--         you can have only one and cannot modify.
--
--     -   Queries that specify a Glue Data Catalog other than the default
--         @AwsDataCatalog@ must be run on Athena engine version 2.
dataCatalog_parameters :: Lens.Lens' DataCatalog (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dataCatalog_parameters = Lens.lens (\DataCatalog' {parameters} -> parameters) (\s@DataCatalog' {} a -> s {parameters = a} :: DataCatalog) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data catalog. The catalog name must be unique for the
-- Amazon Web Services account and can use a maximum of 127 alphanumeric,
-- underscore, at sign, or hyphen characters. The remainder of the length
-- constraint of 256 is reserved for use by Athena.
dataCatalog_name :: Lens.Lens' DataCatalog Prelude.Text
dataCatalog_name = Lens.lens (\DataCatalog' {name} -> name) (\s@DataCatalog' {} a -> s {name = a} :: DataCatalog)

-- | The type of data catalog to create: @LAMBDA@ for a federated catalog,
-- @HIVE@ for an external hive metastore, or @GLUE@ for an Glue Data
-- Catalog.
dataCatalog_type :: Lens.Lens' DataCatalog DataCatalogType
dataCatalog_type = Lens.lens (\DataCatalog' {type'} -> type') (\s@DataCatalog' {} a -> s {type' = a} :: DataCatalog)

instance Data.FromJSON DataCatalog where
  parseJSON =
    Data.withObject
      "DataCatalog"
      ( \x ->
          DataCatalog'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable DataCatalog where
  hashWithSalt _salt DataCatalog' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataCatalog where
  rnf DataCatalog' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
