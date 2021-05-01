{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.UpdateDataCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data catalog that has the specified name.
module Network.AWS.Athena.UpdateDataCatalog
  ( -- * Creating a Request
    UpdateDataCatalog (..),
    newUpdateDataCatalog,

    -- * Request Lenses
    updateDataCatalog_description,
    updateDataCatalog_parameters,
    updateDataCatalog_name,
    updateDataCatalog_type,

    -- * Destructuring the Response
    UpdateDataCatalogResponse (..),
    newUpdateDataCatalogResponse,

    -- * Response Lenses
    updateDataCatalogResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDataCatalog' smart constructor.
data UpdateDataCatalog = UpdateDataCatalog'
  { -- | New or modified text that describes the data catalog.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Lambda function or functions to use for updating the data
    -- catalog. This is a mapping whose values depend on the catalog type.
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
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the data catalog to update. The catalog name must be unique
    -- for the AWS account and can use a maximum of 128 alphanumeric,
    -- underscore, at sign, or hyphen characters.
    name :: Prelude.Text,
    -- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a
    -- federated catalog or @HIVE@ for an external hive metastore.
    --
    -- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
    -- already exists in your account, of which you can have only one.
    -- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
    type' :: DataCatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDataCatalog_description' - New or modified text that describes the data catalog.
--
-- 'parameters', 'updateDataCatalog_parameters' - Specifies the Lambda function or functions to use for updating the data
-- catalog. This is a mapping whose values depend on the catalog type.
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
-- 'name', 'updateDataCatalog_name' - The name of the data catalog to update. The catalog name must be unique
-- for the AWS account and can use a maximum of 128 alphanumeric,
-- underscore, at sign, or hyphen characters.
--
-- 'type'', 'updateDataCatalog_type' - Specifies the type of data catalog to update. Specify @LAMBDA@ for a
-- federated catalog or @HIVE@ for an external hive metastore.
--
-- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
-- already exists in your account, of which you can have only one.
-- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
newUpdateDataCatalog ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataCatalogType ->
  UpdateDataCatalog
newUpdateDataCatalog pName_ pType_ =
  UpdateDataCatalog'
    { description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | New or modified text that describes the data catalog.
updateDataCatalog_description :: Lens.Lens' UpdateDataCatalog (Prelude.Maybe Prelude.Text)
updateDataCatalog_description = Lens.lens (\UpdateDataCatalog' {description} -> description) (\s@UpdateDataCatalog' {} a -> s {description = a} :: UpdateDataCatalog)

-- | Specifies the Lambda function or functions to use for updating the data
-- catalog. This is a mapping whose values depend on the catalog type.
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
updateDataCatalog_parameters :: Lens.Lens' UpdateDataCatalog (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDataCatalog_parameters = Lens.lens (\UpdateDataCatalog' {parameters} -> parameters) (\s@UpdateDataCatalog' {} a -> s {parameters = a} :: UpdateDataCatalog) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the data catalog to update. The catalog name must be unique
-- for the AWS account and can use a maximum of 128 alphanumeric,
-- underscore, at sign, or hyphen characters.
updateDataCatalog_name :: Lens.Lens' UpdateDataCatalog Prelude.Text
updateDataCatalog_name = Lens.lens (\UpdateDataCatalog' {name} -> name) (\s@UpdateDataCatalog' {} a -> s {name = a} :: UpdateDataCatalog)

-- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a
-- federated catalog or @HIVE@ for an external hive metastore.
--
-- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
-- already exists in your account, of which you can have only one.
-- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
updateDataCatalog_type :: Lens.Lens' UpdateDataCatalog DataCatalogType
updateDataCatalog_type = Lens.lens (\UpdateDataCatalog' {type'} -> type') (\s@UpdateDataCatalog' {} a -> s {type' = a} :: UpdateDataCatalog)

instance Prelude.AWSRequest UpdateDataCatalog where
  type Rs UpdateDataCatalog = UpdateDataCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataCatalog

instance Prelude.NFData UpdateDataCatalog

instance Prelude.ToHeaders UpdateDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.UpdateDataCatalog" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDataCatalog where
  toJSON UpdateDataCatalog' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Description" Prelude..=) Prelude.<$> description,
            ("Parameters" Prelude..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )

instance Prelude.ToPath UpdateDataCatalog where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataCatalogResponse' smart constructor.
data UpdateDataCatalogResponse = UpdateDataCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDataCatalogResponse_httpStatus' - The response's http status code.
newUpdateDataCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataCatalogResponse
newUpdateDataCatalogResponse pHttpStatus_ =
  UpdateDataCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDataCatalogResponse_httpStatus :: Lens.Lens' UpdateDataCatalogResponse Prelude.Int
updateDataCatalogResponse_httpStatus = Lens.lens (\UpdateDataCatalogResponse' {httpStatus} -> httpStatus) (\s@UpdateDataCatalogResponse' {} a -> s {httpStatus = a} :: UpdateDataCatalogResponse)

instance Prelude.NFData UpdateDataCatalogResponse
