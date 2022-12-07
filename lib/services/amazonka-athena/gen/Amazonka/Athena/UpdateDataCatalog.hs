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
-- Module      : Amazonka.Athena.UpdateDataCatalog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data catalog that has the specified name.
module Amazonka.Athena.UpdateDataCatalog
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

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- for the Amazon Web Services account and can use a maximum of 127
    -- alphanumeric, underscore, at sign, or hyphen characters. The remainder
    -- of the length constraint of 256 is reserved for use by Athena.
    name :: Prelude.Text,
    -- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a
    -- federated catalog, @HIVE@ for an external hive metastore, or @GLUE@ for
    -- an Glue Data Catalog.
    type' :: DataCatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- for the Amazon Web Services account and can use a maximum of 127
-- alphanumeric, underscore, at sign, or hyphen characters. The remainder
-- of the length constraint of 256 is reserved for use by Athena.
--
-- 'type'', 'updateDataCatalog_type' - Specifies the type of data catalog to update. Specify @LAMBDA@ for a
-- federated catalog, @HIVE@ for an external hive metastore, or @GLUE@ for
-- an Glue Data Catalog.
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
updateDataCatalog_parameters = Lens.lens (\UpdateDataCatalog' {parameters} -> parameters) (\s@UpdateDataCatalog' {} a -> s {parameters = a} :: UpdateDataCatalog) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data catalog to update. The catalog name must be unique
-- for the Amazon Web Services account and can use a maximum of 127
-- alphanumeric, underscore, at sign, or hyphen characters. The remainder
-- of the length constraint of 256 is reserved for use by Athena.
updateDataCatalog_name :: Lens.Lens' UpdateDataCatalog Prelude.Text
updateDataCatalog_name = Lens.lens (\UpdateDataCatalog' {name} -> name) (\s@UpdateDataCatalog' {} a -> s {name = a} :: UpdateDataCatalog)

-- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a
-- federated catalog, @HIVE@ for an external hive metastore, or @GLUE@ for
-- an Glue Data Catalog.
updateDataCatalog_type :: Lens.Lens' UpdateDataCatalog DataCatalogType
updateDataCatalog_type = Lens.lens (\UpdateDataCatalog' {type'} -> type') (\s@UpdateDataCatalog' {} a -> s {type' = a} :: UpdateDataCatalog)

instance Core.AWSRequest UpdateDataCatalog where
  type
    AWSResponse UpdateDataCatalog =
      UpdateDataCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataCatalog where
  hashWithSalt _salt UpdateDataCatalog' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UpdateDataCatalog where
  rnf UpdateDataCatalog' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders UpdateDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateDataCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataCatalog where
  toJSON UpdateDataCatalog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath UpdateDataCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataCatalogResponse' smart constructor.
data UpdateDataCatalogResponse = UpdateDataCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateDataCatalogResponse where
  rnf UpdateDataCatalogResponse' {..} =
    Prelude.rnf httpStatus
