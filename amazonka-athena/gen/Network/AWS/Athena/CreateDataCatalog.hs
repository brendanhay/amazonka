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
-- Module      : Network.AWS.Athena.CreateDataCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates (registers) a data catalog with the specified name and
-- properties. Catalogs created are visible to all users of the same AWS
-- account.
module Network.AWS.Athena.CreateDataCatalog
  ( -- * Creating a Request
    CreateDataCatalog (..),
    newCreateDataCatalog,

    -- * Request Lenses
    createDataCatalog_tags,
    createDataCatalog_description,
    createDataCatalog_parameters,
    createDataCatalog_name,
    createDataCatalog_type,

    -- * Destructuring the Response
    CreateDataCatalogResponse (..),
    newCreateDataCatalogResponse,

    -- * Response Lenses
    createDataCatalogResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataCatalog' smart constructor.
data CreateDataCatalog = CreateDataCatalog'
  { -- | A list of comma separated tags to add to the data catalog that is
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the data catalog to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Lambda function or functions to use for creating the data
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
    -- | The name of the data catalog to create. The catalog name must be unique
    -- for the AWS account and can use a maximum of 128 alphanumeric,
    -- underscore, at sign, or hyphen characters.
    name :: Prelude.Text,
    -- | The type of data catalog to create: @LAMBDA@ for a federated catalog or
    -- @HIVE@ for an external hive metastore.
    --
    -- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
    -- already exists in your account, of which you can have only one.
    -- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
    type' :: DataCatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDataCatalog_tags' - A list of comma separated tags to add to the data catalog that is
-- created.
--
-- 'description', 'createDataCatalog_description' - A description of the data catalog to be created.
--
-- 'parameters', 'createDataCatalog_parameters' - Specifies the Lambda function or functions to use for creating the data
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
-- 'name', 'createDataCatalog_name' - The name of the data catalog to create. The catalog name must be unique
-- for the AWS account and can use a maximum of 128 alphanumeric,
-- underscore, at sign, or hyphen characters.
--
-- 'type'', 'createDataCatalog_type' - The type of data catalog to create: @LAMBDA@ for a federated catalog or
-- @HIVE@ for an external hive metastore.
--
-- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
-- already exists in your account, of which you can have only one.
-- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
newCreateDataCatalog ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataCatalogType ->
  CreateDataCatalog
newCreateDataCatalog pName_ pType_ =
  CreateDataCatalog'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | A list of comma separated tags to add to the data catalog that is
-- created.
createDataCatalog_tags :: Lens.Lens' CreateDataCatalog (Prelude.Maybe [Tag])
createDataCatalog_tags = Lens.lens (\CreateDataCatalog' {tags} -> tags) (\s@CreateDataCatalog' {} a -> s {tags = a} :: CreateDataCatalog) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the data catalog to be created.
createDataCatalog_description :: Lens.Lens' CreateDataCatalog (Prelude.Maybe Prelude.Text)
createDataCatalog_description = Lens.lens (\CreateDataCatalog' {description} -> description) (\s@CreateDataCatalog' {} a -> s {description = a} :: CreateDataCatalog)

-- | Specifies the Lambda function or functions to use for creating the data
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
createDataCatalog_parameters :: Lens.Lens' CreateDataCatalog (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataCatalog_parameters = Lens.lens (\CreateDataCatalog' {parameters} -> parameters) (\s@CreateDataCatalog' {} a -> s {parameters = a} :: CreateDataCatalog) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the data catalog to create. The catalog name must be unique
-- for the AWS account and can use a maximum of 128 alphanumeric,
-- underscore, at sign, or hyphen characters.
createDataCatalog_name :: Lens.Lens' CreateDataCatalog Prelude.Text
createDataCatalog_name = Lens.lens (\CreateDataCatalog' {name} -> name) (\s@CreateDataCatalog' {} a -> s {name = a} :: CreateDataCatalog)

-- | The type of data catalog to create: @LAMBDA@ for a federated catalog or
-- @HIVE@ for an external hive metastore.
--
-- Do not use the @GLUE@ type. This refers to the @AwsDataCatalog@ that
-- already exists in your account, of which you can have only one.
-- Specifying the @GLUE@ type will result in an @INVALID_INPUT@ error.
createDataCatalog_type :: Lens.Lens' CreateDataCatalog DataCatalogType
createDataCatalog_type = Lens.lens (\CreateDataCatalog' {type'} -> type') (\s@CreateDataCatalog' {} a -> s {type' = a} :: CreateDataCatalog)

instance Core.AWSRequest CreateDataCatalog where
  type
    AWSResponse CreateDataCatalog =
      CreateDataCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDataCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataCatalog

instance Prelude.NFData CreateDataCatalog

instance Core.ToHeaders CreateDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.CreateDataCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataCatalog where
  toJSON CreateDataCatalog' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath CreateDataCatalog where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataCatalogResponse' smart constructor.
data CreateDataCatalogResponse = CreateDataCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataCatalogResponse_httpStatus' - The response's http status code.
newCreateDataCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataCatalogResponse
newCreateDataCatalogResponse pHttpStatus_ =
  CreateDataCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createDataCatalogResponse_httpStatus :: Lens.Lens' CreateDataCatalogResponse Prelude.Int
createDataCatalogResponse_httpStatus = Lens.lens (\CreateDataCatalogResponse' {httpStatus} -> httpStatus) (\s@CreateDataCatalogResponse' {} a -> s {httpStatus = a} :: CreateDataCatalogResponse)

instance Prelude.NFData CreateDataCatalogResponse
