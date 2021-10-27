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
-- Module      : Network.AWS.Kendra.CreateDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data source that you want to use with an Amazon Kendra index.
--
-- You specify a name, data source connector type and description for your
-- data source. You also specify configuration information for the data
-- source connector.
--
-- @CreateDataSource@ is a synchronous operation. The operation returns 200
-- if the data source was successfully created. Otherwise, an exception is
-- raised.
module Network.AWS.Kendra.CreateDataSource
  ( -- * Creating a Request
    CreateDataSource (..),
    newCreateDataSource,

    -- * Request Lenses
    createDataSource_languageCode,
    createDataSource_clientToken,
    createDataSource_schedule,
    createDataSource_configuration,
    createDataSource_description,
    createDataSource_tags,
    createDataSource_roleArn,
    createDataSource_name,
    createDataSource_indexId,
    createDataSource_type,

    -- * Destructuring the Response
    CreateDataSourceResponse (..),
    newCreateDataSourceResponse,

    -- * Response Lenses
    createDataSourceResponse_httpStatus,
    createDataSourceResponse_id,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | The code for a language. This allows you to support a language for all
    -- documents when creating the data source. English is supported by
    -- default. For more information on supported languages, including their
    -- codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | A token that you provide to identify the request to create a data
    -- source. Multiple calls to the @CreateDataSource@ operation with the same
    -- client token will create only one data source.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Sets the frequency that Amazon Kendra will check the documents in your
    -- repository and update the index. If you don\'t set a schedule Amazon
    -- Kendra will not periodically update the index. You can call the
    -- @StartDataSourceSyncJob@ operation to update the index.
    --
    -- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
    -- set to @CUSTOM@. If you do, you receive a @ValidationException@
    -- exception.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The connector configuration information that is required to access the
    -- repository.
    --
    -- You can\'t specify the @Configuration@ parameter when the @Type@
    -- parameter is set to @CUSTOM@. If you do, you receive a
    -- @ValidationException@ exception.
    --
    -- The @Configuration@ parameter is required for all other data sources.
    configuration :: Prelude.Maybe DataSourceConfiguration,
    -- | A description for the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify the data source. You can use the
    -- tags to identify and organize your resources and to control access to
    -- resources.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
    --
    -- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
    -- set to @CUSTOM@. If you do, you receive a @ValidationException@
    -- exception.
    --
    -- The @RoleArn@ parameter is required for all other data sources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique name for the data source. A data source name can\'t be changed
    -- without deleting and recreating the data source.
    name :: Prelude.Text,
    -- | The identifier of the index that should be associated with this data
    -- source.
    indexId :: Prelude.Text,
    -- | The type of repository that contains the data source.
    type' :: DataSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'createDataSource_languageCode' - The code for a language. This allows you to support a language for all
-- documents when creating the data source. English is supported by
-- default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'clientToken', 'createDataSource_clientToken' - A token that you provide to identify the request to create a data
-- source. Multiple calls to the @CreateDataSource@ operation with the same
-- client token will create only one data source.
--
-- 'schedule', 'createDataSource_schedule' - Sets the frequency that Amazon Kendra will check the documents in your
-- repository and update the index. If you don\'t set a schedule Amazon
-- Kendra will not periodically update the index. You can call the
-- @StartDataSourceSyncJob@ operation to update the index.
--
-- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- 'configuration', 'createDataSource_configuration' - The connector configuration information that is required to access the
-- repository.
--
-- You can\'t specify the @Configuration@ parameter when the @Type@
-- parameter is set to @CUSTOM@. If you do, you receive a
-- @ValidationException@ exception.
--
-- The @Configuration@ parameter is required for all other data sources.
--
-- 'description', 'createDataSource_description' - A description for the data source.
--
-- 'tags', 'createDataSource_tags' - A list of key-value pairs that identify the data source. You can use the
-- tags to identify and organize your resources and to control access to
-- resources.
--
-- 'roleArn', 'createDataSource_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
--
-- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- The @RoleArn@ parameter is required for all other data sources.
--
-- 'name', 'createDataSource_name' - A unique name for the data source. A data source name can\'t be changed
-- without deleting and recreating the data source.
--
-- 'indexId', 'createDataSource_indexId' - The identifier of the index that should be associated with this data
-- source.
--
-- 'type'', 'createDataSource_type' - The type of repository that contains the data source.
newCreateDataSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  -- | 'type''
  DataSourceType ->
  CreateDataSource
newCreateDataSource pName_ pIndexId_ pType_ =
  CreateDataSource'
    { languageCode = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      schedule = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      name = pName_,
      indexId = pIndexId_,
      type' = pType_
    }

-- | The code for a language. This allows you to support a language for all
-- documents when creating the data source. English is supported by
-- default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
createDataSource_languageCode :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_languageCode = Lens.lens (\CreateDataSource' {languageCode} -> languageCode) (\s@CreateDataSource' {} a -> s {languageCode = a} :: CreateDataSource)

-- | A token that you provide to identify the request to create a data
-- source. Multiple calls to the @CreateDataSource@ operation with the same
-- client token will create only one data source.
createDataSource_clientToken :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_clientToken = Lens.lens (\CreateDataSource' {clientToken} -> clientToken) (\s@CreateDataSource' {} a -> s {clientToken = a} :: CreateDataSource)

-- | Sets the frequency that Amazon Kendra will check the documents in your
-- repository and update the index. If you don\'t set a schedule Amazon
-- Kendra will not periodically update the index. You can call the
-- @StartDataSourceSyncJob@ operation to update the index.
--
-- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
createDataSource_schedule :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_schedule = Lens.lens (\CreateDataSource' {schedule} -> schedule) (\s@CreateDataSource' {} a -> s {schedule = a} :: CreateDataSource)

-- | The connector configuration information that is required to access the
-- repository.
--
-- You can\'t specify the @Configuration@ parameter when the @Type@
-- parameter is set to @CUSTOM@. If you do, you receive a
-- @ValidationException@ exception.
--
-- The @Configuration@ parameter is required for all other data sources.
createDataSource_configuration :: Lens.Lens' CreateDataSource (Prelude.Maybe DataSourceConfiguration)
createDataSource_configuration = Lens.lens (\CreateDataSource' {configuration} -> configuration) (\s@CreateDataSource' {} a -> s {configuration = a} :: CreateDataSource)

-- | A description for the data source.
createDataSource_description :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_description = Lens.lens (\CreateDataSource' {description} -> description) (\s@CreateDataSource' {} a -> s {description = a} :: CreateDataSource)

-- | A list of key-value pairs that identify the data source. You can use the
-- tags to identify and organize your resources and to control access to
-- resources.
createDataSource_tags :: Lens.Lens' CreateDataSource (Prelude.Maybe [Tag])
createDataSource_tags = Lens.lens (\CreateDataSource' {tags} -> tags) (\s@CreateDataSource' {} a -> s {tags = a} :: CreateDataSource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
--
-- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- The @RoleArn@ parameter is required for all other data sources.
createDataSource_roleArn :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_roleArn = Lens.lens (\CreateDataSource' {roleArn} -> roleArn) (\s@CreateDataSource' {} a -> s {roleArn = a} :: CreateDataSource)

-- | A unique name for the data source. A data source name can\'t be changed
-- without deleting and recreating the data source.
createDataSource_name :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_name = Lens.lens (\CreateDataSource' {name} -> name) (\s@CreateDataSource' {} a -> s {name = a} :: CreateDataSource)

-- | The identifier of the index that should be associated with this data
-- source.
createDataSource_indexId :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_indexId = Lens.lens (\CreateDataSource' {indexId} -> indexId) (\s@CreateDataSource' {} a -> s {indexId = a} :: CreateDataSource)

-- | The type of repository that contains the data source.
createDataSource_type :: Lens.Lens' CreateDataSource DataSourceType
createDataSource_type = Lens.lens (\CreateDataSource' {type'} -> type') (\s@CreateDataSource' {} a -> s {type' = a} :: CreateDataSource)

instance Core.AWSRequest CreateDataSource where
  type
    AWSResponse CreateDataSource =
      CreateDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Id")
      )

instance Prelude.Hashable CreateDataSource

instance Prelude.NFData CreateDataSource

instance Core.ToHeaders CreateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.CreateDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataSource where
  toJSON CreateDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath CreateDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the data source.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataSourceResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createDataSourceResponse_id' - A unique identifier for the data source.
newCreateDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  CreateDataSourceResponse
newCreateDataSourceResponse pHttpStatus_ pId_ =
  CreateDataSourceResponse'
    { httpStatus =
        pHttpStatus_,
      id = pId_
    }

-- | The response's http status code.
createDataSourceResponse_httpStatus :: Lens.Lens' CreateDataSourceResponse Prelude.Int
createDataSourceResponse_httpStatus = Lens.lens (\CreateDataSourceResponse' {httpStatus} -> httpStatus) (\s@CreateDataSourceResponse' {} a -> s {httpStatus = a} :: CreateDataSourceResponse)

-- | A unique identifier for the data source.
createDataSourceResponse_id :: Lens.Lens' CreateDataSourceResponse Prelude.Text
createDataSourceResponse_id = Lens.lens (\CreateDataSourceResponse' {id} -> id) (\s@CreateDataSourceResponse' {} a -> s {id = a} :: CreateDataSourceResponse)

instance Prelude.NFData CreateDataSourceResponse
