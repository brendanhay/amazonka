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
-- Module      : Amazonka.Kendra.CreateDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data source connector that you want to use with an Amazon
-- Kendra index.
--
-- You specify a name, data source connector type and description for your
-- data source. You also specify configuration information for the data
-- source connector.
--
-- @CreateDataSource@ is a synchronous operation. The operation returns 200
-- if the data source was successfully created. Otherwise, an exception is
-- raised.
--
-- For an example of creating an index and data source using the Python
-- SDK, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/gs-python.html Getting started with Python SDK>.
-- For an example of creating an index and data source using the Java SDK,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/gs-java.html Getting started with Java SDK>.
module Amazonka.Kendra.CreateDataSource
  ( -- * Creating a Request
    CreateDataSource (..),
    newCreateDataSource,

    -- * Request Lenses
    createDataSource_clientToken,
    createDataSource_configuration,
    createDataSource_customDocumentEnrichmentConfiguration,
    createDataSource_description,
    createDataSource_languageCode,
    createDataSource_roleArn,
    createDataSource_schedule,
    createDataSource_tags,
    createDataSource_vpcConfiguration,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | A token that you provide to identify the request to create a data source
    -- connector. Multiple calls to the @CreateDataSource@ API with the same
    -- client token will create only one data source connector.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Configuration information to connect to your data source repository.
    --
    -- You can\'t specify the @Configuration@ parameter when the @Type@
    -- parameter is set to @CUSTOM@. If you do, you receive a
    -- @ValidationException@ exception.
    --
    -- The @Configuration@ parameter is required for all other data sources.
    configuration :: Prelude.Maybe DataSourceConfiguration,
    -- | Configuration information for altering document metadata and content
    -- during the document ingestion process.
    --
    -- For more information on how to create, modify and delete document
    -- metadata, or make other content alterations when you ingest documents
    -- into Amazon Kendra, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
    customDocumentEnrichmentConfiguration :: Prelude.Maybe CustomDocumentEnrichmentConfiguration,
    -- | A description for the data source connector.
    description :: Prelude.Maybe Prelude.Text,
    -- | The code for a language. This allows you to support a language for all
    -- documents when creating the data source connector. English is supported
    -- by default. For more information on supported languages, including their
    -- codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role with permission to access
    -- the data source and required resources. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra.>.
    --
    -- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
    -- set to @CUSTOM@. If you do, you receive a @ValidationException@
    -- exception.
    --
    -- The @RoleArn@ parameter is required for all other data sources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Sets the frequency for Amazon Kendra to check the documents in your data
    -- source repository and update the index. If you don\'t set a schedule
    -- Amazon Kendra will not periodically update the index. You can call the
    -- @StartDataSourceSyncJob@ API to update the index.
    --
    -- Specify a @cron-@ format schedule string or an empty string to indicate
    -- that the index is updated on demand.
    --
    -- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
    -- set to @CUSTOM@. If you do, you receive a @ValidationException@
    -- exception.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify or categorize the data source
    -- connector. You can also use tags to help control access to the data
    -- source connector. Tag keys and values can consist of Unicode letters,
    -- digits, white space, and any of the following symbols: _ . : \/ = + -
    -- \@.
    tags :: Prelude.Maybe [Tag],
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your data source. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A name for the data source connector.
    name :: Prelude.Text,
    -- | The identifier of the index you want to use with the data source
    -- connector.
    indexId :: Prelude.Text,
    -- | The type of data source repository. For example, @SHAREPOINT@.
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
-- 'clientToken', 'createDataSource_clientToken' - A token that you provide to identify the request to create a data source
-- connector. Multiple calls to the @CreateDataSource@ API with the same
-- client token will create only one data source connector.
--
-- 'configuration', 'createDataSource_configuration' - Configuration information to connect to your data source repository.
--
-- You can\'t specify the @Configuration@ parameter when the @Type@
-- parameter is set to @CUSTOM@. If you do, you receive a
-- @ValidationException@ exception.
--
-- The @Configuration@ parameter is required for all other data sources.
--
-- 'customDocumentEnrichmentConfiguration', 'createDataSource_customDocumentEnrichmentConfiguration' - Configuration information for altering document metadata and content
-- during the document ingestion process.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- 'description', 'createDataSource_description' - A description for the data source connector.
--
-- 'languageCode', 'createDataSource_languageCode' - The code for a language. This allows you to support a language for all
-- documents when creating the data source connector. English is supported
-- by default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'roleArn', 'createDataSource_roleArn' - The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source and required resources. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra.>.
--
-- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- The @RoleArn@ parameter is required for all other data sources.
--
-- 'schedule', 'createDataSource_schedule' - Sets the frequency for Amazon Kendra to check the documents in your data
-- source repository and update the index. If you don\'t set a schedule
-- Amazon Kendra will not periodically update the index. You can call the
-- @StartDataSourceSyncJob@ API to update the index.
--
-- Specify a @cron-@ format schedule string or an empty string to indicate
-- that the index is updated on demand.
--
-- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- 'tags', 'createDataSource_tags' - A list of key-value pairs that identify or categorize the data source
-- connector. You can also use tags to help control access to the data
-- source connector. Tag keys and values can consist of Unicode letters,
-- digits, white space, and any of the following symbols: _ . : \/ = + -
-- \@.
--
-- 'vpcConfiguration', 'createDataSource_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'name', 'createDataSource_name' - A name for the data source connector.
--
-- 'indexId', 'createDataSource_indexId' - The identifier of the index you want to use with the data source
-- connector.
--
-- 'type'', 'createDataSource_type' - The type of data source repository. For example, @SHAREPOINT@.
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
    { clientToken = Prelude.Nothing,
      configuration = Prelude.Nothing,
      customDocumentEnrichmentConfiguration =
        Prelude.Nothing,
      description = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      schedule = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      name = pName_,
      indexId = pIndexId_,
      type' = pType_
    }

-- | A token that you provide to identify the request to create a data source
-- connector. Multiple calls to the @CreateDataSource@ API with the same
-- client token will create only one data source connector.
createDataSource_clientToken :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_clientToken = Lens.lens (\CreateDataSource' {clientToken} -> clientToken) (\s@CreateDataSource' {} a -> s {clientToken = a} :: CreateDataSource)

-- | Configuration information to connect to your data source repository.
--
-- You can\'t specify the @Configuration@ parameter when the @Type@
-- parameter is set to @CUSTOM@. If you do, you receive a
-- @ValidationException@ exception.
--
-- The @Configuration@ parameter is required for all other data sources.
createDataSource_configuration :: Lens.Lens' CreateDataSource (Prelude.Maybe DataSourceConfiguration)
createDataSource_configuration = Lens.lens (\CreateDataSource' {configuration} -> configuration) (\s@CreateDataSource' {} a -> s {configuration = a} :: CreateDataSource)

-- | Configuration information for altering document metadata and content
-- during the document ingestion process.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
createDataSource_customDocumentEnrichmentConfiguration :: Lens.Lens' CreateDataSource (Prelude.Maybe CustomDocumentEnrichmentConfiguration)
createDataSource_customDocumentEnrichmentConfiguration = Lens.lens (\CreateDataSource' {customDocumentEnrichmentConfiguration} -> customDocumentEnrichmentConfiguration) (\s@CreateDataSource' {} a -> s {customDocumentEnrichmentConfiguration = a} :: CreateDataSource)

-- | A description for the data source connector.
createDataSource_description :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_description = Lens.lens (\CreateDataSource' {description} -> description) (\s@CreateDataSource' {} a -> s {description = a} :: CreateDataSource)

-- | The code for a language. This allows you to support a language for all
-- documents when creating the data source connector. English is supported
-- by default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
createDataSource_languageCode :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_languageCode = Lens.lens (\CreateDataSource' {languageCode} -> languageCode) (\s@CreateDataSource' {} a -> s {languageCode = a} :: CreateDataSource)

-- | The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source and required resources. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra.>.
--
-- You can\'t specify the @RoleArn@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
--
-- The @RoleArn@ parameter is required for all other data sources.
createDataSource_roleArn :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_roleArn = Lens.lens (\CreateDataSource' {roleArn} -> roleArn) (\s@CreateDataSource' {} a -> s {roleArn = a} :: CreateDataSource)

-- | Sets the frequency for Amazon Kendra to check the documents in your data
-- source repository and update the index. If you don\'t set a schedule
-- Amazon Kendra will not periodically update the index. You can call the
-- @StartDataSourceSyncJob@ API to update the index.
--
-- Specify a @cron-@ format schedule string or an empty string to indicate
-- that the index is updated on demand.
--
-- You can\'t specify the @Schedule@ parameter when the @Type@ parameter is
-- set to @CUSTOM@. If you do, you receive a @ValidationException@
-- exception.
createDataSource_schedule :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_schedule = Lens.lens (\CreateDataSource' {schedule} -> schedule) (\s@CreateDataSource' {} a -> s {schedule = a} :: CreateDataSource)

-- | A list of key-value pairs that identify or categorize the data source
-- connector. You can also use tags to help control access to the data
-- source connector. Tag keys and values can consist of Unicode letters,
-- digits, white space, and any of the following symbols: _ . : \/ = + -
-- \@.
createDataSource_tags :: Lens.Lens' CreateDataSource (Prelude.Maybe [Tag])
createDataSource_tags = Lens.lens (\CreateDataSource' {tags} -> tags) (\s@CreateDataSource' {} a -> s {tags = a} :: CreateDataSource) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
createDataSource_vpcConfiguration :: Lens.Lens' CreateDataSource (Prelude.Maybe DataSourceVpcConfiguration)
createDataSource_vpcConfiguration = Lens.lens (\CreateDataSource' {vpcConfiguration} -> vpcConfiguration) (\s@CreateDataSource' {} a -> s {vpcConfiguration = a} :: CreateDataSource)

-- | A name for the data source connector.
createDataSource_name :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_name = Lens.lens (\CreateDataSource' {name} -> name) (\s@CreateDataSource' {} a -> s {name = a} :: CreateDataSource)

-- | The identifier of the index you want to use with the data source
-- connector.
createDataSource_indexId :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_indexId = Lens.lens (\CreateDataSource' {indexId} -> indexId) (\s@CreateDataSource' {} a -> s {indexId = a} :: CreateDataSource)

-- | The type of data source repository. For example, @SHAREPOINT@.
createDataSource_type :: Lens.Lens' CreateDataSource DataSourceType
createDataSource_type = Lens.lens (\CreateDataSource' {type'} -> type') (\s@CreateDataSource' {} a -> s {type' = a} :: CreateDataSource)

instance Core.AWSRequest CreateDataSource where
  type
    AWSResponse CreateDataSource =
      CreateDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Id")
      )

instance Prelude.Hashable CreateDataSource where
  hashWithSalt _salt CreateDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` customDocumentEnrichmentConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateDataSource where
  rnf CreateDataSource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf customDocumentEnrichmentConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataSource where
  toJSON CreateDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Configuration" Data..=) Prelude.<$> configuration,
            ("CustomDocumentEnrichmentConfiguration" Data..=)
              Prelude.<$> customDocumentEnrichmentConfiguration,
            ("Description" Data..=) Prelude.<$> description,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateDataSource where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the data source connector.
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
-- 'id', 'createDataSourceResponse_id' - The identifier of the data source connector.
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

-- | The identifier of the data source connector.
createDataSourceResponse_id :: Lens.Lens' CreateDataSourceResponse Prelude.Text
createDataSourceResponse_id = Lens.lens (\CreateDataSourceResponse' {id} -> id) (\s@CreateDataSourceResponse' {} a -> s {id = a} :: CreateDataSourceResponse)

instance Prelude.NFData CreateDataSourceResponse where
  rnf CreateDataSourceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq` Prelude.rnf id
