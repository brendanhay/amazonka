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
-- Module      : Amazonka.Kendra.DescribeDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an Amazon Kendra data source connector.
module Amazonka.Kendra.DescribeDataSource
  ( -- * Creating a Request
    DescribeDataSource (..),
    newDescribeDataSource,

    -- * Request Lenses
    describeDataSource_id,
    describeDataSource_indexId,

    -- * Destructuring the Response
    DescribeDataSourceResponse (..),
    newDescribeDataSourceResponse,

    -- * Response Lenses
    describeDataSourceResponse_schedule,
    describeDataSourceResponse_vpcConfiguration,
    describeDataSourceResponse_customDocumentEnrichmentConfiguration,
    describeDataSourceResponse_indexId,
    describeDataSourceResponse_name,
    describeDataSourceResponse_type,
    describeDataSourceResponse_roleArn,
    describeDataSourceResponse_errorMessage,
    describeDataSourceResponse_configuration,
    describeDataSourceResponse_status,
    describeDataSourceResponse_id,
    describeDataSourceResponse_description,
    describeDataSourceResponse_languageCode,
    describeDataSourceResponse_createdAt,
    describeDataSourceResponse_updatedAt,
    describeDataSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSource' smart constructor.
data DescribeDataSource = DescribeDataSource'
  { -- | The identifier of the data source connector.
    id :: Prelude.Text,
    -- | The identifier of the index used with the data source connector.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeDataSource_id' - The identifier of the data source connector.
--
-- 'indexId', 'describeDataSource_indexId' - The identifier of the index used with the data source connector.
newDescribeDataSource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DescribeDataSource
newDescribeDataSource pId_ pIndexId_ =
  DescribeDataSource' {id = pId_, indexId = pIndexId_}

-- | The identifier of the data source connector.
describeDataSource_id :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_id = Lens.lens (\DescribeDataSource' {id} -> id) (\s@DescribeDataSource' {} a -> s {id = a} :: DescribeDataSource)

-- | The identifier of the index used with the data source connector.
describeDataSource_indexId :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_indexId = Lens.lens (\DescribeDataSource' {indexId} -> indexId) (\s@DescribeDataSource' {} a -> s {indexId = a} :: DescribeDataSource)

instance Core.AWSRequest DescribeDataSource where
  type
    AWSResponse DescribeDataSource =
      DescribeDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourceResponse'
            Prelude.<$> (x Core..?> "Schedule")
            Prelude.<*> (x Core..?> "VpcConfiguration")
            Prelude.<*> (x Core..?> "CustomDocumentEnrichmentConfiguration")
            Prelude.<*> (x Core..?> "IndexId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "Configuration")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSource where
  hashWithSalt _salt DescribeDataSource' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DescribeDataSource where
  rnf DescribeDataSource' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Core.ToHeaders DescribeDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDataSource where
  toJSON DescribeDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DescribeDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSourceResponse' smart constructor.
data DescribeDataSourceResponse = DescribeDataSourceResponse'
  { -- | The schedule for Amazon Kendra to update the index.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your data source. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | Configuration information for altering document metadata and content
    -- during the document ingestion process when you describe a data source.
    --
    -- For more information on how to create, modify and delete document
    -- metadata, or make other content alterations when you ingest documents
    -- into Amazon Kendra, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
    customDocumentEnrichmentConfiguration :: Prelude.Maybe CustomDocumentEnrichmentConfiguration,
    -- | The identifier of the index used with the data source connector.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The name for the data source connector.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the data source. For example, @SHAREPOINT@.
    type' :: Prelude.Maybe DataSourceType,
    -- | The Amazon Resource Name (ARN) of the role with permission to access the
    -- data source and required resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a description of the error that caused the data source to fail.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Configuration details for the data source connector. This shows how the
    -- data source is configured. The configuration options for a data source
    -- depend on the data source provider.
    configuration :: Prelude.Maybe DataSourceConfiguration,
    -- | The current status of the data source connector. When the status is
    -- @ACTIVE@ the data source is ready to use. When the status is @FAILED@,
    -- the @ErrorMessage@ field contains the reason that the data source
    -- failed.
    status :: Prelude.Maybe DataSourceStatus,
    -- | The identifier of the data source connector.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the data source connector.
    description :: Prelude.Maybe Prelude.Text,
    -- | The code for a language. This shows a supported language for all
    -- documents in the data source. English is supported by default. For more
    -- information on supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp of when the data source connector was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix timestamp of when the data source connector was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedule', 'describeDataSourceResponse_schedule' - The schedule for Amazon Kendra to update the index.
--
-- 'vpcConfiguration', 'describeDataSourceResponse_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'customDocumentEnrichmentConfiguration', 'describeDataSourceResponse_customDocumentEnrichmentConfiguration' - Configuration information for altering document metadata and content
-- during the document ingestion process when you describe a data source.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- 'indexId', 'describeDataSourceResponse_indexId' - The identifier of the index used with the data source connector.
--
-- 'name', 'describeDataSourceResponse_name' - The name for the data source connector.
--
-- 'type'', 'describeDataSourceResponse_type' - The type of the data source. For example, @SHAREPOINT@.
--
-- 'roleArn', 'describeDataSourceResponse_roleArn' - The Amazon Resource Name (ARN) of the role with permission to access the
-- data source and required resources.
--
-- 'errorMessage', 'describeDataSourceResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a description of the error that caused the data source to fail.
--
-- 'configuration', 'describeDataSourceResponse_configuration' - Configuration details for the data source connector. This shows how the
-- data source is configured. The configuration options for a data source
-- depend on the data source provider.
--
-- 'status', 'describeDataSourceResponse_status' - The current status of the data source connector. When the status is
-- @ACTIVE@ the data source is ready to use. When the status is @FAILED@,
-- the @ErrorMessage@ field contains the reason that the data source
-- failed.
--
-- 'id', 'describeDataSourceResponse_id' - The identifier of the data source connector.
--
-- 'description', 'describeDataSourceResponse_description' - The description for the data source connector.
--
-- 'languageCode', 'describeDataSourceResponse_languageCode' - The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'describeDataSourceResponse_createdAt' - The Unix timestamp of when the data source connector was created.
--
-- 'updatedAt', 'describeDataSourceResponse_updatedAt' - The Unix timestamp of when the data source connector was last updated.
--
-- 'httpStatus', 'describeDataSourceResponse_httpStatus' - The response's http status code.
newDescribeDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataSourceResponse
newDescribeDataSourceResponse pHttpStatus_ =
  DescribeDataSourceResponse'
    { schedule =
        Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      customDocumentEnrichmentConfiguration =
        Prelude.Nothing,
      indexId = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      configuration = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schedule for Amazon Kendra to update the index.
describeDataSourceResponse_schedule :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_schedule = Lens.lens (\DescribeDataSourceResponse' {schedule} -> schedule) (\s@DescribeDataSourceResponse' {} a -> s {schedule = a} :: DescribeDataSourceResponse)

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your data source. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
describeDataSourceResponse_vpcConfiguration :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceVpcConfiguration)
describeDataSourceResponse_vpcConfiguration = Lens.lens (\DescribeDataSourceResponse' {vpcConfiguration} -> vpcConfiguration) (\s@DescribeDataSourceResponse' {} a -> s {vpcConfiguration = a} :: DescribeDataSourceResponse)

-- | Configuration information for altering document metadata and content
-- during the document ingestion process when you describe a data source.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
describeDataSourceResponse_customDocumentEnrichmentConfiguration :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe CustomDocumentEnrichmentConfiguration)
describeDataSourceResponse_customDocumentEnrichmentConfiguration = Lens.lens (\DescribeDataSourceResponse' {customDocumentEnrichmentConfiguration} -> customDocumentEnrichmentConfiguration) (\s@DescribeDataSourceResponse' {} a -> s {customDocumentEnrichmentConfiguration = a} :: DescribeDataSourceResponse)

-- | The identifier of the index used with the data source connector.
describeDataSourceResponse_indexId :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_indexId = Lens.lens (\DescribeDataSourceResponse' {indexId} -> indexId) (\s@DescribeDataSourceResponse' {} a -> s {indexId = a} :: DescribeDataSourceResponse)

-- | The name for the data source connector.
describeDataSourceResponse_name :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_name = Lens.lens (\DescribeDataSourceResponse' {name} -> name) (\s@DescribeDataSourceResponse' {} a -> s {name = a} :: DescribeDataSourceResponse)

-- | The type of the data source. For example, @SHAREPOINT@.
describeDataSourceResponse_type :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceType)
describeDataSourceResponse_type = Lens.lens (\DescribeDataSourceResponse' {type'} -> type') (\s@DescribeDataSourceResponse' {} a -> s {type' = a} :: DescribeDataSourceResponse)

-- | The Amazon Resource Name (ARN) of the role with permission to access the
-- data source and required resources.
describeDataSourceResponse_roleArn :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_roleArn = Lens.lens (\DescribeDataSourceResponse' {roleArn} -> roleArn) (\s@DescribeDataSourceResponse' {} a -> s {roleArn = a} :: DescribeDataSourceResponse)

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a description of the error that caused the data source to fail.
describeDataSourceResponse_errorMessage :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_errorMessage = Lens.lens (\DescribeDataSourceResponse' {errorMessage} -> errorMessage) (\s@DescribeDataSourceResponse' {} a -> s {errorMessage = a} :: DescribeDataSourceResponse)

-- | Configuration details for the data source connector. This shows how the
-- data source is configured. The configuration options for a data source
-- depend on the data source provider.
describeDataSourceResponse_configuration :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceConfiguration)
describeDataSourceResponse_configuration = Lens.lens (\DescribeDataSourceResponse' {configuration} -> configuration) (\s@DescribeDataSourceResponse' {} a -> s {configuration = a} :: DescribeDataSourceResponse)

-- | The current status of the data source connector. When the status is
-- @ACTIVE@ the data source is ready to use. When the status is @FAILED@,
-- the @ErrorMessage@ field contains the reason that the data source
-- failed.
describeDataSourceResponse_status :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceStatus)
describeDataSourceResponse_status = Lens.lens (\DescribeDataSourceResponse' {status} -> status) (\s@DescribeDataSourceResponse' {} a -> s {status = a} :: DescribeDataSourceResponse)

-- | The identifier of the data source connector.
describeDataSourceResponse_id :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_id = Lens.lens (\DescribeDataSourceResponse' {id} -> id) (\s@DescribeDataSourceResponse' {} a -> s {id = a} :: DescribeDataSourceResponse)

-- | The description for the data source connector.
describeDataSourceResponse_description :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_description = Lens.lens (\DescribeDataSourceResponse' {description} -> description) (\s@DescribeDataSourceResponse' {} a -> s {description = a} :: DescribeDataSourceResponse)

-- | The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
describeDataSourceResponse_languageCode :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_languageCode = Lens.lens (\DescribeDataSourceResponse' {languageCode} -> languageCode) (\s@DescribeDataSourceResponse' {} a -> s {languageCode = a} :: DescribeDataSourceResponse)

-- | The Unix timestamp of when the data source connector was created.
describeDataSourceResponse_createdAt :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeDataSourceResponse_createdAt = Lens.lens (\DescribeDataSourceResponse' {createdAt} -> createdAt) (\s@DescribeDataSourceResponse' {} a -> s {createdAt = a} :: DescribeDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The Unix timestamp of when the data source connector was last updated.
describeDataSourceResponse_updatedAt :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeDataSourceResponse_updatedAt = Lens.lens (\DescribeDataSourceResponse' {updatedAt} -> updatedAt) (\s@DescribeDataSourceResponse' {} a -> s {updatedAt = a} :: DescribeDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeDataSourceResponse_httpStatus :: Lens.Lens' DescribeDataSourceResponse Prelude.Int
describeDataSourceResponse_httpStatus = Lens.lens (\DescribeDataSourceResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSourceResponse' {} a -> s {httpStatus = a} :: DescribeDataSourceResponse)

instance Prelude.NFData DescribeDataSourceResponse where
  rnf DescribeDataSourceResponse' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf customDocumentEnrichmentConfiguration
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
