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
-- Module      : Amazonka.SSM.CreateDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Amazon Web Services Systems Manager (SSM document). An SSM
-- document defines the actions that Systems Manager performs on your
-- managed nodes. For more information about SSM documents, including
-- information about supported schemas, features, and syntax, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-ssm-docs.html Amazon Web Services Systems Manager Documents>
-- in the /Amazon Web Services Systems Manager User Guide/.
module Amazonka.SSM.CreateDocument
  ( -- * Creating a Request
    CreateDocument (..),
    newCreateDocument,

    -- * Request Lenses
    createDocument_attachments,
    createDocument_displayName,
    createDocument_documentFormat,
    createDocument_documentType,
    createDocument_requires,
    createDocument_tags,
    createDocument_targetType,
    createDocument_versionName,
    createDocument_content,
    createDocument_name,

    -- * Destructuring the Response
    CreateDocumentResponse (..),
    newCreateDocumentResponse,

    -- * Response Lenses
    createDocumentResponse_documentDescription,
    createDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateDocument' smart constructor.
data CreateDocument = CreateDocument'
  { -- | A list of key-value pairs that describe attachments to a version of a
    -- document.
    attachments :: Prelude.Maybe [AttachmentsSource],
    -- | An optional field where you can specify a friendly name for the SSM
    -- document. This value can differ for each version of the document. You
    -- can update this value at a later time using the UpdateDocument
    -- operation.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Specify the document format for the request. The document format can be
    -- JSON, YAML, or TEXT. JSON is the default format.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The type of document to create.
    --
    -- The @DeploymentStrategy@ document type is an internal-use-only document
    -- type reserved for AppConfig.
    documentType :: Prelude.Maybe DocumentType,
    -- | A list of SSM documents required by a document. This parameter is used
    -- exclusively by AppConfig. When a user creates an AppConfig configuration
    -- in an SSM document, the user must also specify a required document for
    -- validation purposes. In this case, an @ApplicationConfiguration@
    -- document requires an @ApplicationConfigurationSchema@ document for
    -- validation purposes. For more information, see
    -- <https://docs.aws.amazon.com/appconfig/latest/userguide/what-is-appconfig.html What is AppConfig?>
    -- in the /AppConfig User Guide/.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag an SSM document to
    -- identify the types of targets or the environment where it will run. In
    -- this case, you could specify the following key-value pairs:
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- To add tags to an existing SSM document, use the AddTagsToResource
    -- operation.
    tags :: Prelude.Maybe [Tag],
    -- | Specify a target type to define the kinds of resources the document can
    -- run on. For example, to run a document on EC2 instances, specify the
    -- following value: @\/AWS::EC2::Instance@. If you specify a value of
    -- \'\/\' the document can run on all types of resources. If you don\'t
    -- specify a value, the document can\'t run on any resources. For a list of
    -- valid resource types, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
    -- in the /CloudFormation User Guide/.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact you are
    -- creating with the document. For example, @Release12.1@. This value is
    -- unique across all versions of a document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The content for the new SSM document in JSON or YAML format. We
    -- recommend storing the contents for your new document in an external JSON
    -- or YAML file and referencing the file in a command.
    --
    -- For examples, see the following topics in the /Amazon Web Services
    -- Systems Manager User Guide/.
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (Amazon Web Services API)>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (Amazon Web Services CLI)>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
    content :: Prelude.Text,
    -- | A name for the SSM document.
    --
    -- You can\'t use the following strings as document name prefixes. These
    -- are reserved by Amazon Web Services for use as document name prefixes:
    --
    -- -   @aws@
    --
    -- -   @amazon@
    --
    -- -   @amzn@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'createDocument_attachments' - A list of key-value pairs that describe attachments to a version of a
-- document.
--
-- 'displayName', 'createDocument_displayName' - An optional field where you can specify a friendly name for the SSM
-- document. This value can differ for each version of the document. You
-- can update this value at a later time using the UpdateDocument
-- operation.
--
-- 'documentFormat', 'createDocument_documentFormat' - Specify the document format for the request. The document format can be
-- JSON, YAML, or TEXT. JSON is the default format.
--
-- 'documentType', 'createDocument_documentType' - The type of document to create.
--
-- The @DeploymentStrategy@ document type is an internal-use-only document
-- type reserved for AppConfig.
--
-- 'requires', 'createDocument_requires' - A list of SSM documents required by a document. This parameter is used
-- exclusively by AppConfig. When a user creates an AppConfig configuration
-- in an SSM document, the user must also specify a required document for
-- validation purposes. In this case, an @ApplicationConfiguration@
-- document requires an @ApplicationConfigurationSchema@ document for
-- validation purposes. For more information, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/what-is-appconfig.html What is AppConfig?>
-- in the /AppConfig User Guide/.
--
-- 'tags', 'createDocument_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an SSM document to
-- identify the types of targets or the environment where it will run. In
-- this case, you could specify the following key-value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing SSM document, use the AddTagsToResource
-- operation.
--
-- 'targetType', 'createDocument_targetType' - Specify a target type to define the kinds of resources the document can
-- run on. For example, to run a document on EC2 instances, specify the
-- following value: @\/AWS::EC2::Instance@. If you specify a value of
-- \'\/\' the document can run on all types of resources. If you don\'t
-- specify a value, the document can\'t run on any resources. For a list of
-- valid resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
--
-- 'versionName', 'createDocument_versionName' - An optional field specifying the version of the artifact you are
-- creating with the document. For example, @Release12.1@. This value is
-- unique across all versions of a document, and can\'t be changed.
--
-- 'content', 'createDocument_content' - The content for the new SSM document in JSON or YAML format. We
-- recommend storing the contents for your new document in an external JSON
-- or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /Amazon Web Services
-- Systems Manager User Guide/.
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (Amazon Web Services API)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (Amazon Web Services CLI)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
--
-- 'name', 'createDocument_name' - A name for the SSM document.
--
-- You can\'t use the following strings as document name prefixes. These
-- are reserved by Amazon Web Services for use as document name prefixes:
--
-- -   @aws@
--
-- -   @amazon@
--
-- -   @amzn@
newCreateDocument ::
  -- | 'content'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateDocument
newCreateDocument pContent_ pName_ =
  CreateDocument'
    { attachments = Prelude.Nothing,
      displayName = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentType = Prelude.Nothing,
      requires = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetType = Prelude.Nothing,
      versionName = Prelude.Nothing,
      content = pContent_,
      name = pName_
    }

-- | A list of key-value pairs that describe attachments to a version of a
-- document.
createDocument_attachments :: Lens.Lens' CreateDocument (Prelude.Maybe [AttachmentsSource])
createDocument_attachments = Lens.lens (\CreateDocument' {attachments} -> attachments) (\s@CreateDocument' {} a -> s {attachments = a} :: CreateDocument) Prelude.. Lens.mapping Lens.coerced

-- | An optional field where you can specify a friendly name for the SSM
-- document. This value can differ for each version of the document. You
-- can update this value at a later time using the UpdateDocument
-- operation.
createDocument_displayName :: Lens.Lens' CreateDocument (Prelude.Maybe Prelude.Text)
createDocument_displayName = Lens.lens (\CreateDocument' {displayName} -> displayName) (\s@CreateDocument' {} a -> s {displayName = a} :: CreateDocument)

-- | Specify the document format for the request. The document format can be
-- JSON, YAML, or TEXT. JSON is the default format.
createDocument_documentFormat :: Lens.Lens' CreateDocument (Prelude.Maybe DocumentFormat)
createDocument_documentFormat = Lens.lens (\CreateDocument' {documentFormat} -> documentFormat) (\s@CreateDocument' {} a -> s {documentFormat = a} :: CreateDocument)

-- | The type of document to create.
--
-- The @DeploymentStrategy@ document type is an internal-use-only document
-- type reserved for AppConfig.
createDocument_documentType :: Lens.Lens' CreateDocument (Prelude.Maybe DocumentType)
createDocument_documentType = Lens.lens (\CreateDocument' {documentType} -> documentType) (\s@CreateDocument' {} a -> s {documentType = a} :: CreateDocument)

-- | A list of SSM documents required by a document. This parameter is used
-- exclusively by AppConfig. When a user creates an AppConfig configuration
-- in an SSM document, the user must also specify a required document for
-- validation purposes. In this case, an @ApplicationConfiguration@
-- document requires an @ApplicationConfigurationSchema@ document for
-- validation purposes. For more information, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/what-is-appconfig.html What is AppConfig?>
-- in the /AppConfig User Guide/.
createDocument_requires :: Lens.Lens' CreateDocument (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
createDocument_requires = Lens.lens (\CreateDocument' {requires} -> requires) (\s@CreateDocument' {} a -> s {requires = a} :: CreateDocument) Prelude.. Lens.mapping Lens.coerced

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an SSM document to
-- identify the types of targets or the environment where it will run. In
-- this case, you could specify the following key-value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing SSM document, use the AddTagsToResource
-- operation.
createDocument_tags :: Lens.Lens' CreateDocument (Prelude.Maybe [Tag])
createDocument_tags = Lens.lens (\CreateDocument' {tags} -> tags) (\s@CreateDocument' {} a -> s {tags = a} :: CreateDocument) Prelude.. Lens.mapping Lens.coerced

-- | Specify a target type to define the kinds of resources the document can
-- run on. For example, to run a document on EC2 instances, specify the
-- following value: @\/AWS::EC2::Instance@. If you specify a value of
-- \'\/\' the document can run on all types of resources. If you don\'t
-- specify a value, the document can\'t run on any resources. For a list of
-- valid resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
createDocument_targetType :: Lens.Lens' CreateDocument (Prelude.Maybe Prelude.Text)
createDocument_targetType = Lens.lens (\CreateDocument' {targetType} -> targetType) (\s@CreateDocument' {} a -> s {targetType = a} :: CreateDocument)

-- | An optional field specifying the version of the artifact you are
-- creating with the document. For example, @Release12.1@. This value is
-- unique across all versions of a document, and can\'t be changed.
createDocument_versionName :: Lens.Lens' CreateDocument (Prelude.Maybe Prelude.Text)
createDocument_versionName = Lens.lens (\CreateDocument' {versionName} -> versionName) (\s@CreateDocument' {} a -> s {versionName = a} :: CreateDocument)

-- | The content for the new SSM document in JSON or YAML format. We
-- recommend storing the contents for your new document in an external JSON
-- or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /Amazon Web Services
-- Systems Manager User Guide/.
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (Amazon Web Services API)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (Amazon Web Services CLI)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
createDocument_content :: Lens.Lens' CreateDocument Prelude.Text
createDocument_content = Lens.lens (\CreateDocument' {content} -> content) (\s@CreateDocument' {} a -> s {content = a} :: CreateDocument)

-- | A name for the SSM document.
--
-- You can\'t use the following strings as document name prefixes. These
-- are reserved by Amazon Web Services for use as document name prefixes:
--
-- -   @aws@
--
-- -   @amazon@
--
-- -   @amzn@
createDocument_name :: Lens.Lens' CreateDocument Prelude.Text
createDocument_name = Lens.lens (\CreateDocument' {name} -> name) (\s@CreateDocument' {} a -> s {name = a} :: CreateDocument)

instance Core.AWSRequest CreateDocument where
  type
    AWSResponse CreateDocument =
      CreateDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDocumentResponse'
            Prelude.<$> (x Data..?> "DocumentDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDocument where
  hashWithSalt _salt CreateDocument' {..} =
    _salt
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentType
      `Prelude.hashWithSalt` requires
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDocument where
  rnf CreateDocument' {..} =
    Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf requires
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.CreateDocument" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDocument where
  toJSON CreateDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attachments" Data..=) Prelude.<$> attachments,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("DocumentFormat" Data..=)
              Prelude.<$> documentFormat,
            ("DocumentType" Data..=) Prelude.<$> documentType,
            ("Requires" Data..=) Prelude.<$> requires,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetType" Data..=) Prelude.<$> targetType,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("Content" Data..= content),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
  { -- | Information about the SSM document.
    documentDescription :: Prelude.Maybe DocumentDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentDescription', 'createDocumentResponse_documentDescription' - Information about the SSM document.
--
-- 'httpStatus', 'createDocumentResponse_httpStatus' - The response's http status code.
newCreateDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDocumentResponse
newCreateDocumentResponse pHttpStatus_ =
  CreateDocumentResponse'
    { documentDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the SSM document.
createDocumentResponse_documentDescription :: Lens.Lens' CreateDocumentResponse (Prelude.Maybe DocumentDescription)
createDocumentResponse_documentDescription = Lens.lens (\CreateDocumentResponse' {documentDescription} -> documentDescription) (\s@CreateDocumentResponse' {} a -> s {documentDescription = a} :: CreateDocumentResponse)

-- | The response's http status code.
createDocumentResponse_httpStatus :: Lens.Lens' CreateDocumentResponse Prelude.Int
createDocumentResponse_httpStatus = Lens.lens (\CreateDocumentResponse' {httpStatus} -> httpStatus) (\s@CreateDocumentResponse' {} a -> s {httpStatus = a} :: CreateDocumentResponse)

instance Prelude.NFData CreateDocumentResponse where
  rnf CreateDocumentResponse' {..} =
    Prelude.rnf documentDescription
      `Prelude.seq` Prelude.rnf httpStatus
