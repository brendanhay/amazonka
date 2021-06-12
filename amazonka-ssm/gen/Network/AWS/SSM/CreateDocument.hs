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
-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Systems Manager (SSM) document. An SSM document defines the
-- actions that Systems Manager performs on your managed instances. For
-- more information about SSM documents, including information about
-- supported schemas, features, and syntax, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-ssm-docs.html AWS Systems Manager Documents>
-- in the /AWS Systems Manager User Guide/.
module Network.AWS.SSM.CreateDocument
  ( -- * Creating a Request
    CreateDocument (..),
    newCreateDocument,

    -- * Request Lenses
    createDocument_documentType,
    createDocument_targetType,
    createDocument_requires,
    createDocument_versionName,
    createDocument_documentFormat,
    createDocument_tags,
    createDocument_attachments,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateDocument' smart constructor.
data CreateDocument = CreateDocument'
  { -- | The type of document to create.
    documentType :: Core.Maybe DocumentType,
    -- | Specify a target type to define the kinds of resources the document can
    -- run on. For example, to run a document on EC2 instances, specify the
    -- following value: \/AWS::EC2::Instance. If you specify a value of \'\/\'
    -- the document can run on all types of resources. If you don\'t specify a
    -- value, the document can\'t run on any resources. For a list of valid
    -- resource types, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
    -- in the /AWS CloudFormation User Guide/.
    targetType :: Core.Maybe Core.Text,
    -- | A list of SSM documents required by a document. This parameter is used
    -- exclusively by AWS AppConfig. When a user creates an AppConfig
    -- configuration in an SSM document, the user must also specify a required
    -- document for validation purposes. In this case, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document for validation purposes. For
    -- more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig>
    -- in the /AWS Systems Manager User Guide/.
    requires :: Core.Maybe (Core.NonEmpty DocumentRequires),
    -- | An optional field specifying the version of the artifact you are
    -- creating with the document. For example, \"Release 12, Update 6\". This
    -- value is unique across all versions of a document, and cannot be
    -- changed.
    versionName :: Core.Maybe Core.Text,
    -- | Specify the document format for the request. The document format can be
    -- JSON, YAML, or TEXT. JSON is the default format.
    documentFormat :: Core.Maybe DocumentFormat,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag an SSM document to
    -- identify the types of targets or the environment where it will run. In
    -- this case, you could specify the following key name\/value pairs:
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- To add tags to an existing SSM document, use the AddTagsToResource
    -- action.
    tags :: Core.Maybe [Tag],
    -- | A list of key and value pairs that describe attachments to a version of
    -- a document.
    attachments :: Core.Maybe [AttachmentsSource],
    -- | The content for the new SSM document in JSON or YAML format. We
    -- recommend storing the contents for your new document in an external JSON
    -- or YAML file and referencing the file in a command.
    --
    -- For examples, see the following topics in the /AWS Systems Manager User
    -- Guide/.
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (AWS API)>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (AWS CLI)>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
    content :: Core.Text,
    -- | A name for the Systems Manager document.
    --
    -- You can\'t use the following strings as document name prefixes. These
    -- are reserved by AWS for use as document name prefixes:
    --
    -- -   @aws-@
    --
    -- -   @amazon@
    --
    -- -   @amzn@
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentType', 'createDocument_documentType' - The type of document to create.
--
-- 'targetType', 'createDocument_targetType' - Specify a target type to define the kinds of resources the document can
-- run on. For example, to run a document on EC2 instances, specify the
-- following value: \/AWS::EC2::Instance. If you specify a value of \'\/\'
-- the document can run on all types of resources. If you don\'t specify a
-- value, the document can\'t run on any resources. For a list of valid
-- resource types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
--
-- 'requires', 'createDocument_requires' - A list of SSM documents required by a document. This parameter is used
-- exclusively by AWS AppConfig. When a user creates an AppConfig
-- configuration in an SSM document, the user must also specify a required
-- document for validation purposes. In this case, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document for validation purposes. For
-- more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig>
-- in the /AWS Systems Manager User Guide/.
--
-- 'versionName', 'createDocument_versionName' - An optional field specifying the version of the artifact you are
-- creating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and cannot be
-- changed.
--
-- 'documentFormat', 'createDocument_documentFormat' - Specify the document format for the request. The document format can be
-- JSON, YAML, or TEXT. JSON is the default format.
--
-- 'tags', 'createDocument_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an SSM document to
-- identify the types of targets or the environment where it will run. In
-- this case, you could specify the following key name\/value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing SSM document, use the AddTagsToResource
-- action.
--
-- 'attachments', 'createDocument_attachments' - A list of key and value pairs that describe attachments to a version of
-- a document.
--
-- 'content', 'createDocument_content' - The content for the new SSM document in JSON or YAML format. We
-- recommend storing the contents for your new document in an external JSON
-- or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /AWS Systems Manager User
-- Guide/.
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (AWS API)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (AWS CLI)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
--
-- 'name', 'createDocument_name' - A name for the Systems Manager document.
--
-- You can\'t use the following strings as document name prefixes. These
-- are reserved by AWS for use as document name prefixes:
--
-- -   @aws-@
--
-- -   @amazon@
--
-- -   @amzn@
newCreateDocument ::
  -- | 'content'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateDocument
newCreateDocument pContent_ pName_ =
  CreateDocument'
    { documentType = Core.Nothing,
      targetType = Core.Nothing,
      requires = Core.Nothing,
      versionName = Core.Nothing,
      documentFormat = Core.Nothing,
      tags = Core.Nothing,
      attachments = Core.Nothing,
      content = pContent_,
      name = pName_
    }

-- | The type of document to create.
createDocument_documentType :: Lens.Lens' CreateDocument (Core.Maybe DocumentType)
createDocument_documentType = Lens.lens (\CreateDocument' {documentType} -> documentType) (\s@CreateDocument' {} a -> s {documentType = a} :: CreateDocument)

-- | Specify a target type to define the kinds of resources the document can
-- run on. For example, to run a document on EC2 instances, specify the
-- following value: \/AWS::EC2::Instance. If you specify a value of \'\/\'
-- the document can run on all types of resources. If you don\'t specify a
-- value, the document can\'t run on any resources. For a list of valid
-- resource types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
createDocument_targetType :: Lens.Lens' CreateDocument (Core.Maybe Core.Text)
createDocument_targetType = Lens.lens (\CreateDocument' {targetType} -> targetType) (\s@CreateDocument' {} a -> s {targetType = a} :: CreateDocument)

-- | A list of SSM documents required by a document. This parameter is used
-- exclusively by AWS AppConfig. When a user creates an AppConfig
-- configuration in an SSM document, the user must also specify a required
-- document for validation purposes. In this case, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document for validation purposes. For
-- more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig>
-- in the /AWS Systems Manager User Guide/.
createDocument_requires :: Lens.Lens' CreateDocument (Core.Maybe (Core.NonEmpty DocumentRequires))
createDocument_requires = Lens.lens (\CreateDocument' {requires} -> requires) (\s@CreateDocument' {} a -> s {requires = a} :: CreateDocument) Core.. Lens.mapping Lens._Coerce

-- | An optional field specifying the version of the artifact you are
-- creating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and cannot be
-- changed.
createDocument_versionName :: Lens.Lens' CreateDocument (Core.Maybe Core.Text)
createDocument_versionName = Lens.lens (\CreateDocument' {versionName} -> versionName) (\s@CreateDocument' {} a -> s {versionName = a} :: CreateDocument)

-- | Specify the document format for the request. The document format can be
-- JSON, YAML, or TEXT. JSON is the default format.
createDocument_documentFormat :: Lens.Lens' CreateDocument (Core.Maybe DocumentFormat)
createDocument_documentFormat = Lens.lens (\CreateDocument' {documentFormat} -> documentFormat) (\s@CreateDocument' {} a -> s {documentFormat = a} :: CreateDocument)

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an SSM document to
-- identify the types of targets or the environment where it will run. In
-- this case, you could specify the following key name\/value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing SSM document, use the AddTagsToResource
-- action.
createDocument_tags :: Lens.Lens' CreateDocument (Core.Maybe [Tag])
createDocument_tags = Lens.lens (\CreateDocument' {tags} -> tags) (\s@CreateDocument' {} a -> s {tags = a} :: CreateDocument) Core.. Lens.mapping Lens._Coerce

-- | A list of key and value pairs that describe attachments to a version of
-- a document.
createDocument_attachments :: Lens.Lens' CreateDocument (Core.Maybe [AttachmentsSource])
createDocument_attachments = Lens.lens (\CreateDocument' {attachments} -> attachments) (\s@CreateDocument' {} a -> s {attachments = a} :: CreateDocument) Core.. Lens.mapping Lens._Coerce

-- | The content for the new SSM document in JSON or YAML format. We
-- recommend storing the contents for your new document in an external JSON
-- or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /AWS Systems Manager User
-- Guide/.
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (AWS API)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (AWS CLI)>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
createDocument_content :: Lens.Lens' CreateDocument Core.Text
createDocument_content = Lens.lens (\CreateDocument' {content} -> content) (\s@CreateDocument' {} a -> s {content = a} :: CreateDocument)

-- | A name for the Systems Manager document.
--
-- You can\'t use the following strings as document name prefixes. These
-- are reserved by AWS for use as document name prefixes:
--
-- -   @aws-@
--
-- -   @amazon@
--
-- -   @amzn@
createDocument_name :: Lens.Lens' CreateDocument Core.Text
createDocument_name = Lens.lens (\CreateDocument' {name} -> name) (\s@CreateDocument' {} a -> s {name = a} :: CreateDocument)

instance Core.AWSRequest CreateDocument where
  type
    AWSResponse CreateDocument =
      CreateDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDocumentResponse'
            Core.<$> (x Core..?> "DocumentDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDocument

instance Core.NFData CreateDocument

instance Core.ToHeaders CreateDocument where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreateDocument" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDocument where
  toJSON CreateDocument' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DocumentType" Core..=) Core.<$> documentType,
            ("TargetType" Core..=) Core.<$> targetType,
            ("Requires" Core..=) Core.<$> requires,
            ("VersionName" Core..=) Core.<$> versionName,
            ("DocumentFormat" Core..=) Core.<$> documentFormat,
            ("Tags" Core..=) Core.<$> tags,
            ("Attachments" Core..=) Core.<$> attachments,
            Core.Just ("Content" Core..= content),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateDocument where
  toPath = Core.const "/"

instance Core.ToQuery CreateDocument where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
  { -- | Information about the Systems Manager document.
    documentDescription :: Core.Maybe DocumentDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentDescription', 'createDocumentResponse_documentDescription' - Information about the Systems Manager document.
--
-- 'httpStatus', 'createDocumentResponse_httpStatus' - The response's http status code.
newCreateDocumentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDocumentResponse
newCreateDocumentResponse pHttpStatus_ =
  CreateDocumentResponse'
    { documentDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Systems Manager document.
createDocumentResponse_documentDescription :: Lens.Lens' CreateDocumentResponse (Core.Maybe DocumentDescription)
createDocumentResponse_documentDescription = Lens.lens (\CreateDocumentResponse' {documentDescription} -> documentDescription) (\s@CreateDocumentResponse' {} a -> s {documentDescription = a} :: CreateDocumentResponse)

-- | The response's http status code.
createDocumentResponse_httpStatus :: Lens.Lens' CreateDocumentResponse Core.Int
createDocumentResponse_httpStatus = Lens.lens (\CreateDocumentResponse' {httpStatus} -> httpStatus) (\s@CreateDocumentResponse' {} a -> s {httpStatus = a} :: CreateDocumentResponse)

instance Core.NFData CreateDocumentResponse
