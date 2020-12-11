{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Systems Manager (SSM) document. An SSM document defines the actions that Systems Manager performs on your managed instances. For more information about SSM documents, including information about supported schemas, features, and syntax, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-ssm-docs.html AWS Systems Manager Documents> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.CreateDocument
  ( -- * Creating a request
    CreateDocument (..),
    mkCreateDocument,

    -- ** Request lenses
    cdDocumentType,
    cdAttachments,
    cdVersionName,
    cdTargetType,
    cdDocumentFormat,
    cdRequires,
    cdTags,
    cdContent,
    cdName,

    -- * Destructuring the response
    CreateDocumentResponse (..),
    mkCreateDocumentResponse,

    -- ** Response lenses
    cdrsDocumentDescription,
    cdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateDocument' smart constructor.
data CreateDocument = CreateDocument'
  { documentType ::
      Lude.Maybe DocumentType,
    attachments :: Lude.Maybe [AttachmentsSource],
    versionName :: Lude.Maybe Lude.Text,
    targetType :: Lude.Maybe Lude.Text,
    documentFormat :: Lude.Maybe DocumentFormat,
    requires :: Lude.Maybe (Lude.NonEmpty DocumentRequires),
    tags :: Lude.Maybe [Tag],
    content :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocument' with the minimum fields required to make a request.
--
-- * 'attachments' - A list of key and value pairs that describe attachments to a version of a document.
-- * 'content' - The content for the new SSM document in JSON or YAML format. We recommend storing the contents for your new document in an external JSON or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /AWS Systems Manager User Guide/ .
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (AWS API)>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (AWS CLI)>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
--
--
-- * 'documentFormat' - Specify the document format for the request. The document format can be JSON, YAML, or TEXT. JSON is the default format.
-- * 'documentType' - The type of document to create.
-- * 'name' - A name for the Systems Manager document.
--
-- /Important:/ You can't use the following strings as document name prefixes. These are reserved by AWS for use as document name prefixes:
--
--     * @aws-@
--
--
--     * @amazon@
--
--
--     * @amzn@
--
--
-- * 'requires' - A list of SSM documents required by a document. This parameter is used exclusively by AWS AppConfig. When a user creates an AppConfig configuration in an SSM document, the user must also specify a required document for validation purposes. In this case, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document for validation purposes. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig> in the /AWS Systems Manager User Guide/ .
-- * 'tags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an SSM document to identify the types of targets or the environment where it will run. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
--
--
-- * 'targetType' - Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
-- * 'versionName' - An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
mkCreateDocument ::
  -- | 'content'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateDocument
mkCreateDocument pContent_ pName_ =
  CreateDocument'
    { documentType = Lude.Nothing,
      attachments = Lude.Nothing,
      versionName = Lude.Nothing,
      targetType = Lude.Nothing,
      documentFormat = Lude.Nothing,
      requires = Lude.Nothing,
      tags = Lude.Nothing,
      content = pContent_,
      name = pName_
    }

-- | The type of document to create.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDocumentType :: Lens.Lens' CreateDocument (Lude.Maybe DocumentType)
cdDocumentType = Lens.lens (documentType :: CreateDocument -> Lude.Maybe DocumentType) (\s a -> s {documentType = a} :: CreateDocument)
{-# DEPRECATED cdDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | A list of key and value pairs that describe attachments to a version of a document.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAttachments :: Lens.Lens' CreateDocument (Lude.Maybe [AttachmentsSource])
cdAttachments = Lens.lens (attachments :: CreateDocument -> Lude.Maybe [AttachmentsSource]) (\s a -> s {attachments = a} :: CreateDocument)
{-# DEPRECATED cdAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVersionName :: Lens.Lens' CreateDocument (Lude.Maybe Lude.Text)
cdVersionName = Lens.lens (versionName :: CreateDocument -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: CreateDocument)
{-# DEPRECATED cdVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTargetType :: Lens.Lens' CreateDocument (Lude.Maybe Lude.Text)
cdTargetType = Lens.lens (targetType :: CreateDocument -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: CreateDocument)
{-# DEPRECATED cdTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | Specify the document format for the request. The document format can be JSON, YAML, or TEXT. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDocumentFormat :: Lens.Lens' CreateDocument (Lude.Maybe DocumentFormat)
cdDocumentFormat = Lens.lens (documentFormat :: CreateDocument -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: CreateDocument)
{-# DEPRECATED cdDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | A list of SSM documents required by a document. This parameter is used exclusively by AWS AppConfig. When a user creates an AppConfig configuration in an SSM document, the user must also specify a required document for validation purposes. In this case, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document for validation purposes. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRequires :: Lens.Lens' CreateDocument (Lude.Maybe (Lude.NonEmpty DocumentRequires))
cdRequires = Lens.lens (requires :: CreateDocument -> Lude.Maybe (Lude.NonEmpty DocumentRequires)) (\s a -> s {requires = a} :: CreateDocument)
{-# DEPRECATED cdRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an SSM document to identify the types of targets or the environment where it will run. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDocument (Lude.Maybe [Tag])
cdTags = Lens.lens (tags :: CreateDocument -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDocument)
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The content for the new SSM document in JSON or YAML format. We recommend storing the contents for your new document in an external JSON or YAML file and referencing the file in a command.
--
-- For examples, see the following topics in the /AWS Systems Manager User Guide/ .
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (AWS API)>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-cli.html Create an SSM document (AWS CLI)>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/create-ssm-document-api.html Create an SSM document (API)>
--
--
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContent :: Lens.Lens' CreateDocument Lude.Text
cdContent = Lens.lens (content :: CreateDocument -> Lude.Text) (\s a -> s {content = a} :: CreateDocument)
{-# DEPRECATED cdContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | A name for the Systems Manager document.
--
-- /Important:/ You can't use the following strings as document name prefixes. These are reserved by AWS for use as document name prefixes:
--
--     * @aws-@
--
--
--     * @amazon@
--
--
--     * @amzn@
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDocument Lude.Text
cdName = Lens.lens (name :: CreateDocument -> Lude.Text) (\s a -> s {name = a} :: CreateDocument)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateDocument where
  type Rs CreateDocument = CreateDocumentResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDocumentResponse'
            Lude.<$> (x Lude..?> "DocumentDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDocument where
  toJSON CreateDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DocumentType" Lude..=) Lude.<$> documentType,
            ("Attachments" Lude..=) Lude.<$> attachments,
            ("VersionName" Lude..=) Lude.<$> versionName,
            ("TargetType" Lude..=) Lude.<$> targetType,
            ("DocumentFormat" Lude..=) Lude.<$> documentFormat,
            ("Requires" Lude..=) Lude.<$> requires,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Content" Lude..= content),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
  { documentDescription ::
      Lude.Maybe DocumentDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocumentResponse' with the minimum fields required to make a request.
--
-- * 'documentDescription' - Information about the Systems Manager document.
-- * 'responseStatus' - The response status code.
mkCreateDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDocumentResponse
mkCreateDocumentResponse pResponseStatus_ =
  CreateDocumentResponse'
    { documentDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Systems Manager document.
--
-- /Note:/ Consider using 'documentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDocumentDescription :: Lens.Lens' CreateDocumentResponse (Lude.Maybe DocumentDescription)
cdrsDocumentDescription = Lens.lens (documentDescription :: CreateDocumentResponse -> Lude.Maybe DocumentDescription) (\s a -> s {documentDescription = a} :: CreateDocumentResponse)
{-# DEPRECATED cdrsDocumentDescription "Use generic-lens or generic-optics with 'documentDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDocumentResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDocumentResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
