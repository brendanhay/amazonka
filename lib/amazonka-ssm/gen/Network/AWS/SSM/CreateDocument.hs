{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDocument (..)
    , mkCreateDocument
    -- ** Request lenses
    , cdContent
    , cdName
    , cdAttachments
    , cdDocumentFormat
    , cdDocumentType
    , cdRequires
    , cdTags
    , cdTargetType
    , cdVersionName

    -- * Destructuring the response
    , CreateDocumentResponse (..)
    , mkCreateDocumentResponse
    -- ** Response lenses
    , cdrrsDocumentDescription
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateDocument' smart constructor.
data CreateDocument = CreateDocument'
  { content :: Types.Content
    -- ^ The content for the new SSM document in JSON or YAML format. We recommend storing the contents for your new document in an external JSON or YAML file and referencing the file in a command.
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
  , name :: Types.Name
    -- ^ A name for the Systems Manager document.
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
  , attachments :: Core.Maybe [Types.AttachmentsSource]
    -- ^ A list of key and value pairs that describe attachments to a version of a document.
  , documentFormat :: Core.Maybe Types.DocumentFormat
    -- ^ Specify the document format for the request. The document format can be JSON, YAML, or TEXT. JSON is the default format.
  , documentType :: Core.Maybe Types.DocumentType
    -- ^ The type of document to create.
  , requires :: Core.Maybe (Core.NonEmpty Types.DocumentRequires)
    -- ^ A list of SSM documents required by a document. This parameter is used exclusively by AWS AppConfig. When a user creates an AppConfig configuration in an SSM document, the user must also specify a required document for validation purposes. In this case, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document for validation purposes. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig> in the /AWS Systems Manager User Guide/ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an SSM document to identify the types of targets or the environment where it will run. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=OS,Value=Windows@ 
--
--
--     * @Key=Environment,Value=Production@ 
--
--
  , targetType :: Core.Maybe Types.TargetType
    -- ^ Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ . 
  , versionName :: Core.Maybe Types.VersionName
    -- ^ An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDocument' value with any optional fields omitted.
mkCreateDocument
    :: Types.Content -- ^ 'content'
    -> Types.Name -- ^ 'name'
    -> CreateDocument
mkCreateDocument content name
  = CreateDocument'{content, name, attachments = Core.Nothing,
                    documentFormat = Core.Nothing, documentType = Core.Nothing,
                    requires = Core.Nothing, tags = Core.Nothing,
                    targetType = Core.Nothing, versionName = Core.Nothing}

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
cdContent :: Lens.Lens' CreateDocument Types.Content
cdContent = Lens.field @"content"
{-# INLINEABLE cdContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

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
cdName :: Lens.Lens' CreateDocument Types.Name
cdName = Lens.field @"name"
{-# INLINEABLE cdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of key and value pairs that describe attachments to a version of a document.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAttachments :: Lens.Lens' CreateDocument (Core.Maybe [Types.AttachmentsSource])
cdAttachments = Lens.field @"attachments"
{-# INLINEABLE cdAttachments #-}
{-# DEPRECATED attachments "Use generic-lens or generic-optics with 'attachments' instead"  #-}

-- | Specify the document format for the request. The document format can be JSON, YAML, or TEXT. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDocumentFormat :: Lens.Lens' CreateDocument (Core.Maybe Types.DocumentFormat)
cdDocumentFormat = Lens.field @"documentFormat"
{-# INLINEABLE cdDocumentFormat #-}
{-# DEPRECATED documentFormat "Use generic-lens or generic-optics with 'documentFormat' instead"  #-}

-- | The type of document to create.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDocumentType :: Lens.Lens' CreateDocument (Core.Maybe Types.DocumentType)
cdDocumentType = Lens.field @"documentType"
{-# INLINEABLE cdDocumentType #-}
{-# DEPRECATED documentType "Use generic-lens or generic-optics with 'documentType' instead"  #-}

-- | A list of SSM documents required by a document. This parameter is used exclusively by AWS AppConfig. When a user creates an AppConfig configuration in an SSM document, the user must also specify a required document for validation purposes. In this case, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document for validation purposes. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRequires :: Lens.Lens' CreateDocument (Core.Maybe (Core.NonEmpty Types.DocumentRequires))
cdRequires = Lens.field @"requires"
{-# INLINEABLE cdRequires #-}
{-# DEPRECATED requires "Use generic-lens or generic-optics with 'requires' instead"  #-}

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
cdTags :: Lens.Lens' CreateDocument (Core.Maybe [Types.Tag])
cdTags = Lens.field @"tags"
{-# INLINEABLE cdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ . 
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTargetType :: Lens.Lens' CreateDocument (Core.Maybe Types.TargetType)
cdTargetType = Lens.field @"targetType"
{-# INLINEABLE cdTargetType #-}
{-# DEPRECATED targetType "Use generic-lens or generic-optics with 'targetType' instead"  #-}

-- | An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVersionName :: Lens.Lens' CreateDocument (Core.Maybe Types.VersionName)
cdVersionName = Lens.field @"versionName"
{-# INLINEABLE cdVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

instance Core.ToQuery CreateDocument where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDocument where
        toHeaders CreateDocument{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.CreateDocument") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDocument where
        toJSON CreateDocument{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Content" Core..= content),
                  Core.Just ("Name" Core..= name),
                  ("Attachments" Core..=) Core.<$> attachments,
                  ("DocumentFormat" Core..=) Core.<$> documentFormat,
                  ("DocumentType" Core..=) Core.<$> documentType,
                  ("Requires" Core..=) Core.<$> requires,
                  ("Tags" Core..=) Core.<$> tags,
                  ("TargetType" Core..=) Core.<$> targetType,
                  ("VersionName" Core..=) Core.<$> versionName])

instance Core.AWSRequest CreateDocument where
        type Rs CreateDocument = CreateDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDocumentResponse' Core.<$>
                   (x Core..:? "DocumentDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
  { documentDescription :: Core.Maybe Types.DocumentDescription
    -- ^ Information about the Systems Manager document.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDocumentResponse' value with any optional fields omitted.
mkCreateDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDocumentResponse
mkCreateDocumentResponse responseStatus
  = CreateDocumentResponse'{documentDescription = Core.Nothing,
                            responseStatus}

-- | Information about the Systems Manager document.
--
-- /Note:/ Consider using 'documentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDocumentDescription :: Lens.Lens' CreateDocumentResponse (Core.Maybe Types.DocumentDescription)
cdrrsDocumentDescription = Lens.field @"documentDescription"
{-# INLINEABLE cdrrsDocumentDescription #-}
{-# DEPRECATED documentDescription "Use generic-lens or generic-optics with 'documentDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDocumentResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
