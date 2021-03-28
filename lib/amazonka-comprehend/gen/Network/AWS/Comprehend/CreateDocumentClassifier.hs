{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classifier that you can use to categorize documents. To create a classifier, you provide a set of training documents that labeled with the categories that you want to use. After the classifier is trained you can use it to categorize a set of labeled documents into the categories. For more information, see 'how-document-classification' .
module Network.AWS.Comprehend.CreateDocumentClassifier
    (
    -- * Creating a request
      CreateDocumentClassifier (..)
    , mkCreateDocumentClassifier
    -- ** Request lenses
    , cdcDocumentClassifierName
    , cdcDataAccessRoleArn
    , cdcInputDataConfig
    , cdcLanguageCode
    , cdcClientRequestToken
    , cdcMode
    , cdcOutputDataConfig
    , cdcTags
    , cdcVolumeKmsKeyId
    , cdcVpcConfig

    -- * Destructuring the response
    , CreateDocumentClassifierResponse (..)
    , mkCreateDocumentClassifierResponse
    -- ** Response lenses
    , cdcrrsDocumentClassifierArn
    , cdcrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { documentClassifierName :: Types.DocumentClassifierName
    -- ^ The name of the document classifier.
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , inputDataConfig :: Types.DocumentClassifierInputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , mode :: Core.Maybe Types.DocumentClassifierMode
    -- ^ Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
  , outputDataConfig :: Core.Maybe Types.DocumentClassifierOutputDataConfig
    -- ^ Enables the addition of output results configuration parameters for custom classifier jobs.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department. 
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDocumentClassifier' value with any optional fields omitted.
mkCreateDocumentClassifier
    :: Types.DocumentClassifierName -- ^ 'documentClassifierName'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.DocumentClassifierInputDataConfig -- ^ 'inputDataConfig'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> CreateDocumentClassifier
mkCreateDocumentClassifier documentClassifierName dataAccessRoleArn
  inputDataConfig languageCode
  = CreateDocumentClassifier'{documentClassifierName,
                              dataAccessRoleArn, inputDataConfig, languageCode,
                              clientRequestToken = Core.Nothing, mode = Core.Nothing,
                              outputDataConfig = Core.Nothing, tags = Core.Nothing,
                              volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | The name of the document classifier.
--
-- /Note:/ Consider using 'documentClassifierName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDocumentClassifierName :: Lens.Lens' CreateDocumentClassifier Types.DocumentClassifierName
cdcDocumentClassifierName = Lens.field @"documentClassifierName"
{-# INLINEABLE cdcDocumentClassifierName #-}
{-# DEPRECATED documentClassifierName "Use generic-lens or generic-optics with 'documentClassifierName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDataAccessRoleArn :: Lens.Lens' CreateDocumentClassifier Types.IamRoleArn
cdcDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE cdcDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcInputDataConfig :: Lens.Lens' CreateDocumentClassifier Types.DocumentClassifierInputDataConfig
cdcInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE cdcInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcLanguageCode :: Lens.Lens' CreateDocumentClassifier Types.LanguageCode
cdcLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE cdcLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcClientRequestToken :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Types.ClientRequestToken)
cdcClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cdcClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcMode :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Types.DocumentClassifierMode)
cdcMode = Lens.field @"mode"
{-# INLINEABLE cdcMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | Enables the addition of output results configuration parameters for custom classifier jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOutputDataConfig :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Types.DocumentClassifierOutputDataConfig)
cdcOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE cdcOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department. 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDocumentClassifier (Core.Maybe [Types.Tag])
cdcTags = Lens.field @"tags"
{-# INLINEABLE cdcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVolumeKmsKeyId :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Types.VolumeKmsKeyId)
cdcVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE cdcVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVpcConfig :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Types.VpcConfig)
cdcVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE cdcVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery CreateDocumentClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDocumentClassifier where
        toHeaders CreateDocumentClassifier{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.CreateDocumentClassifier")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDocumentClassifier where
        toJSON CreateDocumentClassifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("DocumentClassifierName" Core..= documentClassifierName),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Mode" Core..=) Core.<$> mode,
                  ("OutputDataConfig" Core..=) Core.<$> outputDataConfig,
                  ("Tags" Core..=) Core.<$> tags,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest CreateDocumentClassifier where
        type Rs CreateDocumentClassifier = CreateDocumentClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDocumentClassifierResponse' Core.<$>
                   (x Core..:? "DocumentClassifierArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { documentClassifierArn :: Core.Maybe Types.DocumentClassifierArn
    -- ^ The Amazon Resource Name (ARN) that identifies the document classifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDocumentClassifierResponse' value with any optional fields omitted.
mkCreateDocumentClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDocumentClassifierResponse
mkCreateDocumentClassifierResponse responseStatus
  = CreateDocumentClassifierResponse'{documentClassifierArn =
                                        Core.Nothing,
                                      responseStatus}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsDocumentClassifierArn :: Lens.Lens' CreateDocumentClassifierResponse (Core.Maybe Types.DocumentClassifierArn)
cdcrrsDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# INLINEABLE cdcrrsDocumentClassifierArn #-}
{-# DEPRECATED documentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDocumentClassifierResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
