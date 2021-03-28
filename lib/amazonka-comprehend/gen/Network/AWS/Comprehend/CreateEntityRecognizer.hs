{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entity recognizer using submitted files. After your @CreateEntityRecognizer@ request is submitted, you can check job status using the API. 
module Network.AWS.Comprehend.CreateEntityRecognizer
    (
    -- * Creating a request
      CreateEntityRecognizer (..)
    , mkCreateEntityRecognizer
    -- ** Request lenses
    , cerRecognizerName
    , cerDataAccessRoleArn
    , cerInputDataConfig
    , cerLanguageCode
    , cerClientRequestToken
    , cerTags
    , cerVolumeKmsKeyId
    , cerVpcConfig

    -- * Destructuring the response
    , CreateEntityRecognizerResponse (..)
    , mkCreateEntityRecognizerResponse
    -- ** Response lenses
    , cerrrsEntityRecognizerArn
    , cerrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEntityRecognizer' smart constructor.
data CreateEntityRecognizer = CreateEntityRecognizer'
  { recognizerName :: Types.RecognizerName
    -- ^ The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , inputDataConfig :: Types.EntityRecognizerInputDataConfig
    -- ^ Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created. 
  , languageCode :: Types.LanguageCode
    -- ^ You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department. 
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
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEntityRecognizer' value with any optional fields omitted.
mkCreateEntityRecognizer
    :: Types.RecognizerName -- ^ 'recognizerName'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.EntityRecognizerInputDataConfig -- ^ 'inputDataConfig'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> CreateEntityRecognizer
mkCreateEntityRecognizer recognizerName dataAccessRoleArn
  inputDataConfig languageCode
  = CreateEntityRecognizer'{recognizerName, dataAccessRoleArn,
                            inputDataConfig, languageCode, clientRequestToken = Core.Nothing,
                            tags = Core.Nothing, volumeKmsKeyId = Core.Nothing,
                            vpcConfig = Core.Nothing}

-- | The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
--
-- /Note:/ Consider using 'recognizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerRecognizerName :: Lens.Lens' CreateEntityRecognizer Types.RecognizerName
cerRecognizerName = Lens.field @"recognizerName"
{-# INLINEABLE cerRecognizerName #-}
{-# DEPRECATED recognizerName "Use generic-lens or generic-optics with 'recognizerName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerDataAccessRoleArn :: Lens.Lens' CreateEntityRecognizer Types.IamRoleArn
cerDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE cerDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created. 
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerInputDataConfig :: Lens.Lens' CreateEntityRecognizer Types.EntityRecognizerInputDataConfig
cerInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE cerInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerLanguageCode :: Lens.Lens' CreateEntityRecognizer Types.LanguageCode
cerLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE cerLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerClientRequestToken :: Lens.Lens' CreateEntityRecognizer (Core.Maybe Types.ClientRequestToken)
cerClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cerClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department. 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerTags :: Lens.Lens' CreateEntityRecognizer (Core.Maybe [Types.Tag])
cerTags = Lens.field @"tags"
{-# INLINEABLE cerTags #-}
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
cerVolumeKmsKeyId :: Lens.Lens' CreateEntityRecognizer (Core.Maybe Types.VolumeKmsKeyId)
cerVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE cerVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerVpcConfig :: Lens.Lens' CreateEntityRecognizer (Core.Maybe Types.VpcConfig)
cerVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE cerVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery CreateEntityRecognizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEntityRecognizer where
        toHeaders CreateEntityRecognizer{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.CreateEntityRecognizer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEntityRecognizer where
        toJSON CreateEntityRecognizer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RecognizerName" Core..= recognizerName),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Tags" Core..=) Core.<$> tags,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest CreateEntityRecognizer where
        type Rs CreateEntityRecognizer = CreateEntityRecognizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEntityRecognizerResponse' Core.<$>
                   (x Core..:? "EntityRecognizerArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEntityRecognizerResponse' smart constructor.
data CreateEntityRecognizerResponse = CreateEntityRecognizerResponse'
  { entityRecognizerArn :: Core.Maybe Types.EntityRecognizerArn
    -- ^ The Amazon Resource Name (ARN) that identifies the entity recognizer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEntityRecognizerResponse' value with any optional fields omitted.
mkCreateEntityRecognizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateEntityRecognizerResponse
mkCreateEntityRecognizerResponse responseStatus
  = CreateEntityRecognizerResponse'{entityRecognizerArn =
                                      Core.Nothing,
                                    responseStatus}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrrsEntityRecognizerArn :: Lens.Lens' CreateEntityRecognizerResponse (Core.Maybe Types.EntityRecognizerArn)
cerrrsEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# INLINEABLE cerrrsEntityRecognizerArn #-}
{-# DEPRECATED entityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrrsResponseStatus :: Lens.Lens' CreateEntityRecognizerResponse Core.Int
cerrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cerrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
