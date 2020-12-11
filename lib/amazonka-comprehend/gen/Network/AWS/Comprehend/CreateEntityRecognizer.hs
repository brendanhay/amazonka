{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateEntityRecognizer (..),
    mkCreateEntityRecognizer,

    -- ** Request lenses
    cerVPCConfig,
    cerVolumeKMSKeyId,
    cerClientRequestToken,
    cerTags,
    cerRecognizerName,
    cerDataAccessRoleARN,
    cerInputDataConfig,
    cerLanguageCode,

    -- * Destructuring the response
    CreateEntityRecognizerResponse (..),
    mkCreateEntityRecognizerResponse,

    -- ** Response lenses
    cerrsEntityRecognizerARN,
    cerrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEntityRecognizer' smart constructor.
data CreateEntityRecognizer = CreateEntityRecognizer'
  { vpcConfig ::
      Lude.Maybe VPCConfig,
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    recognizerName :: Lude.Text,
    dataAccessRoleARN :: Lude.Text,
    inputDataConfig ::
      EntityRecognizerInputDataConfig,
    languageCode :: LanguageCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEntityRecognizer' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'inputDataConfig' - Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
-- * 'languageCode' - You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
-- * 'recognizerName' - The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
-- * 'tags' - Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkCreateEntityRecognizer ::
  -- | 'recognizerName'
  Lude.Text ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'inputDataConfig'
  EntityRecognizerInputDataConfig ->
  -- | 'languageCode'
  LanguageCode ->
  CreateEntityRecognizer
mkCreateEntityRecognizer
  pRecognizerName_
  pDataAccessRoleARN_
  pInputDataConfig_
  pLanguageCode_ =
    CreateEntityRecognizer'
      { vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        tags = Lude.Nothing,
        recognizerName = pRecognizerName_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        inputDataConfig = pInputDataConfig_,
        languageCode = pLanguageCode_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerVPCConfig :: Lens.Lens' CreateEntityRecognizer (Lude.Maybe VPCConfig)
cerVPCConfig = Lens.lens (vpcConfig :: CreateEntityRecognizer -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerVolumeKMSKeyId :: Lens.Lens' CreateEntityRecognizer (Lude.Maybe Lude.Text)
cerVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: CreateEntityRecognizer -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerClientRequestToken :: Lens.Lens' CreateEntityRecognizer (Lude.Maybe Lude.Text)
cerClientRequestToken = Lens.lens (clientRequestToken :: CreateEntityRecognizer -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerTags :: Lens.Lens' CreateEntityRecognizer (Lude.Maybe [Tag])
cerTags = Lens.lens (tags :: CreateEntityRecognizer -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
--
-- /Note:/ Consider using 'recognizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerRecognizerName :: Lens.Lens' CreateEntityRecognizer Lude.Text
cerRecognizerName = Lens.lens (recognizerName :: CreateEntityRecognizer -> Lude.Text) (\s a -> s {recognizerName = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerRecognizerName "Use generic-lens or generic-optics with 'recognizerName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerDataAccessRoleARN :: Lens.Lens' CreateEntityRecognizer Lude.Text
cerDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: CreateEntityRecognizer -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerInputDataConfig :: Lens.Lens' CreateEntityRecognizer EntityRecognizerInputDataConfig
cerInputDataConfig = Lens.lens (inputDataConfig :: CreateEntityRecognizer -> EntityRecognizerInputDataConfig) (\s a -> s {inputDataConfig = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerLanguageCode :: Lens.Lens' CreateEntityRecognizer LanguageCode
cerLanguageCode = Lens.lens (languageCode :: CreateEntityRecognizer -> LanguageCode) (\s a -> s {languageCode = a} :: CreateEntityRecognizer)
{-# DEPRECATED cerLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest CreateEntityRecognizer where
  type Rs CreateEntityRecognizer = CreateEntityRecognizerResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEntityRecognizerResponse'
            Lude.<$> (x Lude..?> "EntityRecognizerArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEntityRecognizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.CreateEntityRecognizer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEntityRecognizer where
  toJSON CreateEntityRecognizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RecognizerName" Lude..= recognizerName),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath CreateEntityRecognizer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEntityRecognizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEntityRecognizerResponse' smart constructor.
data CreateEntityRecognizerResponse = CreateEntityRecognizerResponse'
  { entityRecognizerARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
-- * 'responseStatus' - The response status code.
mkCreateEntityRecognizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEntityRecognizerResponse
mkCreateEntityRecognizerResponse pResponseStatus_ =
  CreateEntityRecognizerResponse'
    { entityRecognizerARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEntityRecognizerARN :: Lens.Lens' CreateEntityRecognizerResponse (Lude.Maybe Lude.Text)
cerrsEntityRecognizerARN = Lens.lens (entityRecognizerARN :: CreateEntityRecognizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {entityRecognizerARN = a} :: CreateEntityRecognizerResponse)
{-# DEPRECATED cerrsEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEntityRecognizerResponse Lude.Int
cerrsResponseStatus = Lens.lens (responseStatus :: CreateEntityRecognizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEntityRecognizerResponse)
{-# DEPRECATED cerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
