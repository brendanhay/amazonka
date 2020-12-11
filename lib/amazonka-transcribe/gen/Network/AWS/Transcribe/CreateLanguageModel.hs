{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.CreateLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom language model. Use Amazon S3 prefixes to provide the location of your input files. The time it takes to create your model depends on the size of your training data.
module Network.AWS.Transcribe.CreateLanguageModel
  ( -- * Creating a request
    CreateLanguageModel (..),
    mkCreateLanguageModel,

    -- ** Request lenses
    clmLanguageCode,
    clmBaseModelName,
    clmModelName,
    clmInputDataConfig,

    -- * Destructuring the response
    CreateLanguageModelResponse (..),
    mkCreateLanguageModelResponse,

    -- ** Response lenses
    clmrsLanguageCode,
    clmrsModelName,
    clmrsInputDataConfig,
    clmrsBaseModelName,
    clmrsModelStatus,
    clmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkCreateLanguageModel' smart constructor.
data CreateLanguageModel = CreateLanguageModel'
  { languageCode ::
      CLMLanguageCode,
    baseModelName :: BaseModelName,
    modelName :: Lude.Text,
    inputDataConfig :: InputDataConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLanguageModel' with the minimum fields required to make a request.
--
-- * 'baseModelName' - The Amazon Transcribe standard language model, or base model used to create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ .
-- If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
-- * 'inputDataConfig' - Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
-- * 'languageCode' - The language of the input text you're using to train your custom language model.
-- * 'modelName' - The name you choose for your custom language model when you create it.
mkCreateLanguageModel ::
  -- | 'languageCode'
  CLMLanguageCode ->
  -- | 'baseModelName'
  BaseModelName ->
  -- | 'modelName'
  Lude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  CreateLanguageModel
mkCreateLanguageModel
  pLanguageCode_
  pBaseModelName_
  pModelName_
  pInputDataConfig_ =
    CreateLanguageModel'
      { languageCode = pLanguageCode_,
        baseModelName = pBaseModelName_,
        modelName = pModelName_,
        inputDataConfig = pInputDataConfig_
      }

-- | The language of the input text you're using to train your custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageCode :: Lens.Lens' CreateLanguageModel CLMLanguageCode
clmLanguageCode = Lens.lens (languageCode :: CreateLanguageModel -> CLMLanguageCode) (\s a -> s {languageCode = a} :: CreateLanguageModel)
{-# DEPRECATED clmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon Transcribe standard language model, or base model used to create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ .
-- If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmBaseModelName :: Lens.Lens' CreateLanguageModel BaseModelName
clmBaseModelName = Lens.lens (baseModelName :: CreateLanguageModel -> BaseModelName) (\s a -> s {baseModelName = a} :: CreateLanguageModel)
{-# DEPRECATED clmBaseModelName "Use generic-lens or generic-optics with 'baseModelName' instead." #-}

-- | The name you choose for your custom language model when you create it.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmModelName :: Lens.Lens' CreateLanguageModel Lude.Text
clmModelName = Lens.lens (modelName :: CreateLanguageModel -> Lude.Text) (\s a -> s {modelName = a} :: CreateLanguageModel)
{-# DEPRECATED clmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmInputDataConfig :: Lens.Lens' CreateLanguageModel InputDataConfig
clmInputDataConfig = Lens.lens (inputDataConfig :: CreateLanguageModel -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: CreateLanguageModel)
{-# DEPRECATED clmInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

instance Lude.AWSRequest CreateLanguageModel where
  type Rs CreateLanguageModel = CreateLanguageModelResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLanguageModelResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "ModelName")
            Lude.<*> (x Lude..?> "InputDataConfig")
            Lude.<*> (x Lude..?> "BaseModelName")
            Lude.<*> (x Lude..?> "ModelStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLanguageModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.CreateLanguageModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLanguageModel where
  toJSON CreateLanguageModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("BaseModelName" Lude..= baseModelName),
            Lude.Just ("ModelName" Lude..= modelName),
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig)
          ]
      )

instance Lude.ToPath CreateLanguageModel where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLanguageModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLanguageModelResponse' smart constructor.
data CreateLanguageModelResponse = CreateLanguageModelResponse'
  { languageCode ::
      Lude.Maybe CLMLanguageCode,
    modelName :: Lude.Maybe Lude.Text,
    inputDataConfig ::
      Lude.Maybe InputDataConfig,
    baseModelName ::
      Lude.Maybe BaseModelName,
    modelStatus ::
      Lude.Maybe ModelStatus,
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

-- | Creates a value of 'CreateLanguageModelResponse' with the minimum fields required to make a request.
--
-- * 'baseModelName' - The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
-- * 'inputDataConfig' - The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
-- * 'languageCode' - The language code of the text you've used to create a custom language model.
-- * 'modelName' - The name you've chosen for your custom language model.
-- * 'modelStatus' - The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
-- * 'responseStatus' - The response status code.
mkCreateLanguageModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLanguageModelResponse
mkCreateLanguageModelResponse pResponseStatus_ =
  CreateLanguageModelResponse'
    { languageCode = Lude.Nothing,
      modelName = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      baseModelName = Lude.Nothing,
      modelStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code of the text you've used to create a custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsLanguageCode :: Lens.Lens' CreateLanguageModelResponse (Lude.Maybe CLMLanguageCode)
clmrsLanguageCode = Lens.lens (languageCode :: CreateLanguageModelResponse -> Lude.Maybe CLMLanguageCode) (\s a -> s {languageCode = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name you've chosen for your custom language model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsModelName :: Lens.Lens' CreateLanguageModelResponse (Lude.Maybe Lude.Text)
clmrsModelName = Lens.lens (modelName :: CreateLanguageModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {modelName = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsInputDataConfig :: Lens.Lens' CreateLanguageModelResponse (Lude.Maybe InputDataConfig)
clmrsInputDataConfig = Lens.lens (inputDataConfig :: CreateLanguageModelResponse -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsBaseModelName :: Lens.Lens' CreateLanguageModelResponse (Lude.Maybe BaseModelName)
clmrsBaseModelName = Lens.lens (baseModelName :: CreateLanguageModelResponse -> Lude.Maybe BaseModelName) (\s a -> s {baseModelName = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsBaseModelName "Use generic-lens or generic-optics with 'baseModelName' instead." #-}

-- | The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
--
-- /Note:/ Consider using 'modelStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsModelStatus :: Lens.Lens' CreateLanguageModelResponse (Lude.Maybe ModelStatus)
clmrsModelStatus = Lens.lens (modelStatus :: CreateLanguageModelResponse -> Lude.Maybe ModelStatus) (\s a -> s {modelStatus = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsModelStatus "Use generic-lens or generic-optics with 'modelStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrsResponseStatus :: Lens.Lens' CreateLanguageModelResponse Lude.Int
clmrsResponseStatus = Lens.lens (responseStatus :: CreateLanguageModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLanguageModelResponse)
{-# DEPRECATED clmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
