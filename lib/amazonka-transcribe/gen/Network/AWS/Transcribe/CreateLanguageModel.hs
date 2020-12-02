{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    createLanguageModel,
    CreateLanguageModel,

    -- * Request Lenses
    clmLanguageCode,
    clmBaseModelName,
    clmModelName,
    clmInputDataConfig,

    -- * Destructuring the Response
    createLanguageModelResponse,
    CreateLanguageModelResponse,

    -- * Response Lenses
    clmrsLanguageCode,
    clmrsModelName,
    clmrsInputDataConfig,
    clmrsBaseModelName,
    clmrsModelStatus,
    clmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'createLanguageModel' smart constructor.
data CreateLanguageModel = CreateLanguageModel'
  { _clmLanguageCode ::
      !CLMLanguageCode,
    _clmBaseModelName :: !BaseModelName,
    _clmModelName :: !Text,
    _clmInputDataConfig :: !InputDataConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLanguageModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmLanguageCode' - The language of the input text you're using to train your custom language model.
--
-- * 'clmBaseModelName' - The Amazon Transcribe standard language model, or base model used to create your custom language model. If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ . If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
--
-- * 'clmModelName' - The name you choose for your custom language model when you create it.
--
-- * 'clmInputDataConfig' - Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
createLanguageModel ::
  -- | 'clmLanguageCode'
  CLMLanguageCode ->
  -- | 'clmBaseModelName'
  BaseModelName ->
  -- | 'clmModelName'
  Text ->
  -- | 'clmInputDataConfig'
  InputDataConfig ->
  CreateLanguageModel
createLanguageModel
  pLanguageCode_
  pBaseModelName_
  pModelName_
  pInputDataConfig_ =
    CreateLanguageModel'
      { _clmLanguageCode = pLanguageCode_,
        _clmBaseModelName = pBaseModelName_,
        _clmModelName = pModelName_,
        _clmInputDataConfig = pInputDataConfig_
      }

-- | The language of the input text you're using to train your custom language model.
clmLanguageCode :: Lens' CreateLanguageModel CLMLanguageCode
clmLanguageCode = lens _clmLanguageCode (\s a -> s {_clmLanguageCode = a})

-- | The Amazon Transcribe standard language model, or base model used to create your custom language model. If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ . If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
clmBaseModelName :: Lens' CreateLanguageModel BaseModelName
clmBaseModelName = lens _clmBaseModelName (\s a -> s {_clmBaseModelName = a})

-- | The name you choose for your custom language model when you create it.
clmModelName :: Lens' CreateLanguageModel Text
clmModelName = lens _clmModelName (\s a -> s {_clmModelName = a})

-- | Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
clmInputDataConfig :: Lens' CreateLanguageModel InputDataConfig
clmInputDataConfig = lens _clmInputDataConfig (\s a -> s {_clmInputDataConfig = a})

instance AWSRequest CreateLanguageModel where
  type Rs CreateLanguageModel = CreateLanguageModelResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          CreateLanguageModelResponse'
            <$> (x .?> "LanguageCode")
            <*> (x .?> "ModelName")
            <*> (x .?> "InputDataConfig")
            <*> (x .?> "BaseModelName")
            <*> (x .?> "ModelStatus")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateLanguageModel

instance NFData CreateLanguageModel

instance ToHeaders CreateLanguageModel where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.CreateLanguageModel" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateLanguageModel where
  toJSON CreateLanguageModel' {..} =
    object
      ( catMaybes
          [ Just ("LanguageCode" .= _clmLanguageCode),
            Just ("BaseModelName" .= _clmBaseModelName),
            Just ("ModelName" .= _clmModelName),
            Just ("InputDataConfig" .= _clmInputDataConfig)
          ]
      )

instance ToPath CreateLanguageModel where
  toPath = const "/"

instance ToQuery CreateLanguageModel where
  toQuery = const mempty

-- | /See:/ 'createLanguageModelResponse' smart constructor.
data CreateLanguageModelResponse = CreateLanguageModelResponse'
  { _clmrsLanguageCode ::
      !(Maybe CLMLanguageCode),
    _clmrsModelName :: !(Maybe Text),
    _clmrsInputDataConfig ::
      !(Maybe InputDataConfig),
    _clmrsBaseModelName ::
      !(Maybe BaseModelName),
    _clmrsModelStatus ::
      !(Maybe ModelStatus),
    _clmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLanguageModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmrsLanguageCode' - The language code of the text you've used to create a custom language model.
--
-- * 'clmrsModelName' - The name you've chosen for your custom language model.
--
-- * 'clmrsInputDataConfig' - The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
--
-- * 'clmrsBaseModelName' - The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
--
-- * 'clmrsModelStatus' - The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
--
-- * 'clmrsResponseStatus' - -- | The response status code.
createLanguageModelResponse ::
  -- | 'clmrsResponseStatus'
  Int ->
  CreateLanguageModelResponse
createLanguageModelResponse pResponseStatus_ =
  CreateLanguageModelResponse'
    { _clmrsLanguageCode = Nothing,
      _clmrsModelName = Nothing,
      _clmrsInputDataConfig = Nothing,
      _clmrsBaseModelName = Nothing,
      _clmrsModelStatus = Nothing,
      _clmrsResponseStatus = pResponseStatus_
    }

-- | The language code of the text you've used to create a custom language model.
clmrsLanguageCode :: Lens' CreateLanguageModelResponse (Maybe CLMLanguageCode)
clmrsLanguageCode = lens _clmrsLanguageCode (\s a -> s {_clmrsLanguageCode = a})

-- | The name you've chosen for your custom language model.
clmrsModelName :: Lens' CreateLanguageModelResponse (Maybe Text)
clmrsModelName = lens _clmrsModelName (\s a -> s {_clmrsModelName = a})

-- | The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
clmrsInputDataConfig :: Lens' CreateLanguageModelResponse (Maybe InputDataConfig)
clmrsInputDataConfig = lens _clmrsInputDataConfig (\s a -> s {_clmrsInputDataConfig = a})

-- | The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
clmrsBaseModelName :: Lens' CreateLanguageModelResponse (Maybe BaseModelName)
clmrsBaseModelName = lens _clmrsBaseModelName (\s a -> s {_clmrsBaseModelName = a})

-- | The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
clmrsModelStatus :: Lens' CreateLanguageModelResponse (Maybe ModelStatus)
clmrsModelStatus = lens _clmrsModelStatus (\s a -> s {_clmrsModelStatus = a})

-- | -- | The response status code.
clmrsResponseStatus :: Lens' CreateLanguageModelResponse Int
clmrsResponseStatus = lens _clmrsResponseStatus (\s a -> s {_clmrsResponseStatus = a})

instance NFData CreateLanguageModelResponse
