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
-- Module      : Network.AWS.SageMaker.CreateAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for running a SageMaker image as a KernelGateway app. The configuration specifies the Amazon Elastic File System (EFS) storage volume on the image, and a list of the kernels in the image.
module Network.AWS.SageMaker.CreateAppImageConfig
  ( -- * Creating a Request
    createAppImageConfig,
    CreateAppImageConfig,

    -- * Request Lenses
    caicKernelGatewayImageConfig,
    caicTags,
    caicAppImageConfigName,

    -- * Destructuring the Response
    createAppImageConfigResponse,
    CreateAppImageConfigResponse,

    -- * Response Lenses
    caicrsAppImageConfigARN,
    caicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { _caicKernelGatewayImageConfig ::
      !(Maybe KernelGatewayImageConfig),
    _caicTags :: !(Maybe [Tag]),
    _caicAppImageConfigName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAppImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caicKernelGatewayImageConfig' - The KernelGatewayImageConfig.
--
-- * 'caicTags' - A list of tags to apply to the AppImageConfig.
--
-- * 'caicAppImageConfigName' - The name of the AppImageConfig. Must be unique to your account.
createAppImageConfig ::
  -- | 'caicAppImageConfigName'
  Text ->
  CreateAppImageConfig
createAppImageConfig pAppImageConfigName_ =
  CreateAppImageConfig'
    { _caicKernelGatewayImageConfig = Nothing,
      _caicTags = Nothing,
      _caicAppImageConfigName = pAppImageConfigName_
    }

-- | The KernelGatewayImageConfig.
caicKernelGatewayImageConfig :: Lens' CreateAppImageConfig (Maybe KernelGatewayImageConfig)
caicKernelGatewayImageConfig = lens _caicKernelGatewayImageConfig (\s a -> s {_caicKernelGatewayImageConfig = a})

-- | A list of tags to apply to the AppImageConfig.
caicTags :: Lens' CreateAppImageConfig [Tag]
caicTags = lens _caicTags (\s a -> s {_caicTags = a}) . _Default . _Coerce

-- | The name of the AppImageConfig. Must be unique to your account.
caicAppImageConfigName :: Lens' CreateAppImageConfig Text
caicAppImageConfigName = lens _caicAppImageConfigName (\s a -> s {_caicAppImageConfigName = a})

instance AWSRequest CreateAppImageConfig where
  type Rs CreateAppImageConfig = CreateAppImageConfigResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            <$> (x .?> "AppImageConfigArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateAppImageConfig

instance NFData CreateAppImageConfig

instance ToHeaders CreateAppImageConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.CreateAppImageConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig' {..} =
    object
      ( catMaybes
          [ ("KernelGatewayImageConfig" .=) <$> _caicKernelGatewayImageConfig,
            ("Tags" .=) <$> _caicTags,
            Just ("AppImageConfigName" .= _caicAppImageConfigName)
          ]
      )

instance ToPath CreateAppImageConfig where
  toPath = const "/"

instance ToQuery CreateAppImageConfig where
  toQuery = const mempty

-- | /See:/ 'createAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { _caicrsAppImageConfigARN ::
      !(Maybe Text),
    _caicrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAppImageConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caicrsAppImageConfigARN' - The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- * 'caicrsResponseStatus' - -- | The response status code.
createAppImageConfigResponse ::
  -- | 'caicrsResponseStatus'
  Int ->
  CreateAppImageConfigResponse
createAppImageConfigResponse pResponseStatus_ =
  CreateAppImageConfigResponse'
    { _caicrsAppImageConfigARN = Nothing,
      _caicrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
caicrsAppImageConfigARN :: Lens' CreateAppImageConfigResponse (Maybe Text)
caicrsAppImageConfigARN = lens _caicrsAppImageConfigARN (\s a -> s {_caicrsAppImageConfigARN = a})

-- | -- | The response status code.
caicrsResponseStatus :: Lens' CreateAppImageConfigResponse Int
caicrsResponseStatus = lens _caicrsResponseStatus (\s a -> s {_caicrsResponseStatus = a})

instance NFData CreateAppImageConfigResponse
