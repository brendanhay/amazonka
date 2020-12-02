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
-- Module      : Network.AWS.SageMaker.UpdateAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of an AppImageConfig.
module Network.AWS.SageMaker.UpdateAppImageConfig
  ( -- * Creating a Request
    updateAppImageConfig,
    UpdateAppImageConfig,

    -- * Request Lenses
    uaicKernelGatewayImageConfig,
    uaicAppImageConfigName,

    -- * Destructuring the Response
    updateAppImageConfigResponse,
    UpdateAppImageConfigResponse,

    -- * Response Lenses
    uaicrsAppImageConfigARN,
    uaicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateAppImageConfig' smart constructor.
data UpdateAppImageConfig = UpdateAppImageConfig'
  { _uaicKernelGatewayImageConfig ::
      !(Maybe KernelGatewayImageConfig),
    _uaicAppImageConfigName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAppImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaicKernelGatewayImageConfig' - The new KernelGateway app to run on the image.
--
-- * 'uaicAppImageConfigName' - The name of the AppImageConfig to update.
updateAppImageConfig ::
  -- | 'uaicAppImageConfigName'
  Text ->
  UpdateAppImageConfig
updateAppImageConfig pAppImageConfigName_ =
  UpdateAppImageConfig'
    { _uaicKernelGatewayImageConfig = Nothing,
      _uaicAppImageConfigName = pAppImageConfigName_
    }

-- | The new KernelGateway app to run on the image.
uaicKernelGatewayImageConfig :: Lens' UpdateAppImageConfig (Maybe KernelGatewayImageConfig)
uaicKernelGatewayImageConfig = lens _uaicKernelGatewayImageConfig (\s a -> s {_uaicKernelGatewayImageConfig = a})

-- | The name of the AppImageConfig to update.
uaicAppImageConfigName :: Lens' UpdateAppImageConfig Text
uaicAppImageConfigName = lens _uaicAppImageConfigName (\s a -> s {_uaicAppImageConfigName = a})

instance AWSRequest UpdateAppImageConfig where
  type Rs UpdateAppImageConfig = UpdateAppImageConfigResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateAppImageConfigResponse'
            <$> (x .?> "AppImageConfigArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateAppImageConfig

instance NFData UpdateAppImageConfig

instance ToHeaders UpdateAppImageConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.UpdateAppImageConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateAppImageConfig where
  toJSON UpdateAppImageConfig' {..} =
    object
      ( catMaybes
          [ ("KernelGatewayImageConfig" .=) <$> _uaicKernelGatewayImageConfig,
            Just ("AppImageConfigName" .= _uaicAppImageConfigName)
          ]
      )

instance ToPath UpdateAppImageConfig where
  toPath = const "/"

instance ToQuery UpdateAppImageConfig where
  toQuery = const mempty

-- | /See:/ 'updateAppImageConfigResponse' smart constructor.
data UpdateAppImageConfigResponse = UpdateAppImageConfigResponse'
  { _uaicrsAppImageConfigARN ::
      !(Maybe Text),
    _uaicrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAppImageConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaicrsAppImageConfigARN' - The Amazon Resource Name (ARN) for the AppImageConfig.
--
-- * 'uaicrsResponseStatus' - -- | The response status code.
updateAppImageConfigResponse ::
  -- | 'uaicrsResponseStatus'
  Int ->
  UpdateAppImageConfigResponse
updateAppImageConfigResponse pResponseStatus_ =
  UpdateAppImageConfigResponse'
    { _uaicrsAppImageConfigARN = Nothing,
      _uaicrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) for the AppImageConfig.
uaicrsAppImageConfigARN :: Lens' UpdateAppImageConfigResponse (Maybe Text)
uaicrsAppImageConfigARN = lens _uaicrsAppImageConfigARN (\s a -> s {_uaicrsAppImageConfigARN = a})

-- | -- | The response status code.
uaicrsResponseStatus :: Lens' UpdateAppImageConfigResponse Int
uaicrsResponseStatus = lens _uaicrsResponseStatus (\s a -> s {_uaicrsResponseStatus = a})

instance NFData UpdateAppImageConfigResponse
