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
-- Module      : Network.AWS.Lambda.GetCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified code signing configuration.
module Network.AWS.Lambda.GetCodeSigningConfig
  ( -- * Creating a Request
    getCodeSigningConfig,
    GetCodeSigningConfig,

    -- * Request Lenses
    gcscCodeSigningConfigARN,

    -- * Destructuring the Response
    getCodeSigningConfigResponse,
    GetCodeSigningConfigResponse,

    -- * Response Lenses
    gcscrsResponseStatus,
    gcscrsCodeSigningConfig,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCodeSigningConfig' smart constructor.
newtype GetCodeSigningConfig = GetCodeSigningConfig'
  { _gcscCodeSigningConfigARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcscCodeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
getCodeSigningConfig ::
  -- | 'gcscCodeSigningConfigARN'
  Text ->
  GetCodeSigningConfig
getCodeSigningConfig pCodeSigningConfigARN_ =
  GetCodeSigningConfig'
    { _gcscCodeSigningConfigARN =
        pCodeSigningConfigARN_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
gcscCodeSigningConfigARN :: Lens' GetCodeSigningConfig Text
gcscCodeSigningConfigARN = lens _gcscCodeSigningConfigARN (\s a -> s {_gcscCodeSigningConfigARN = a})

instance AWSRequest GetCodeSigningConfig where
  type Rs GetCodeSigningConfig = GetCodeSigningConfigResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          GetCodeSigningConfigResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "CodeSigningConfig")
      )

instance Hashable GetCodeSigningConfig

instance NFData GetCodeSigningConfig

instance ToHeaders GetCodeSigningConfig where
  toHeaders = const mempty

instance ToPath GetCodeSigningConfig where
  toPath GetCodeSigningConfig' {..} =
    mconcat
      [ "/2020-04-22/code-signing-configs/",
        toBS _gcscCodeSigningConfigARN
      ]

instance ToQuery GetCodeSigningConfig where
  toQuery = const mempty

-- | /See:/ 'getCodeSigningConfigResponse' smart constructor.
data GetCodeSigningConfigResponse = GetCodeSigningConfigResponse'
  { _gcscrsResponseStatus ::
      !Int,
    _gcscrsCodeSigningConfig ::
      !CodeSigningConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcscrsResponseStatus' - -- | The response status code.
--
-- * 'gcscrsCodeSigningConfig' - The code signing configuration
getCodeSigningConfigResponse ::
  -- | 'gcscrsResponseStatus'
  Int ->
  -- | 'gcscrsCodeSigningConfig'
  CodeSigningConfig ->
  GetCodeSigningConfigResponse
getCodeSigningConfigResponse pResponseStatus_ pCodeSigningConfig_ =
  GetCodeSigningConfigResponse'
    { _gcscrsResponseStatus =
        pResponseStatus_,
      _gcscrsCodeSigningConfig = pCodeSigningConfig_
    }

-- | -- | The response status code.
gcscrsResponseStatus :: Lens' GetCodeSigningConfigResponse Int
gcscrsResponseStatus = lens _gcscrsResponseStatus (\s a -> s {_gcscrsResponseStatus = a})

-- | The code signing configuration
gcscrsCodeSigningConfig :: Lens' GetCodeSigningConfigResponse CodeSigningConfig
gcscrsCodeSigningConfig = lens _gcscrsCodeSigningConfig (\s a -> s {_gcscrsCodeSigningConfig = a})

instance NFData GetCodeSigningConfigResponse
