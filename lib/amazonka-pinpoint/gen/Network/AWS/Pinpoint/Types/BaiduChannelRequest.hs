{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
--
--
-- /See:/ 'baiduChannelRequest' smart constructor.
data BaiduChannelRequest = BaiduChannelRequest'
  { _bcrEnabled ::
      !(Maybe Bool),
    _bcrSecretKey :: !Text,
    _bcrAPIKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BaiduChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcrEnabled' - Specifies whether to enable the Baidu channel for the application.
--
-- * 'bcrSecretKey' - The secret key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- * 'bcrAPIKey' - The API key that you received from the Baidu Cloud Push service to communicate with the service.
baiduChannelRequest ::
  -- | 'bcrSecretKey'
  Text ->
  -- | 'bcrAPIKey'
  Text ->
  BaiduChannelRequest
baiduChannelRequest pSecretKey_ pAPIKey_ =
  BaiduChannelRequest'
    { _bcrEnabled = Nothing,
      _bcrSecretKey = pSecretKey_,
      _bcrAPIKey = pAPIKey_
    }

-- | Specifies whether to enable the Baidu channel for the application.
bcrEnabled :: Lens' BaiduChannelRequest (Maybe Bool)
bcrEnabled = lens _bcrEnabled (\s a -> s {_bcrEnabled = a})

-- | The secret key that you received from the Baidu Cloud Push service to communicate with the service.
bcrSecretKey :: Lens' BaiduChannelRequest Text
bcrSecretKey = lens _bcrSecretKey (\s a -> s {_bcrSecretKey = a})

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
bcrAPIKey :: Lens' BaiduChannelRequest Text
bcrAPIKey = lens _bcrAPIKey (\s a -> s {_bcrAPIKey = a})

instance Hashable BaiduChannelRequest

instance NFData BaiduChannelRequest

instance ToJSON BaiduChannelRequest where
  toJSON BaiduChannelRequest' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _bcrEnabled,
            Just ("SecretKey" .= _bcrSecretKey),
            Just ("ApiKey" .= _bcrAPIKey)
          ]
      )
