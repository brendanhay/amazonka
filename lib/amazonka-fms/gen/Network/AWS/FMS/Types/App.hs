{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.App where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An individual AWS Firewall Manager application.
--
--
--
-- /See:/ 'app' smart constructor.
data App = App'
  { _appAppName :: !Text,
    _appProtocol :: !Text,
    _appPort :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appAppName' - The application's name.
--
-- * 'appProtocol' - The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
--
-- * 'appPort' - The application's port number, for example @80@ .
app ::
  -- | 'appAppName'
  Text ->
  -- | 'appProtocol'
  Text ->
  -- | 'appPort'
  Natural ->
  App
app pAppName_ pProtocol_ pPort_ =
  App'
    { _appAppName = pAppName_,
      _appProtocol = pProtocol_,
      _appPort = _Nat # pPort_
    }

-- | The application's name.
appAppName :: Lens' App Text
appAppName = lens _appAppName (\s a -> s {_appAppName = a})

-- | The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
appProtocol :: Lens' App Text
appProtocol = lens _appProtocol (\s a -> s {_appProtocol = a})

-- | The application's port number, for example @80@ .
appPort :: Lens' App Natural
appPort = lens _appPort (\s a -> s {_appPort = a}) . _Nat

instance FromJSON App where
  parseJSON =
    withObject
      "App"
      ( \x ->
          App' <$> (x .: "AppName") <*> (x .: "Protocol") <*> (x .: "Port")
      )

instance Hashable App

instance NFData App

instance ToJSON App where
  toJSON App' {..} =
    object
      ( catMaybes
          [ Just ("AppName" .= _appAppName),
            Just ("Protocol" .= _appProtocol),
            Just ("Port" .= _appPort)
          ]
      )
