{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the GCM channel for an application. This channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
--
--
-- /See:/ 'gcmChannelRequest' smart constructor.
data GCMChannelRequest = GCMChannelRequest'
  { _gcrEnabled ::
      !(Maybe Bool),
    _gcrAPIKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GCMChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrEnabled' - Specifies whether to enable the GCM channel for the application.
--
-- * 'gcrAPIKey' - The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
gcmChannelRequest ::
  -- | 'gcrAPIKey'
  Text ->
  GCMChannelRequest
gcmChannelRequest pAPIKey_ =
  GCMChannelRequest' {_gcrEnabled = Nothing, _gcrAPIKey = pAPIKey_}

-- | Specifies whether to enable the GCM channel for the application.
gcrEnabled :: Lens' GCMChannelRequest (Maybe Bool)
gcrEnabled = lens _gcrEnabled (\s a -> s {_gcrEnabled = a})

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
gcrAPIKey :: Lens' GCMChannelRequest Text
gcrAPIKey = lens _gcrAPIKey (\s a -> s {_gcrAPIKey = a})

instance Hashable GCMChannelRequest

instance NFData GCMChannelRequest

instance ToJSON GCMChannelRequest where
  toJSON GCMChannelRequest' {..} =
    object
      ( catMaybes
          [("Enabled" .=) <$> _gcrEnabled, Just ("ApiKey" .= _gcrAPIKey)]
      )
