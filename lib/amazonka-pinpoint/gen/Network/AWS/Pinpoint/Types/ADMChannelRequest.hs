{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
--
--
-- /See:/ 'aDMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { _admcrEnabled ::
      !(Maybe Bool),
    _admcrClientSecret :: !Text,
    _admcrClientId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ADMChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admcrEnabled' - Specifies whether to enable the ADM channel for the application.
--
-- * 'admcrClientSecret' - The Client Secret that you received from Amazon to send messages by using ADM.
--
-- * 'admcrClientId' - The Client ID that you received from Amazon to send messages by using ADM.
aDMChannelRequest ::
  -- | 'admcrClientSecret'
  Text ->
  -- | 'admcrClientId'
  Text ->
  ADMChannelRequest
aDMChannelRequest pClientSecret_ pClientId_ =
  ADMChannelRequest'
    { _admcrEnabled = Nothing,
      _admcrClientSecret = pClientSecret_,
      _admcrClientId = pClientId_
    }

-- | Specifies whether to enable the ADM channel for the application.
admcrEnabled :: Lens' ADMChannelRequest (Maybe Bool)
admcrEnabled = lens _admcrEnabled (\s a -> s {_admcrEnabled = a})

-- | The Client Secret that you received from Amazon to send messages by using ADM.
admcrClientSecret :: Lens' ADMChannelRequest Text
admcrClientSecret = lens _admcrClientSecret (\s a -> s {_admcrClientSecret = a})

-- | The Client ID that you received from Amazon to send messages by using ADM.
admcrClientId :: Lens' ADMChannelRequest Text
admcrClientId = lens _admcrClientId (\s a -> s {_admcrClientId = a})

instance Hashable ADMChannelRequest

instance NFData ADMChannelRequest

instance ToJSON ADMChannelRequest where
  toJSON ADMChannelRequest' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _admcrEnabled,
            Just ("ClientSecret" .= _admcrClientSecret),
            Just ("ClientId" .= _admcrClientId)
          ]
      )
