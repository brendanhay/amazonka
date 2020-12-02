{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticKeySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticKeySettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.Prelude

-- | Static Key Settings
--
-- /See:/ 'staticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'
  { _sksKeyProviderServer ::
      !(Maybe InputLocation),
    _sksStaticKeyValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StaticKeySettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sksKeyProviderServer' - The URL of the license server used for protecting content.
--
-- * 'sksStaticKeyValue' - Static key value as a 32 character hexadecimal string.
staticKeySettings ::
  -- | 'sksStaticKeyValue'
  Text ->
  StaticKeySettings
staticKeySettings pStaticKeyValue_ =
  StaticKeySettings'
    { _sksKeyProviderServer = Nothing,
      _sksStaticKeyValue = pStaticKeyValue_
    }

-- | The URL of the license server used for protecting content.
sksKeyProviderServer :: Lens' StaticKeySettings (Maybe InputLocation)
sksKeyProviderServer = lens _sksKeyProviderServer (\s a -> s {_sksKeyProviderServer = a})

-- | Static key value as a 32 character hexadecimal string.
sksStaticKeyValue :: Lens' StaticKeySettings Text
sksStaticKeyValue = lens _sksStaticKeyValue (\s a -> s {_sksStaticKeyValue = a})

instance FromJSON StaticKeySettings where
  parseJSON =
    withObject
      "StaticKeySettings"
      ( \x ->
          StaticKeySettings'
            <$> (x .:? "keyProviderServer") <*> (x .: "staticKeyValue")
      )

instance Hashable StaticKeySettings

instance NFData StaticKeySettings

instance ToJSON StaticKeySettings where
  toJSON StaticKeySettings' {..} =
    object
      ( catMaybes
          [ ("keyProviderServer" .=) <$> _sksKeyProviderServer,
            Just ("staticKeyValue" .= _sksStaticKeyValue)
          ]
      )
