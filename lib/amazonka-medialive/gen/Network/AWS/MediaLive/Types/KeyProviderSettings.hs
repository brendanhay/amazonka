{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.KeyProviderSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.KeyProviderSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.StaticKeySettings
import Network.AWS.Prelude

-- | Key Provider Settings
--
-- /See:/ 'keyProviderSettings' smart constructor.
newtype KeyProviderSettings = KeyProviderSettings'
  { _kpsStaticKeySettings ::
      Maybe StaticKeySettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyProviderSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpsStaticKeySettings' - Undocumented member.
keyProviderSettings ::
  KeyProviderSettings
keyProviderSettings =
  KeyProviderSettings' {_kpsStaticKeySettings = Nothing}

-- | Undocumented member.
kpsStaticKeySettings :: Lens' KeyProviderSettings (Maybe StaticKeySettings)
kpsStaticKeySettings = lens _kpsStaticKeySettings (\s a -> s {_kpsStaticKeySettings = a})

instance FromJSON KeyProviderSettings where
  parseJSON =
    withObject
      "KeyProviderSettings"
      (\x -> KeyProviderSettings' <$> (x .:? "staticKeySettings"))

instance Hashable KeyProviderSettings

instance NFData KeyProviderSettings

instance ToJSON KeyProviderSettings where
  toJSON KeyProviderSettings' {..} =
    object
      (catMaybes [("staticKeySettings" .=) <$> _kpsStaticKeySettings])
