{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StaticKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.StaticKeyProvider where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'staticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { _skpStaticKeyValue ::
      !(Maybe Text),
    _skpURL :: !(Maybe Text),
    _skpKeyFormat :: !(Maybe Text),
    _skpKeyFormatVersions :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StaticKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpStaticKeyValue' - Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
--
-- * 'skpURL' - Relates to DRM implementation. The location of the license server used for protecting content.
--
-- * 'skpKeyFormat' - Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
--
-- * 'skpKeyFormatVersions' - Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
staticKeyProvider ::
  StaticKeyProvider
staticKeyProvider =
  StaticKeyProvider'
    { _skpStaticKeyValue = Nothing,
      _skpURL = Nothing,
      _skpKeyFormat = Nothing,
      _skpKeyFormatVersions = Nothing
    }

-- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
skpStaticKeyValue :: Lens' StaticKeyProvider (Maybe Text)
skpStaticKeyValue = lens _skpStaticKeyValue (\s a -> s {_skpStaticKeyValue = a})

-- | Relates to DRM implementation. The location of the license server used for protecting content.
skpURL :: Lens' StaticKeyProvider (Maybe Text)
skpURL = lens _skpURL (\s a -> s {_skpURL = a})

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
skpKeyFormat :: Lens' StaticKeyProvider (Maybe Text)
skpKeyFormat = lens _skpKeyFormat (\s a -> s {_skpKeyFormat = a})

-- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
skpKeyFormatVersions :: Lens' StaticKeyProvider (Maybe Text)
skpKeyFormatVersions = lens _skpKeyFormatVersions (\s a -> s {_skpKeyFormatVersions = a})

instance FromJSON StaticKeyProvider where
  parseJSON =
    withObject
      "StaticKeyProvider"
      ( \x ->
          StaticKeyProvider'
            <$> (x .:? "staticKeyValue")
            <*> (x .:? "url")
            <*> (x .:? "keyFormat")
            <*> (x .:? "keyFormatVersions")
      )

instance Hashable StaticKeyProvider

instance NFData StaticKeyProvider

instance ToJSON StaticKeyProvider where
  toJSON StaticKeyProvider' {..} =
    object
      ( catMaybes
          [ ("staticKeyValue" .=) <$> _skpStaticKeyValue,
            ("url" .=) <$> _skpURL,
            ("keyFormat" .=) <$> _skpKeyFormat,
            ("keyFormatVersions" .=) <$> _skpKeyFormatVersions
          ]
      )
