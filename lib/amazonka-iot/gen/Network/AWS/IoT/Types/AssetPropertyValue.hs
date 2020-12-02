{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyValue where

import Network.AWS.IoT.Types.AssetPropertyTimestamp
import Network.AWS.IoT.Types.AssetPropertyVariant
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An asset property value entry containing the following information.
--
--
--
-- /See:/ 'assetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { _apvQuality ::
      !(Maybe Text),
    _apvValue :: !AssetPropertyVariant,
    _apvTimestamp :: !AssetPropertyTimestamp
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssetPropertyValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apvQuality' - Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
--
-- * 'apvValue' - The value of the asset property.
--
-- * 'apvTimestamp' - The asset property value timestamp.
assetPropertyValue ::
  -- | 'apvValue'
  AssetPropertyVariant ->
  -- | 'apvTimestamp'
  AssetPropertyTimestamp ->
  AssetPropertyValue
assetPropertyValue pValue_ pTimestamp_ =
  AssetPropertyValue'
    { _apvQuality = Nothing,
      _apvValue = pValue_,
      _apvTimestamp = pTimestamp_
    }

-- | Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
apvQuality :: Lens' AssetPropertyValue (Maybe Text)
apvQuality = lens _apvQuality (\s a -> s {_apvQuality = a})

-- | The value of the asset property.
apvValue :: Lens' AssetPropertyValue AssetPropertyVariant
apvValue = lens _apvValue (\s a -> s {_apvValue = a})

-- | The asset property value timestamp.
apvTimestamp :: Lens' AssetPropertyValue AssetPropertyTimestamp
apvTimestamp = lens _apvTimestamp (\s a -> s {_apvTimestamp = a})

instance FromJSON AssetPropertyValue where
  parseJSON =
    withObject
      "AssetPropertyValue"
      ( \x ->
          AssetPropertyValue'
            <$> (x .:? "quality") <*> (x .: "value") <*> (x .: "timestamp")
      )

instance Hashable AssetPropertyValue

instance NFData AssetPropertyValue

instance ToJSON AssetPropertyValue where
  toJSON AssetPropertyValue' {..} =
    object
      ( catMaybes
          [ ("quality" .=) <$> _apvQuality,
            Just ("value" .= _apvValue),
            Just ("timestamp" .= _apvTimestamp)
          ]
      )
