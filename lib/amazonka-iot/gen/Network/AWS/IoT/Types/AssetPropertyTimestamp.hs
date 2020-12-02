{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyTimestamp where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An asset property timestamp entry containing the following information.
--
--
--
-- /See:/ 'assetPropertyTimestamp' smart constructor.
data AssetPropertyTimestamp = AssetPropertyTimestamp'
  { _aptOffsetInNanos ::
      !(Maybe Text),
    _aptTimeInSeconds :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssetPropertyTimestamp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aptOffsetInNanos' - Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
--
-- * 'aptTimeInSeconds' - A string that contains the time in seconds since epoch. Accepts substitution templates.
assetPropertyTimestamp ::
  -- | 'aptTimeInSeconds'
  Text ->
  AssetPropertyTimestamp
assetPropertyTimestamp pTimeInSeconds_ =
  AssetPropertyTimestamp'
    { _aptOffsetInNanos = Nothing,
      _aptTimeInSeconds = pTimeInSeconds_
    }

-- | Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
aptOffsetInNanos :: Lens' AssetPropertyTimestamp (Maybe Text)
aptOffsetInNanos = lens _aptOffsetInNanos (\s a -> s {_aptOffsetInNanos = a})

-- | A string that contains the time in seconds since epoch. Accepts substitution templates.
aptTimeInSeconds :: Lens' AssetPropertyTimestamp Text
aptTimeInSeconds = lens _aptTimeInSeconds (\s a -> s {_aptTimeInSeconds = a})

instance FromJSON AssetPropertyTimestamp where
  parseJSON =
    withObject
      "AssetPropertyTimestamp"
      ( \x ->
          AssetPropertyTimestamp'
            <$> (x .:? "offsetInNanos") <*> (x .: "timeInSeconds")
      )

instance Hashable AssetPropertyTimestamp

instance NFData AssetPropertyTimestamp

instance ToJSON AssetPropertyTimestamp where
  toJSON AssetPropertyTimestamp' {..} =
    object
      ( catMaybes
          [ ("offsetInNanos" .=) <$> _aptOffsetInNanos,
            Just ("timeInSeconds" .= _aptTimeInSeconds)
          ]
      )
