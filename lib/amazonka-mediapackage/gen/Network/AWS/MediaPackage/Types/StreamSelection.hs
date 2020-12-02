{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamSelection where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.StreamOrder
import Network.AWS.Prelude

-- | A StreamSelection configuration.
--
-- /See:/ 'streamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { _ssStreamOrder ::
      !(Maybe StreamOrder),
    _ssMinVideoBitsPerSecond :: !(Maybe Int),
    _ssMaxVideoBitsPerSecond :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamOrder' - A directive that determines the order of streams in the output.
--
-- * 'ssMinVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
--
-- * 'ssMaxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
streamSelection ::
  StreamSelection
streamSelection =
  StreamSelection'
    { _ssStreamOrder = Nothing,
      _ssMinVideoBitsPerSecond = Nothing,
      _ssMaxVideoBitsPerSecond = Nothing
    }

-- | A directive that determines the order of streams in the output.
ssStreamOrder :: Lens' StreamSelection (Maybe StreamOrder)
ssStreamOrder = lens _ssStreamOrder (\s a -> s {_ssStreamOrder = a})

-- | The minimum video bitrate (bps) to include in output.
ssMinVideoBitsPerSecond :: Lens' StreamSelection (Maybe Int)
ssMinVideoBitsPerSecond = lens _ssMinVideoBitsPerSecond (\s a -> s {_ssMinVideoBitsPerSecond = a})

-- | The maximum video bitrate (bps) to include in output.
ssMaxVideoBitsPerSecond :: Lens' StreamSelection (Maybe Int)
ssMaxVideoBitsPerSecond = lens _ssMaxVideoBitsPerSecond (\s a -> s {_ssMaxVideoBitsPerSecond = a})

instance FromJSON StreamSelection where
  parseJSON =
    withObject
      "StreamSelection"
      ( \x ->
          StreamSelection'
            <$> (x .:? "streamOrder")
            <*> (x .:? "minVideoBitsPerSecond")
            <*> (x .:? "maxVideoBitsPerSecond")
      )

instance Hashable StreamSelection

instance NFData StreamSelection

instance ToJSON StreamSelection where
  toJSON StreamSelection' {..} =
    object
      ( catMaybes
          [ ("streamOrder" .=) <$> _ssStreamOrder,
            ("minVideoBitsPerSecond" .=) <$> _ssMinVideoBitsPerSecond,
            ("maxVideoBitsPerSecond" .=) <$> _ssMaxVideoBitsPerSecond
          ]
      )
