{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsInputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsInputSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Hls Input Settings
--
-- /See:/ 'hlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { _hisBufferSegments ::
      !(Maybe Nat),
    _hisRetries :: !(Maybe Nat),
    _hisRetryInterval :: !(Maybe Nat),
    _hisBandwidth :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsInputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hisBufferSegments' - When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
--
-- * 'hisRetries' - The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
--
-- * 'hisRetryInterval' - The number of seconds between retries when an attempt to read a manifest or segment fails.
--
-- * 'hisBandwidth' - When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
hlsInputSettings ::
  HlsInputSettings
hlsInputSettings =
  HlsInputSettings'
    { _hisBufferSegments = Nothing,
      _hisRetries = Nothing,
      _hisRetryInterval = Nothing,
      _hisBandwidth = Nothing
    }

-- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
hisBufferSegments :: Lens' HlsInputSettings (Maybe Natural)
hisBufferSegments = lens _hisBufferSegments (\s a -> s {_hisBufferSegments = a}) . mapping _Nat

-- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
hisRetries :: Lens' HlsInputSettings (Maybe Natural)
hisRetries = lens _hisRetries (\s a -> s {_hisRetries = a}) . mapping _Nat

-- | The number of seconds between retries when an attempt to read a manifest or segment fails.
hisRetryInterval :: Lens' HlsInputSettings (Maybe Natural)
hisRetryInterval = lens _hisRetryInterval (\s a -> s {_hisRetryInterval = a}) . mapping _Nat

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
hisBandwidth :: Lens' HlsInputSettings (Maybe Natural)
hisBandwidth = lens _hisBandwidth (\s a -> s {_hisBandwidth = a}) . mapping _Nat

instance FromJSON HlsInputSettings where
  parseJSON =
    withObject
      "HlsInputSettings"
      ( \x ->
          HlsInputSettings'
            <$> (x .:? "bufferSegments")
            <*> (x .:? "retries")
            <*> (x .:? "retryInterval")
            <*> (x .:? "bandwidth")
      )

instance Hashable HlsInputSettings

instance NFData HlsInputSettings

instance ToJSON HlsInputSettings where
  toJSON HlsInputSettings' {..} =
    object
      ( catMaybes
          [ ("bufferSegments" .=) <$> _hisBufferSegments,
            ("retries" .=) <$> _hisRetries,
            ("retryInterval" .=) <$> _hisRetryInterval,
            ("bandwidth" .=) <$> _hisBandwidth
          ]
      )
