{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsBasicPutSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Hls Basic Put Settings
--
-- /See:/ 'hlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { _hbpsNumRetries ::
      !(Maybe Nat),
    _hbpsConnectionRetryInterval :: !(Maybe Nat),
    _hbpsFilecacheDuration :: !(Maybe Nat),
    _hbpsRestartDelay :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsBasicPutSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hbpsNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hbpsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hbpsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hbpsRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsBasicPutSettings ::
  HlsBasicPutSettings
hlsBasicPutSettings =
  HlsBasicPutSettings'
    { _hbpsNumRetries = Nothing,
      _hbpsConnectionRetryInterval = Nothing,
      _hbpsFilecacheDuration = Nothing,
      _hbpsRestartDelay = Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hbpsNumRetries :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsNumRetries = lens _hbpsNumRetries (\s a -> s {_hbpsNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hbpsConnectionRetryInterval :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsConnectionRetryInterval = lens _hbpsConnectionRetryInterval (\s a -> s {_hbpsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hbpsFilecacheDuration :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsFilecacheDuration = lens _hbpsFilecacheDuration (\s a -> s {_hbpsFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hbpsRestartDelay :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsRestartDelay = lens _hbpsRestartDelay (\s a -> s {_hbpsRestartDelay = a}) . mapping _Nat

instance FromJSON HlsBasicPutSettings where
  parseJSON =
    withObject
      "HlsBasicPutSettings"
      ( \x ->
          HlsBasicPutSettings'
            <$> (x .:? "numRetries")
            <*> (x .:? "connectionRetryInterval")
            <*> (x .:? "filecacheDuration")
            <*> (x .:? "restartDelay")
      )

instance Hashable HlsBasicPutSettings

instance NFData HlsBasicPutSettings

instance ToJSON HlsBasicPutSettings where
  toJSON HlsBasicPutSettings' {..} =
    object
      ( catMaybes
          [ ("numRetries" .=) <$> _hbpsNumRetries,
            ("connectionRetryInterval" .=) <$> _hbpsConnectionRetryInterval,
            ("filecacheDuration" .=) <$> _hbpsFilecacheDuration,
            ("restartDelay" .=) <$> _hbpsRestartDelay
          ]
      )
