{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsMediaStoreSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
import Network.AWS.Prelude

-- | Hls Media Store Settings
--
-- /See:/ 'hlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { _hmssNumRetries ::
      !(Maybe Nat),
    _hmssConnectionRetryInterval :: !(Maybe Nat),
    _hmssFilecacheDuration :: !(Maybe Nat),
    _hmssMediaStoreStorageClass ::
      !(Maybe HlsMediaStoreStorageClass),
    _hmssRestartDelay :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsMediaStoreSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmssNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hmssConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hmssFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hmssMediaStoreStorageClass' - When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
--
-- * 'hmssRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsMediaStoreSettings ::
  HlsMediaStoreSettings
hlsMediaStoreSettings =
  HlsMediaStoreSettings'
    { _hmssNumRetries = Nothing,
      _hmssConnectionRetryInterval = Nothing,
      _hmssFilecacheDuration = Nothing,
      _hmssMediaStoreStorageClass = Nothing,
      _hmssRestartDelay = Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hmssNumRetries :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssNumRetries = lens _hmssNumRetries (\s a -> s {_hmssNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hmssConnectionRetryInterval :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssConnectionRetryInterval = lens _hmssConnectionRetryInterval (\s a -> s {_hmssConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hmssFilecacheDuration :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssFilecacheDuration = lens _hmssFilecacheDuration (\s a -> s {_hmssFilecacheDuration = a}) . mapping _Nat

-- | When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
hmssMediaStoreStorageClass :: Lens' HlsMediaStoreSettings (Maybe HlsMediaStoreStorageClass)
hmssMediaStoreStorageClass = lens _hmssMediaStoreStorageClass (\s a -> s {_hmssMediaStoreStorageClass = a})

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hmssRestartDelay :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssRestartDelay = lens _hmssRestartDelay (\s a -> s {_hmssRestartDelay = a}) . mapping _Nat

instance FromJSON HlsMediaStoreSettings where
  parseJSON =
    withObject
      "HlsMediaStoreSettings"
      ( \x ->
          HlsMediaStoreSettings'
            <$> (x .:? "numRetries")
            <*> (x .:? "connectionRetryInterval")
            <*> (x .:? "filecacheDuration")
            <*> (x .:? "mediaStoreStorageClass")
            <*> (x .:? "restartDelay")
      )

instance Hashable HlsMediaStoreSettings

instance NFData HlsMediaStoreSettings

instance ToJSON HlsMediaStoreSettings where
  toJSON HlsMediaStoreSettings' {..} =
    object
      ( catMaybes
          [ ("numRetries" .=) <$> _hmssNumRetries,
            ("connectionRetryInterval" .=) <$> _hmssConnectionRetryInterval,
            ("filecacheDuration" .=) <$> _hmssFilecacheDuration,
            ("mediaStoreStorageClass" .=) <$> _hmssMediaStoreStorageClass,
            ("restartDelay" .=) <$> _hmssRestartDelay
          ]
      )
