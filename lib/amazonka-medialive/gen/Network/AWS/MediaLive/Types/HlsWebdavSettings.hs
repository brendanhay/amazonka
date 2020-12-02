{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsWebdavSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode
import Network.AWS.Prelude

-- | Hls Webdav Settings
--
-- /See:/ 'hlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'
  { _hwsHTTPTransferMode ::
      !(Maybe HlsWebdavHTTPTransferMode),
    _hwsNumRetries :: !(Maybe Nat),
    _hwsConnectionRetryInterval :: !(Maybe Nat),
    _hwsFilecacheDuration :: !(Maybe Nat),
    _hwsRestartDelay :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsWebdavSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hwsHTTPTransferMode' - Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- * 'hwsNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hwsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hwsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hwsRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsWebdavSettings ::
  HlsWebdavSettings
hlsWebdavSettings =
  HlsWebdavSettings'
    { _hwsHTTPTransferMode = Nothing,
      _hwsNumRetries = Nothing,
      _hwsConnectionRetryInterval = Nothing,
      _hwsFilecacheDuration = Nothing,
      _hwsRestartDelay = Nothing
    }

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
hwsHTTPTransferMode :: Lens' HlsWebdavSettings (Maybe HlsWebdavHTTPTransferMode)
hwsHTTPTransferMode = lens _hwsHTTPTransferMode (\s a -> s {_hwsHTTPTransferMode = a})

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hwsNumRetries :: Lens' HlsWebdavSettings (Maybe Natural)
hwsNumRetries = lens _hwsNumRetries (\s a -> s {_hwsNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hwsConnectionRetryInterval :: Lens' HlsWebdavSettings (Maybe Natural)
hwsConnectionRetryInterval = lens _hwsConnectionRetryInterval (\s a -> s {_hwsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hwsFilecacheDuration :: Lens' HlsWebdavSettings (Maybe Natural)
hwsFilecacheDuration = lens _hwsFilecacheDuration (\s a -> s {_hwsFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hwsRestartDelay :: Lens' HlsWebdavSettings (Maybe Natural)
hwsRestartDelay = lens _hwsRestartDelay (\s a -> s {_hwsRestartDelay = a}) . mapping _Nat

instance FromJSON HlsWebdavSettings where
  parseJSON =
    withObject
      "HlsWebdavSettings"
      ( \x ->
          HlsWebdavSettings'
            <$> (x .:? "httpTransferMode")
            <*> (x .:? "numRetries")
            <*> (x .:? "connectionRetryInterval")
            <*> (x .:? "filecacheDuration")
            <*> (x .:? "restartDelay")
      )

instance Hashable HlsWebdavSettings

instance NFData HlsWebdavSettings

instance ToJSON HlsWebdavSettings where
  toJSON HlsWebdavSettings' {..} =
    object
      ( catMaybes
          [ ("httpTransferMode" .=) <$> _hwsHTTPTransferMode,
            ("numRetries" .=) <$> _hwsNumRetries,
            ("connectionRetryInterval" .=) <$> _hwsConnectionRetryInterval,
            ("filecacheDuration" .=) <$> _hwsFilecacheDuration,
            ("restartDelay" .=) <$> _hwsRestartDelay
          ]
      )
