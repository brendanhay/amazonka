{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAkamaiSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode
import Network.AWS.Prelude

-- | Hls Akamai Settings
--
-- /See:/ 'hlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { _hasHTTPTransferMode ::
      !(Maybe HlsAkamaiHTTPTransferMode),
    _hasNumRetries :: !(Maybe Nat),
    _hasToken :: !(Maybe Text),
    _hasConnectionRetryInterval :: !(Maybe Nat),
    _hasFilecacheDuration :: !(Maybe Nat),
    _hasRestartDelay :: !(Maybe Nat),
    _hasSalt :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsAkamaiSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hasHTTPTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
--
-- * 'hasNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hasToken' - Token parameter for authenticated akamai. If not specified, _gda_ is used.
--
-- * 'hasConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hasFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hasRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- * 'hasSalt' - Salt for authenticated Akamai.
hlsAkamaiSettings ::
  HlsAkamaiSettings
hlsAkamaiSettings =
  HlsAkamaiSettings'
    { _hasHTTPTransferMode = Nothing,
      _hasNumRetries = Nothing,
      _hasToken = Nothing,
      _hasConnectionRetryInterval = Nothing,
      _hasFilecacheDuration = Nothing,
      _hasRestartDelay = Nothing,
      _hasSalt = Nothing
    }

-- | Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
hasHTTPTransferMode :: Lens' HlsAkamaiSettings (Maybe HlsAkamaiHTTPTransferMode)
hasHTTPTransferMode = lens _hasHTTPTransferMode (\s a -> s {_hasHTTPTransferMode = a})

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hasNumRetries :: Lens' HlsAkamaiSettings (Maybe Natural)
hasNumRetries = lens _hasNumRetries (\s a -> s {_hasNumRetries = a}) . mapping _Nat

-- | Token parameter for authenticated akamai. If not specified, _gda_ is used.
hasToken :: Lens' HlsAkamaiSettings (Maybe Text)
hasToken = lens _hasToken (\s a -> s {_hasToken = a})

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hasConnectionRetryInterval :: Lens' HlsAkamaiSettings (Maybe Natural)
hasConnectionRetryInterval = lens _hasConnectionRetryInterval (\s a -> s {_hasConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hasFilecacheDuration :: Lens' HlsAkamaiSettings (Maybe Natural)
hasFilecacheDuration = lens _hasFilecacheDuration (\s a -> s {_hasFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hasRestartDelay :: Lens' HlsAkamaiSettings (Maybe Natural)
hasRestartDelay = lens _hasRestartDelay (\s a -> s {_hasRestartDelay = a}) . mapping _Nat

-- | Salt for authenticated Akamai.
hasSalt :: Lens' HlsAkamaiSettings (Maybe Text)
hasSalt = lens _hasSalt (\s a -> s {_hasSalt = a})

instance FromJSON HlsAkamaiSettings where
  parseJSON =
    withObject
      "HlsAkamaiSettings"
      ( \x ->
          HlsAkamaiSettings'
            <$> (x .:? "httpTransferMode")
            <*> (x .:? "numRetries")
            <*> (x .:? "token")
            <*> (x .:? "connectionRetryInterval")
            <*> (x .:? "filecacheDuration")
            <*> (x .:? "restartDelay")
            <*> (x .:? "salt")
      )

instance Hashable HlsAkamaiSettings

instance NFData HlsAkamaiSettings

instance ToJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings' {..} =
    object
      ( catMaybes
          [ ("httpTransferMode" .=) <$> _hasHTTPTransferMode,
            ("numRetries" .=) <$> _hasNumRetries,
            ("token" .=) <$> _hasToken,
            ("connectionRetryInterval" .=) <$> _hasConnectionRetryInterval,
            ("filecacheDuration" .=) <$> _hasFilecacheDuration,
            ("restartDelay" .=) <$> _hasRestartDelay,
            ("salt" .=) <$> _hasSalt
          ]
      )
