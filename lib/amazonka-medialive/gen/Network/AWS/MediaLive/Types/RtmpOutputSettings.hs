{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
import Network.AWS.Prelude

-- | Rtmp Output Settings
--
-- /See:/ 'rtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'
  { _rosNumRetries ::
      !(Maybe Nat),
    _rosCertificateMode ::
      !(Maybe RtmpOutputCertificateMode),
    _rosConnectionRetryInterval :: !(Maybe Nat),
    _rosDestination :: !OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RtmpOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rosNumRetries' - Number of retry attempts.
--
-- * 'rosCertificateMode' - If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
--
-- * 'rosConnectionRetryInterval' - Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
--
-- * 'rosDestination' - The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
rtmpOutputSettings ::
  -- | 'rosDestination'
  OutputLocationRef ->
  RtmpOutputSettings
rtmpOutputSettings pDestination_ =
  RtmpOutputSettings'
    { _rosNumRetries = Nothing,
      _rosCertificateMode = Nothing,
      _rosConnectionRetryInterval = Nothing,
      _rosDestination = pDestination_
    }

-- | Number of retry attempts.
rosNumRetries :: Lens' RtmpOutputSettings (Maybe Natural)
rosNumRetries = lens _rosNumRetries (\s a -> s {_rosNumRetries = a}) . mapping _Nat

-- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
rosCertificateMode :: Lens' RtmpOutputSettings (Maybe RtmpOutputCertificateMode)
rosCertificateMode = lens _rosCertificateMode (\s a -> s {_rosCertificateMode = a})

-- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
rosConnectionRetryInterval :: Lens' RtmpOutputSettings (Maybe Natural)
rosConnectionRetryInterval = lens _rosConnectionRetryInterval (\s a -> s {_rosConnectionRetryInterval = a}) . mapping _Nat

-- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
rosDestination :: Lens' RtmpOutputSettings OutputLocationRef
rosDestination = lens _rosDestination (\s a -> s {_rosDestination = a})

instance FromJSON RtmpOutputSettings where
  parseJSON =
    withObject
      "RtmpOutputSettings"
      ( \x ->
          RtmpOutputSettings'
            <$> (x .:? "numRetries")
            <*> (x .:? "certificateMode")
            <*> (x .:? "connectionRetryInterval")
            <*> (x .: "destination")
      )

instance Hashable RtmpOutputSettings

instance NFData RtmpOutputSettings

instance ToJSON RtmpOutputSettings where
  toJSON RtmpOutputSettings' {..} =
    object
      ( catMaybes
          [ ("numRetries" .=) <$> _rosNumRetries,
            ("certificateMode" .=) <$> _rosCertificateMode,
            ("connectionRetryInterval" .=) <$> _rosConnectionRetryInterval,
            Just ("destination" .= _rosDestination)
          ]
      )
