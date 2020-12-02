{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FecOutputSettings
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.UdpContainerSettings
import Network.AWS.Prelude

-- | Udp Output Settings
--
-- /See:/ 'udpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'
  { _uosFecOutputSettings ::
      !(Maybe FecOutputSettings),
    _uosBufferMsec :: !(Maybe Nat),
    _uosDestination :: !OutputLocationRef,
    _uosContainerSettings :: !UdpContainerSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UdpOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uosFecOutputSettings' - Settings for enabling and adjusting Forward Error Correction on UDP outputs.
--
-- * 'uosBufferMsec' - UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
--
-- * 'uosDestination' - Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
--
-- * 'uosContainerSettings' - Undocumented member.
udpOutputSettings ::
  -- | 'uosDestination'
  OutputLocationRef ->
  -- | 'uosContainerSettings'
  UdpContainerSettings ->
  UdpOutputSettings
udpOutputSettings pDestination_ pContainerSettings_ =
  UdpOutputSettings'
    { _uosFecOutputSettings = Nothing,
      _uosBufferMsec = Nothing,
      _uosDestination = pDestination_,
      _uosContainerSettings = pContainerSettings_
    }

-- | Settings for enabling and adjusting Forward Error Correction on UDP outputs.
uosFecOutputSettings :: Lens' UdpOutputSettings (Maybe FecOutputSettings)
uosFecOutputSettings = lens _uosFecOutputSettings (\s a -> s {_uosFecOutputSettings = a})

-- | UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
uosBufferMsec :: Lens' UdpOutputSettings (Maybe Natural)
uosBufferMsec = lens _uosBufferMsec (\s a -> s {_uosBufferMsec = a}) . mapping _Nat

-- | Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
uosDestination :: Lens' UdpOutputSettings OutputLocationRef
uosDestination = lens _uosDestination (\s a -> s {_uosDestination = a})

-- | Undocumented member.
uosContainerSettings :: Lens' UdpOutputSettings UdpContainerSettings
uosContainerSettings = lens _uosContainerSettings (\s a -> s {_uosContainerSettings = a})

instance FromJSON UdpOutputSettings where
  parseJSON =
    withObject
      "UdpOutputSettings"
      ( \x ->
          UdpOutputSettings'
            <$> (x .:? "fecOutputSettings")
            <*> (x .:? "bufferMsec")
            <*> (x .: "destination")
            <*> (x .: "containerSettings")
      )

instance Hashable UdpOutputSettings

instance NFData UdpOutputSettings

instance ToJSON UdpOutputSettings where
  toJSON UdpOutputSettings' {..} =
    object
      ( catMaybes
          [ ("fecOutputSettings" .=) <$> _uosFecOutputSettings,
            ("bufferMsec" .=) <$> _uosBufferMsec,
            Just ("destination" .= _uosDestination),
            Just ("containerSettings" .= _uosContainerSettings)
          ]
      )
