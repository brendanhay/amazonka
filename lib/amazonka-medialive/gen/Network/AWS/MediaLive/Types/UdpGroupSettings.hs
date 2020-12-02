{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputLossActionForUdpOut
import Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
import Network.AWS.Prelude

-- | Udp Group Settings
--
-- /See:/ 'udpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { _ugsTimedMetadataId3Period ::
      !(Maybe Nat),
    _ugsInputLossAction :: !(Maybe InputLossActionForUdpOut),
    _ugsTimedMetadataId3Frame ::
      !(Maybe UdpTimedMetadataId3Frame)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UdpGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'ugsInputLossAction' - Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
--
-- * 'ugsTimedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
udpGroupSettings ::
  UdpGroupSettings
udpGroupSettings =
  UdpGroupSettings'
    { _ugsTimedMetadataId3Period = Nothing,
      _ugsInputLossAction = Nothing,
      _ugsTimedMetadataId3Frame = Nothing
    }

-- | Timed Metadata interval in seconds.
ugsTimedMetadataId3Period :: Lens' UdpGroupSettings (Maybe Natural)
ugsTimedMetadataId3Period = lens _ugsTimedMetadataId3Period (\s a -> s {_ugsTimedMetadataId3Period = a}) . mapping _Nat

-- | Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
ugsInputLossAction :: Lens' UdpGroupSettings (Maybe InputLossActionForUdpOut)
ugsInputLossAction = lens _ugsInputLossAction (\s a -> s {_ugsInputLossAction = a})

-- | Indicates ID3 frame that has the timecode.
ugsTimedMetadataId3Frame :: Lens' UdpGroupSettings (Maybe UdpTimedMetadataId3Frame)
ugsTimedMetadataId3Frame = lens _ugsTimedMetadataId3Frame (\s a -> s {_ugsTimedMetadataId3Frame = a})

instance FromJSON UdpGroupSettings where
  parseJSON =
    withObject
      "UdpGroupSettings"
      ( \x ->
          UdpGroupSettings'
            <$> (x .:? "timedMetadataId3Period")
            <*> (x .:? "inputLossAction")
            <*> (x .:? "timedMetadataId3Frame")
      )

instance Hashable UdpGroupSettings

instance NFData UdpGroupSettings

instance ToJSON UdpGroupSettings where
  toJSON UdpGroupSettings' {..} =
    object
      ( catMaybes
          [ ("timedMetadataId3Period" .=) <$> _ugsTimedMetadataId3Period,
            ("inputLossAction" .=) <$> _ugsInputLossAction,
            ("timedMetadataId3Frame" .=) <$> _ugsTimedMetadataId3Frame
          ]
      )
