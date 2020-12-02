{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceSpecification where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ReservationCodec
import Network.AWS.MediaLive.Types.ReservationMaximumBitrate
import Network.AWS.MediaLive.Types.ReservationMaximumFramerate
import Network.AWS.MediaLive.Types.ReservationResolution
import Network.AWS.MediaLive.Types.ReservationResourceType
import Network.AWS.MediaLive.Types.ReservationSpecialFeature
import Network.AWS.MediaLive.Types.ReservationVideoQuality
import Network.AWS.Prelude

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'reservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'
  { _rrsVideoQuality ::
      !( Maybe
           ReservationVideoQuality
       ),
    _rrsMaximumFramerate ::
      !( Maybe
           ReservationMaximumFramerate
       ),
    _rrsResourceType ::
      !( Maybe
           ReservationResourceType
       ),
    _rrsResolution ::
      !( Maybe
           ReservationResolution
       ),
    _rrsCodec ::
      !(Maybe ReservationCodec),
    _rrsSpecialFeature ::
      !( Maybe
           ReservationSpecialFeature
       ),
    _rrsChannelClass ::
      !(Maybe ChannelClass),
    _rrsMaximumBitrate ::
      !( Maybe
           ReservationMaximumBitrate
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationResourceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsVideoQuality' - Video quality, e.g. 'STANDARD' (Outputs only)
--
-- * 'rrsMaximumFramerate' - Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
--
-- * 'rrsResourceType' - Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- * 'rrsResolution' - Resolution, e.g. 'HD'
--
-- * 'rrsCodec' - Codec, e.g. 'AVC'
--
-- * 'rrsSpecialFeature' - Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
--
-- * 'rrsChannelClass' - Channel class, e.g. 'STANDARD'
--
-- * 'rrsMaximumBitrate' - Maximum bitrate, e.g. 'MAX_20_MBPS'
reservationResourceSpecification ::
  ReservationResourceSpecification
reservationResourceSpecification =
  ReservationResourceSpecification'
    { _rrsVideoQuality = Nothing,
      _rrsMaximumFramerate = Nothing,
      _rrsResourceType = Nothing,
      _rrsResolution = Nothing,
      _rrsCodec = Nothing,
      _rrsSpecialFeature = Nothing,
      _rrsChannelClass = Nothing,
      _rrsMaximumBitrate = Nothing
    }

-- | Video quality, e.g. 'STANDARD' (Outputs only)
rrsVideoQuality :: Lens' ReservationResourceSpecification (Maybe ReservationVideoQuality)
rrsVideoQuality = lens _rrsVideoQuality (\s a -> s {_rrsVideoQuality = a})

-- | Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
rrsMaximumFramerate :: Lens' ReservationResourceSpecification (Maybe ReservationMaximumFramerate)
rrsMaximumFramerate = lens _rrsMaximumFramerate (\s a -> s {_rrsMaximumFramerate = a})

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
rrsResourceType :: Lens' ReservationResourceSpecification (Maybe ReservationResourceType)
rrsResourceType = lens _rrsResourceType (\s a -> s {_rrsResourceType = a})

-- | Resolution, e.g. 'HD'
rrsResolution :: Lens' ReservationResourceSpecification (Maybe ReservationResolution)
rrsResolution = lens _rrsResolution (\s a -> s {_rrsResolution = a})

-- | Codec, e.g. 'AVC'
rrsCodec :: Lens' ReservationResourceSpecification (Maybe ReservationCodec)
rrsCodec = lens _rrsCodec (\s a -> s {_rrsCodec = a})

-- | Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
rrsSpecialFeature :: Lens' ReservationResourceSpecification (Maybe ReservationSpecialFeature)
rrsSpecialFeature = lens _rrsSpecialFeature (\s a -> s {_rrsSpecialFeature = a})

-- | Channel class, e.g. 'STANDARD'
rrsChannelClass :: Lens' ReservationResourceSpecification (Maybe ChannelClass)
rrsChannelClass = lens _rrsChannelClass (\s a -> s {_rrsChannelClass = a})

-- | Maximum bitrate, e.g. 'MAX_20_MBPS'
rrsMaximumBitrate :: Lens' ReservationResourceSpecification (Maybe ReservationMaximumBitrate)
rrsMaximumBitrate = lens _rrsMaximumBitrate (\s a -> s {_rrsMaximumBitrate = a})

instance FromJSON ReservationResourceSpecification where
  parseJSON =
    withObject
      "ReservationResourceSpecification"
      ( \x ->
          ReservationResourceSpecification'
            <$> (x .:? "videoQuality")
            <*> (x .:? "maximumFramerate")
            <*> (x .:? "resourceType")
            <*> (x .:? "resolution")
            <*> (x .:? "codec")
            <*> (x .:? "specialFeature")
            <*> (x .:? "channelClass")
            <*> (x .:? "maximumBitrate")
      )

instance Hashable ReservationResourceSpecification

instance NFData ReservationResourceSpecification
