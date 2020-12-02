{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.VideoMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
--
--
-- /See:/ 'videoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { _vmFrameRate ::
      !(Maybe Double),
    _vmFormat :: !(Maybe Text),
    _vmCodec :: !(Maybe Text),
    _vmFrameHeight :: !(Maybe Nat),
    _vmDurationMillis :: !(Maybe Nat),
    _vmFrameWidth :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmFrameRate' - Number of frames per second in the video.
--
-- * 'vmFormat' - Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- * 'vmCodec' - Type of compression used in the analyzed video.
--
-- * 'vmFrameHeight' - Vertical pixel dimension of the video.
--
-- * 'vmDurationMillis' - Length of the video in milliseconds.
--
-- * 'vmFrameWidth' - Horizontal pixel dimension of the video.
videoMetadata ::
  VideoMetadata
videoMetadata =
  VideoMetadata'
    { _vmFrameRate = Nothing,
      _vmFormat = Nothing,
      _vmCodec = Nothing,
      _vmFrameHeight = Nothing,
      _vmDurationMillis = Nothing,
      _vmFrameWidth = Nothing
    }

-- | Number of frames per second in the video.
vmFrameRate :: Lens' VideoMetadata (Maybe Double)
vmFrameRate = lens _vmFrameRate (\s a -> s {_vmFrameRate = a})

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
vmFormat :: Lens' VideoMetadata (Maybe Text)
vmFormat = lens _vmFormat (\s a -> s {_vmFormat = a})

-- | Type of compression used in the analyzed video.
vmCodec :: Lens' VideoMetadata (Maybe Text)
vmCodec = lens _vmCodec (\s a -> s {_vmCodec = a})

-- | Vertical pixel dimension of the video.
vmFrameHeight :: Lens' VideoMetadata (Maybe Natural)
vmFrameHeight = lens _vmFrameHeight (\s a -> s {_vmFrameHeight = a}) . mapping _Nat

-- | Length of the video in milliseconds.
vmDurationMillis :: Lens' VideoMetadata (Maybe Natural)
vmDurationMillis = lens _vmDurationMillis (\s a -> s {_vmDurationMillis = a}) . mapping _Nat

-- | Horizontal pixel dimension of the video.
vmFrameWidth :: Lens' VideoMetadata (Maybe Natural)
vmFrameWidth = lens _vmFrameWidth (\s a -> s {_vmFrameWidth = a}) . mapping _Nat

instance FromJSON VideoMetadata where
  parseJSON =
    withObject
      "VideoMetadata"
      ( \x ->
          VideoMetadata'
            <$> (x .:? "FrameRate")
            <*> (x .:? "Format")
            <*> (x .:? "Codec")
            <*> (x .:? "FrameHeight")
            <*> (x .:? "DurationMillis")
            <*> (x .:? "FrameWidth")
      )

instance Hashable VideoMetadata

instance NFData VideoMetadata
