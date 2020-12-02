{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ShotSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ShotSegment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a shot detection segment detected in a video. For more information, see 'SegmentDetection' .
--
--
--
-- /See:/ 'shotSegment' smart constructor.
data ShotSegment = ShotSegment'
  { _ssConfidence :: !(Maybe Double),
    _ssIndex :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShotSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssConfidence' - The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- * 'ssIndex' - An Identifier for a shot detection segment detected in a video.
shotSegment ::
  ShotSegment
shotSegment =
  ShotSegment' {_ssConfidence = Nothing, _ssIndex = Nothing}

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
ssConfidence :: Lens' ShotSegment (Maybe Double)
ssConfidence = lens _ssConfidence (\s a -> s {_ssConfidence = a})

-- | An Identifier for a shot detection segment detected in a video.
ssIndex :: Lens' ShotSegment (Maybe Natural)
ssIndex = lens _ssIndex (\s a -> s {_ssIndex = a}) . mapping _Nat

instance FromJSON ShotSegment where
  parseJSON =
    withObject
      "ShotSegment"
      (\x -> ShotSegment' <$> (x .:? "Confidence") <*> (x .:? "Index"))

instance Hashable ShotSegment

instance NFData ShotSegment
