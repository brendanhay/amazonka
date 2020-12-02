{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TextDetectionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextDetectionResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.TextDetection

-- | Information about text detected in a video. Incudes the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
--
--
-- /See:/ 'textDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { _tdrTextDetection ::
      !(Maybe TextDetection),
    _tdrTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TextDetectionResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdrTextDetection' - Details about text detected in a video.
--
-- * 'tdrTimestamp' - The time, in milliseconds from the start of the video, that the text was detected.
textDetectionResult ::
  TextDetectionResult
textDetectionResult =
  TextDetectionResult'
    { _tdrTextDetection = Nothing,
      _tdrTimestamp = Nothing
    }

-- | Details about text detected in a video.
tdrTextDetection :: Lens' TextDetectionResult (Maybe TextDetection)
tdrTextDetection = lens _tdrTextDetection (\s a -> s {_tdrTextDetection = a})

-- | The time, in milliseconds from the start of the video, that the text was detected.
tdrTimestamp :: Lens' TextDetectionResult (Maybe Integer)
tdrTimestamp = lens _tdrTimestamp (\s a -> s {_tdrTimestamp = a})

instance FromJSON TextDetectionResult where
  parseJSON =
    withObject
      "TextDetectionResult"
      ( \x ->
          TextDetectionResult'
            <$> (x .:? "TextDetection") <*> (x .:? "Timestamp")
      )

instance Hashable TextDetectionResult

instance NFData TextDetectionResult
