{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TechnicalCueSegment where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.TechnicalCueType

-- | Information about a technical cue segment. For more information, see 'SegmentDetection' .
--
--
--
-- /See:/ 'technicalCueSegment' smart constructor.
data TechnicalCueSegment = TechnicalCueSegment'
  { _tcsConfidence ::
      !(Maybe Double),
    _tcsType :: !(Maybe TechnicalCueType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TechnicalCueSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsConfidence' - The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- * 'tcsType' - The type of the technical cue.
technicalCueSegment ::
  TechnicalCueSegment
technicalCueSegment =
  TechnicalCueSegment'
    { _tcsConfidence = Nothing,
      _tcsType = Nothing
    }

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
tcsConfidence :: Lens' TechnicalCueSegment (Maybe Double)
tcsConfidence = lens _tcsConfidence (\s a -> s {_tcsConfidence = a})

-- | The type of the technical cue.
tcsType :: Lens' TechnicalCueSegment (Maybe TechnicalCueType)
tcsType = lens _tcsType (\s a -> s {_tcsType = a})

instance FromJSON TechnicalCueSegment where
  parseJSON =
    withObject
      "TechnicalCueSegment"
      ( \x ->
          TechnicalCueSegment' <$> (x .:? "Confidence") <*> (x .:? "Type")
      )

instance Hashable TechnicalCueSegment

instance NFData TechnicalCueSegment
