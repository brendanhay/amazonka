{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters for the technical segments returned by 'GetSegmentDetection' . For more information, see 'StartSegmentDetectionFilters' .
--
--
--
-- /See:/ 'startTechnicalCueDetectionFilter' smart constructor.
newtype StartTechnicalCueDetectionFilter = StartTechnicalCueDetectionFilter'
  { _stcdfMinSegmentConfidence ::
      Maybe Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTechnicalCueDetectionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stcdfMinSegmentConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value. If you don't specify @MinSegmentConfidence@ , @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
startTechnicalCueDetectionFilter ::
  StartTechnicalCueDetectionFilter
startTechnicalCueDetectionFilter =
  StartTechnicalCueDetectionFilter'
    { _stcdfMinSegmentConfidence =
        Nothing
    }

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value. If you don't specify @MinSegmentConfidence@ , @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
stcdfMinSegmentConfidence :: Lens' StartTechnicalCueDetectionFilter (Maybe Double)
stcdfMinSegmentConfidence = lens _stcdfMinSegmentConfidence (\s a -> s {_stcdfMinSegmentConfidence = a})

instance Hashable StartTechnicalCueDetectionFilter

instance NFData StartTechnicalCueDetectionFilter

instance ToJSON StartTechnicalCueDetectionFilter where
  toJSON StartTechnicalCueDetectionFilter' {..} =
    object
      ( catMaybes
          [("MinSegmentConfidence" .=) <$> _stcdfMinSegmentConfidence]
      )
