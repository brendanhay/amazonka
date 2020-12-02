{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartShotDetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartShotDetectionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters for the shot detection segments returned by @GetSegmentDetection@ . For more information, see 'StartSegmentDetectionFilters' .
--
--
--
-- /See:/ 'startShotDetectionFilter' smart constructor.
newtype StartShotDetectionFilter = StartShotDetectionFilter'
  { _ssdfMinSegmentConfidence ::
      Maybe Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartShotDetectionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdfMinSegmentConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value. If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
startShotDetectionFilter ::
  StartShotDetectionFilter
startShotDetectionFilter =
  StartShotDetectionFilter' {_ssdfMinSegmentConfidence = Nothing}

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value. If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
ssdfMinSegmentConfidence :: Lens' StartShotDetectionFilter (Maybe Double)
ssdfMinSegmentConfidence = lens _ssdfMinSegmentConfidence (\s a -> s {_ssdfMinSegmentConfidence = a})

instance Hashable StartShotDetectionFilter

instance NFData StartShotDetectionFilter

instance ToJSON StartShotDetectionFilter where
  toJSON StartShotDetectionFilter' {..} =
    object
      ( catMaybes
          [("MinSegmentConfidence" .=) <$> _ssdfMinSegmentConfidence]
      )
