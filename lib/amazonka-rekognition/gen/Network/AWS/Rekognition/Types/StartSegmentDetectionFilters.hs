{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartSegmentDetectionFilters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.StartShotDetectionFilter
import Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter

-- | Filters applied to the technical cue or shot detection segments. For more information, see 'StartSegmentDetection' .
--
--
--
-- /See:/ 'startSegmentDetectionFilters' smart constructor.
data StartSegmentDetectionFilters = StartSegmentDetectionFilters'
  { _ssdfTechnicalCueFilter ::
      !( Maybe
           StartTechnicalCueDetectionFilter
       ),
    _ssdfShotFilter ::
      !(Maybe StartShotDetectionFilter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSegmentDetectionFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdfTechnicalCueFilter' - Filters that are specific to technical cues.
--
-- * 'ssdfShotFilter' - Filters that are specific to shot detections.
startSegmentDetectionFilters ::
  StartSegmentDetectionFilters
startSegmentDetectionFilters =
  StartSegmentDetectionFilters'
    { _ssdfTechnicalCueFilter = Nothing,
      _ssdfShotFilter = Nothing
    }

-- | Filters that are specific to technical cues.
ssdfTechnicalCueFilter :: Lens' StartSegmentDetectionFilters (Maybe StartTechnicalCueDetectionFilter)
ssdfTechnicalCueFilter = lens _ssdfTechnicalCueFilter (\s a -> s {_ssdfTechnicalCueFilter = a})

-- | Filters that are specific to shot detections.
ssdfShotFilter :: Lens' StartSegmentDetectionFilters (Maybe StartShotDetectionFilter)
ssdfShotFilter = lens _ssdfShotFilter (\s a -> s {_ssdfShotFilter = a})

instance Hashable StartSegmentDetectionFilters

instance NFData StartSegmentDetectionFilters

instance ToJSON StartSegmentDetectionFilters where
  toJSON StartSegmentDetectionFilters' {..} =
    object
      ( catMaybes
          [ ("TechnicalCueFilter" .=) <$> _ssdfTechnicalCueFilter,
            ("ShotFilter" .=) <$> _ssdfShotFilter
          ]
      )
