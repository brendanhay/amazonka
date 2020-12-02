{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartTextDetectionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartTextDetectionFilters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | Set of optional parameters that let you set the criteria text must meet to be included in your response. @WordFilter@ looks at a word's height, width and minimum confidence. @RegionOfInterest@ lets you set a specific region of the screen to look for text in.
--
--
--
-- /See:/ 'startTextDetectionFilters' smart constructor.
data StartTextDetectionFilters = StartTextDetectionFilters'
  { _stdfRegionsOfInterest ::
      !(Maybe [RegionOfInterest]),
    _stdfWordFilter ::
      !(Maybe DetectionFilter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTextDetectionFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdfRegionsOfInterest' - Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
--
-- * 'stdfWordFilter' - Filters focusing on qualities of the text, such as confidence or size.
startTextDetectionFilters ::
  StartTextDetectionFilters
startTextDetectionFilters =
  StartTextDetectionFilters'
    { _stdfRegionsOfInterest = Nothing,
      _stdfWordFilter = Nothing
    }

-- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
stdfRegionsOfInterest :: Lens' StartTextDetectionFilters [RegionOfInterest]
stdfRegionsOfInterest = lens _stdfRegionsOfInterest (\s a -> s {_stdfRegionsOfInterest = a}) . _Default . _Coerce

-- | Filters focusing on qualities of the text, such as confidence or size.
stdfWordFilter :: Lens' StartTextDetectionFilters (Maybe DetectionFilter)
stdfWordFilter = lens _stdfWordFilter (\s a -> s {_stdfWordFilter = a})

instance Hashable StartTextDetectionFilters

instance NFData StartTextDetectionFilters

instance ToJSON StartTextDetectionFilters where
  toJSON StartTextDetectionFilters' {..} =
    object
      ( catMaybes
          [ ("RegionsOfInterest" .=) <$> _stdfRegionsOfInterest,
            ("WordFilter" .=) <$> _stdfWordFilter
          ]
      )
