{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.DetectTextFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectTextFilters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | A set of optional parameters that you can use to set the criteria that the text must meet to be included in your response. @WordFilter@ looks at a word’s height, width, and minimum confidence. @RegionOfInterest@ lets you set a specific region of the image to look for text in.
--
--
--
-- /See:/ 'detectTextFilters' smart constructor.
data DetectTextFilters = DetectTextFilters'
  { _dtfRegionsOfInterest ::
      !(Maybe [RegionOfInterest]),
    _dtfWordFilter :: !(Maybe DetectionFilter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectTextFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtfRegionsOfInterest' - A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
--
-- * 'dtfWordFilter' - Undocumented member.
detectTextFilters ::
  DetectTextFilters
detectTextFilters =
  DetectTextFilters'
    { _dtfRegionsOfInterest = Nothing,
      _dtfWordFilter = Nothing
    }

-- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
dtfRegionsOfInterest :: Lens' DetectTextFilters [RegionOfInterest]
dtfRegionsOfInterest = lens _dtfRegionsOfInterest (\s a -> s {_dtfRegionsOfInterest = a}) . _Default . _Coerce

-- | Undocumented member.
dtfWordFilter :: Lens' DetectTextFilters (Maybe DetectionFilter)
dtfWordFilter = lens _dtfWordFilter (\s a -> s {_dtfWordFilter = a})

instance Hashable DetectTextFilters

instance NFData DetectTextFilters

instance ToJSON DetectTextFilters where
  toJSON DetectTextFilters' {..} =
    object
      ( catMaybes
          [ ("RegionsOfInterest" .=) <$> _dtfRegionsOfInterest,
            ("WordFilter" .=) <$> _dtfWordFilter
          ]
      )
