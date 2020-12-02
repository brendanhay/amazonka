{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.DetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of parameters that allow you to filter out certain results from your returned results.
--
--
--
-- /See:/ 'detectionFilter' smart constructor.
data DetectionFilter = DetectionFilter'
  { _dfMinBoundingBoxHeight ::
      !(Maybe Double),
    _dfMinBoundingBoxWidth :: !(Maybe Double),
    _dfMinConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfMinBoundingBoxHeight' - Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
--
-- * 'dfMinBoundingBoxWidth' - Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
--
-- * 'dfMinConfidence' - Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
detectionFilter ::
  DetectionFilter
detectionFilter =
  DetectionFilter'
    { _dfMinBoundingBoxHeight = Nothing,
      _dfMinBoundingBoxWidth = Nothing,
      _dfMinConfidence = Nothing
    }

-- | Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
dfMinBoundingBoxHeight :: Lens' DetectionFilter (Maybe Double)
dfMinBoundingBoxHeight = lens _dfMinBoundingBoxHeight (\s a -> s {_dfMinBoundingBoxHeight = a})

-- | Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
dfMinBoundingBoxWidth :: Lens' DetectionFilter (Maybe Double)
dfMinBoundingBoxWidth = lens _dfMinBoundingBoxWidth (\s a -> s {_dfMinBoundingBoxWidth = a})

-- | Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
dfMinConfidence :: Lens' DetectionFilter (Maybe Double)
dfMinConfidence = lens _dfMinConfidence (\s a -> s {_dfMinConfidence = a})

instance Hashable DetectionFilter

instance NFData DetectionFilter

instance ToJSON DetectionFilter where
  toJSON DetectionFilter' {..} =
    object
      ( catMaybes
          [ ("MinBoundingBoxHeight" .=) <$> _dfMinBoundingBoxHeight,
            ("MinBoundingBoxWidth" .=) <$> _dfMinBoundingBoxWidth,
            ("MinConfidence" .=) <$> _dfMinConfidence
          ]
      )
