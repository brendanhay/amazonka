{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.BoundingBox
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.BoundingBox where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the bounding box around the label, face, text or personal protective equipment. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
--
-- The @top@ and @left@ values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
--
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
--
--
-- /See:/ 'boundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { _bbHeight :: !(Maybe Double),
    _bbLeft :: !(Maybe Double),
    _bbWidth :: !(Maybe Double),
    _bbTop :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BoundingBox' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbHeight' - Height of the bounding box as a ratio of the overall image height.
--
-- * 'bbLeft' - Left coordinate of the bounding box as a ratio of overall image width.
--
-- * 'bbWidth' - Width of the bounding box as a ratio of the overall image width.
--
-- * 'bbTop' - Top coordinate of the bounding box as a ratio of overall image height.
boundingBox ::
  BoundingBox
boundingBox =
  BoundingBox'
    { _bbHeight = Nothing,
      _bbLeft = Nothing,
      _bbWidth = Nothing,
      _bbTop = Nothing
    }

-- | Height of the bounding box as a ratio of the overall image height.
bbHeight :: Lens' BoundingBox (Maybe Double)
bbHeight = lens _bbHeight (\s a -> s {_bbHeight = a})

-- | Left coordinate of the bounding box as a ratio of overall image width.
bbLeft :: Lens' BoundingBox (Maybe Double)
bbLeft = lens _bbLeft (\s a -> s {_bbLeft = a})

-- | Width of the bounding box as a ratio of the overall image width.
bbWidth :: Lens' BoundingBox (Maybe Double)
bbWidth = lens _bbWidth (\s a -> s {_bbWidth = a})

-- | Top coordinate of the bounding box as a ratio of overall image height.
bbTop :: Lens' BoundingBox (Maybe Double)
bbTop = lens _bbTop (\s a -> s {_bbTop = a})

instance FromJSON BoundingBox where
  parseJSON =
    withObject
      "BoundingBox"
      ( \x ->
          BoundingBox'
            <$> (x .:? "Height")
            <*> (x .:? "Left")
            <*> (x .:? "Width")
            <*> (x .:? "Top")
      )

instance Hashable BoundingBox

instance NFData BoundingBox

instance ToJSON BoundingBox where
  toJSON BoundingBox' {..} =
    object
      ( catMaybes
          [ ("Height" .=) <$> _bbHeight,
            ("Left" .=) <$> _bbLeft,
            ("Width" .=) <$> _bbWidth,
            ("Top" .=) <$> _bbTop
          ]
      )
