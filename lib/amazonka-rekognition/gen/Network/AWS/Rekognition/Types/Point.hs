{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Point
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Point where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.
--
--
-- An array of @Point@ objects, @Polygon@ , is returned by 'DetectText' and by 'DetectCustomLabels' . @Polygon@ represents a fine-grained polygon around a detected item. For more information, see Geometry in the Amazon Rekognition Developer Guide.
--
--
-- /See:/ 'point' smart constructor.
data Point = Point' {_pX :: !(Maybe Double), _pY :: !(Maybe Double)}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Point' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pX' - The value of the X coordinate for a point on a @Polygon@ .
--
-- * 'pY' - The value of the Y coordinate for a point on a @Polygon@ .
point ::
  Point
point = Point' {_pX = Nothing, _pY = Nothing}

-- | The value of the X coordinate for a point on a @Polygon@ .
pX :: Lens' Point (Maybe Double)
pX = lens _pX (\s a -> s {_pX = a})

-- | The value of the Y coordinate for a point on a @Polygon@ .
pY :: Lens' Point (Maybe Double)
pY = lens _pY (\s a -> s {_pY = a})

instance FromJSON Point where
  parseJSON =
    withObject
      "Point"
      (\x -> Point' <$> (x .:? "X") <*> (x .:? "Y"))

instance Hashable Point

instance NFData Point
