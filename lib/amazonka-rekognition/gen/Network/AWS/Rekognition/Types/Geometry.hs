{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Geometry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Geometry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Point

-- | Information about where an object ('DetectCustomLabels' ) or text ('DetectText' ) is located on an image.
--
--
--
-- /See:/ 'geometry' smart constructor.
data Geometry = Geometry'
  { _gBoundingBox :: !(Maybe BoundingBox),
    _gPolygon :: !(Maybe [Point])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Geometry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gBoundingBox' - An axis-aligned coarse representation of the detected item's location on the image.
--
-- * 'gPolygon' - Within the bounding box, a fine-grained polygon around the detected item.
geometry ::
  Geometry
geometry = Geometry' {_gBoundingBox = Nothing, _gPolygon = Nothing}

-- | An axis-aligned coarse representation of the detected item's location on the image.
gBoundingBox :: Lens' Geometry (Maybe BoundingBox)
gBoundingBox = lens _gBoundingBox (\s a -> s {_gBoundingBox = a})

-- | Within the bounding box, a fine-grained polygon around the detected item.
gPolygon :: Lens' Geometry [Point]
gPolygon = lens _gPolygon (\s a -> s {_gPolygon = a}) . _Default . _Coerce

instance FromJSON Geometry where
  parseJSON =
    withObject
      "Geometry"
      ( \x ->
          Geometry'
            <$> (x .:? "BoundingBox") <*> (x .:? "Polygon" .!= mempty)
      )

instance Hashable Geometry

instance NFData Geometry
