{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Landmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Landmark where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.LandmarkType

-- | Indicates the location of the landmark on the face.
--
--
--
-- /See:/ 'landmark' smart constructor.
data Landmark = Landmark'
  { _lType :: !(Maybe LandmarkType),
    _lX :: !(Maybe Double),
    _lY :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Landmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lType' - Type of landmark.
--
-- * 'lX' - The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
--
-- * 'lY' - The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
landmark ::
  Landmark
landmark =
  Landmark' {_lType = Nothing, _lX = Nothing, _lY = Nothing}

-- | Type of landmark.
lType :: Lens' Landmark (Maybe LandmarkType)
lType = lens _lType (\s a -> s {_lType = a})

-- | The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
lX :: Lens' Landmark (Maybe Double)
lX = lens _lX (\s a -> s {_lX = a})

-- | The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
lY :: Lens' Landmark (Maybe Double)
lY = lens _lY (\s a -> s {_lY = a})

instance FromJSON Landmark where
  parseJSON =
    withObject
      "Landmark"
      ( \x ->
          Landmark' <$> (x .:? "Type") <*> (x .:? "X") <*> (x .:? "Y")
      )

instance Hashable Landmark

instance NFData Landmark
