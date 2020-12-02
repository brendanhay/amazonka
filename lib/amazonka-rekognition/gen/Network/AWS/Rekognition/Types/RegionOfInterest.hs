{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.RegionOfInterest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.RegionOfInterest where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Specifies a location within the frame that Rekognition checks for text. Uses a @BoundingBox@ object to set a region of the screen.
--
--
-- A word is included in the region if the word is more than half in that region. If there is more than one region, the word will be compared with all regions of the screen. Any word more than half in a region is kept in the results.
--
--
-- /See:/ 'regionOfInterest' smart constructor.
newtype RegionOfInterest = RegionOfInterest'
  { _roiBoundingBox ::
      Maybe BoundingBox
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegionOfInterest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roiBoundingBox' - The box representing a region of interest on screen.
regionOfInterest ::
  RegionOfInterest
regionOfInterest = RegionOfInterest' {_roiBoundingBox = Nothing}

-- | The box representing a region of interest on screen.
roiBoundingBox :: Lens' RegionOfInterest (Maybe BoundingBox)
roiBoundingBox = lens _roiBoundingBox (\s a -> s {_roiBoundingBox = a})

instance Hashable RegionOfInterest

instance NFData RegionOfInterest

instance ToJSON RegionOfInterest where
  toJSON RegionOfInterest' {..} =
    object (catMaybes [("BoundingBox" .=) <$> _roiBoundingBox])
