{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionOffset where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specify the offset between the upper-left corner of the video frame and the top left corner of the overlay.
--
-- /See:/ 'motionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { _miioImageX ::
      !(Maybe Nat),
    _miioImageY :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MotionImageInsertionOffset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miioImageX' - Set the distance, in pixels, between the overlay and the left edge of the video frame.
--
-- * 'miioImageY' - Set the distance, in pixels, between the overlay and the top edge of the video frame.
motionImageInsertionOffset ::
  MotionImageInsertionOffset
motionImageInsertionOffset =
  MotionImageInsertionOffset'
    { _miioImageX = Nothing,
      _miioImageY = Nothing
    }

-- | Set the distance, in pixels, between the overlay and the left edge of the video frame.
miioImageX :: Lens' MotionImageInsertionOffset (Maybe Natural)
miioImageX = lens _miioImageX (\s a -> s {_miioImageX = a}) . mapping _Nat

-- | Set the distance, in pixels, between the overlay and the top edge of the video frame.
miioImageY :: Lens' MotionImageInsertionOffset (Maybe Natural)
miioImageY = lens _miioImageY (\s a -> s {_miioImageY = a}) . mapping _Nat

instance FromJSON MotionImageInsertionOffset where
  parseJSON =
    withObject
      "MotionImageInsertionOffset"
      ( \x ->
          MotionImageInsertionOffset'
            <$> (x .:? "imageX") <*> (x .:? "imageY")
      )

instance Hashable MotionImageInsertionOffset

instance NFData MotionImageInsertionOffset

instance ToJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset' {..} =
    object
      ( catMaybes
          [("imageX" .=) <$> _miioImageX, ("imageY" .=) <$> _miioImageY]
      )
