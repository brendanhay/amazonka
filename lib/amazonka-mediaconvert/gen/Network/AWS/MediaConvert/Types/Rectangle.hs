{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Rectangle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Rectangle where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'rectangle' smart constructor.
data Rectangle = Rectangle'
  { _rHeight :: !(Maybe Nat),
    _rWidth :: !(Maybe Nat),
    _rX :: !(Maybe Nat),
    _rY :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rectangle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - Height of rectangle in pixels. Specify only even numbers.
--
-- * 'rWidth' - Width of rectangle in pixels. Specify only even numbers.
--
-- * 'rX' - The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
--
-- * 'rY' - The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
rectangle ::
  Rectangle
rectangle =
  Rectangle'
    { _rHeight = Nothing,
      _rWidth = Nothing,
      _rX = Nothing,
      _rY = Nothing
    }

-- | Height of rectangle in pixels. Specify only even numbers.
rHeight :: Lens' Rectangle (Maybe Natural)
rHeight = lens _rHeight (\s a -> s {_rHeight = a}) . mapping _Nat

-- | Width of rectangle in pixels. Specify only even numbers.
rWidth :: Lens' Rectangle (Maybe Natural)
rWidth = lens _rWidth (\s a -> s {_rWidth = a}) . mapping _Nat

-- | The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
rX :: Lens' Rectangle (Maybe Natural)
rX = lens _rX (\s a -> s {_rX = a}) . mapping _Nat

-- | The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
rY :: Lens' Rectangle (Maybe Natural)
rY = lens _rY (\s a -> s {_rY = a}) . mapping _Nat

instance FromJSON Rectangle where
  parseJSON =
    withObject
      "Rectangle"
      ( \x ->
          Rectangle'
            <$> (x .:? "height")
            <*> (x .:? "width")
            <*> (x .:? "x")
            <*> (x .:? "y")
      )

instance Hashable Rectangle

instance NFData Rectangle

instance ToJSON Rectangle where
  toJSON Rectangle' {..} =
    object
      ( catMaybes
          [ ("height" .=) <$> _rHeight,
            ("width" .=) <$> _rWidth,
            ("x" .=) <$> _rX,
            ("y" .=) <$> _rY
          ]
      )
