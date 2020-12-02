{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For motion overlays that don't have a built-in frame rate, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. The overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /See:/ 'motionImageInsertionFramerate' smart constructor.
data MotionImageInsertionFramerate = MotionImageInsertionFramerate'
  { _miifFramerateDenominator ::
      !(Maybe Nat),
    _miifFramerateNumerator ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MotionImageInsertionFramerate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miifFramerateDenominator' - The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
--
-- * 'miifFramerateNumerator' - The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
motionImageInsertionFramerate ::
  MotionImageInsertionFramerate
motionImageInsertionFramerate =
  MotionImageInsertionFramerate'
    { _miifFramerateDenominator =
        Nothing,
      _miifFramerateNumerator = Nothing
    }

-- | The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
miifFramerateDenominator :: Lens' MotionImageInsertionFramerate (Maybe Natural)
miifFramerateDenominator = lens _miifFramerateDenominator (\s a -> s {_miifFramerateDenominator = a}) . mapping _Nat

-- | The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
miifFramerateNumerator :: Lens' MotionImageInsertionFramerate (Maybe Natural)
miifFramerateNumerator = lens _miifFramerateNumerator (\s a -> s {_miifFramerateNumerator = a}) . mapping _Nat

instance FromJSON MotionImageInsertionFramerate where
  parseJSON =
    withObject
      "MotionImageInsertionFramerate"
      ( \x ->
          MotionImageInsertionFramerate'
            <$> (x .:? "framerateDenominator") <*> (x .:? "framerateNumerator")
      )

instance Hashable MotionImageInsertionFramerate

instance NFData MotionImageInsertionFramerate

instance ToJSON MotionImageInsertionFramerate where
  toJSON MotionImageInsertionFramerate' {..} =
    object
      ( catMaybes
          [ ("framerateDenominator" .=) <$> _miifFramerateDenominator,
            ("framerateNumerator" .=) <$> _miifFramerateNumerator
          ]
      )
