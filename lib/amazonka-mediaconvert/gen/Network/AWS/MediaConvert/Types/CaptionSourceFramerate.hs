{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceFramerate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
--
-- /See:/ 'captionSourceFramerate' smart constructor.
data CaptionSourceFramerate = CaptionSourceFramerate'
  { _csfFramerateDenominator ::
      !(Maybe Nat),
    _csfFramerateNumerator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSourceFramerate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfFramerateDenominator' - Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
--
-- * 'csfFramerateNumerator' - Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
captionSourceFramerate ::
  CaptionSourceFramerate
captionSourceFramerate =
  CaptionSourceFramerate'
    { _csfFramerateDenominator = Nothing,
      _csfFramerateNumerator = Nothing
    }

-- | Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
csfFramerateDenominator :: Lens' CaptionSourceFramerate (Maybe Natural)
csfFramerateDenominator = lens _csfFramerateDenominator (\s a -> s {_csfFramerateDenominator = a}) . mapping _Nat

-- | Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
csfFramerateNumerator :: Lens' CaptionSourceFramerate (Maybe Natural)
csfFramerateNumerator = lens _csfFramerateNumerator (\s a -> s {_csfFramerateNumerator = a}) . mapping _Nat

instance FromJSON CaptionSourceFramerate where
  parseJSON =
    withObject
      "CaptionSourceFramerate"
      ( \x ->
          CaptionSourceFramerate'
            <$> (x .:? "framerateDenominator") <*> (x .:? "framerateNumerator")
      )

instance Hashable CaptionSourceFramerate

instance NFData CaptionSourceFramerate

instance ToJSON CaptionSourceFramerate where
  toJSON CaptionSourceFramerate' {..} =
    object
      ( catMaybes
          [ ("framerateDenominator" .=) <$> _csfFramerateDenominator,
            ("framerateNumerator" .=) <$> _csfFramerateNumerator
          ]
      )
