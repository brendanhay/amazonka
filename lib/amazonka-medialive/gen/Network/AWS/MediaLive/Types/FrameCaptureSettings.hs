{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
import Network.AWS.Prelude

-- | Frame Capture Settings
--
-- /See:/ 'frameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { _fcsCaptureIntervalUnits ::
      !(Maybe FrameCaptureIntervalUnit),
    _fcsCaptureInterval :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FrameCaptureSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcsCaptureIntervalUnits' - Unit for the frame capture interval.
--
-- * 'fcsCaptureInterval' - The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
frameCaptureSettings ::
  -- | 'fcsCaptureInterval'
  Natural ->
  FrameCaptureSettings
frameCaptureSettings pCaptureInterval_ =
  FrameCaptureSettings'
    { _fcsCaptureIntervalUnits = Nothing,
      _fcsCaptureInterval = _Nat # pCaptureInterval_
    }

-- | Unit for the frame capture interval.
fcsCaptureIntervalUnits :: Lens' FrameCaptureSettings (Maybe FrameCaptureIntervalUnit)
fcsCaptureIntervalUnits = lens _fcsCaptureIntervalUnits (\s a -> s {_fcsCaptureIntervalUnits = a})

-- | The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
fcsCaptureInterval :: Lens' FrameCaptureSettings Natural
fcsCaptureInterval = lens _fcsCaptureInterval (\s a -> s {_fcsCaptureInterval = a}) . _Nat

instance FromJSON FrameCaptureSettings where
  parseJSON =
    withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            <$> (x .:? "captureIntervalUnits") <*> (x .: "captureInterval")
      )

instance Hashable FrameCaptureSettings

instance NFData FrameCaptureSettings

instance ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    object
      ( catMaybes
          [ ("captureIntervalUnits" .=) <$> _fcsCaptureIntervalUnits,
            Just ("captureInterval" .= _fcsCaptureInterval)
          ]
      )
