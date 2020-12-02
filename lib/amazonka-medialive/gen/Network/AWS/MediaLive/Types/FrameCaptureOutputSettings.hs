{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureOutputSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Frame Capture Output Settings
--
-- /See:/ 'frameCaptureOutputSettings' smart constructor.
newtype FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { _fcosNameModifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FrameCaptureOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcosNameModifier' - Required if the output group contains more than one output. This modifier forms part of the output file name.
frameCaptureOutputSettings ::
  FrameCaptureOutputSettings
frameCaptureOutputSettings =
  FrameCaptureOutputSettings' {_fcosNameModifier = Nothing}

-- | Required if the output group contains more than one output. This modifier forms part of the output file name.
fcosNameModifier :: Lens' FrameCaptureOutputSettings (Maybe Text)
fcosNameModifier = lens _fcosNameModifier (\s a -> s {_fcosNameModifier = a})

instance FromJSON FrameCaptureOutputSettings where
  parseJSON =
    withObject
      "FrameCaptureOutputSettings"
      (\x -> FrameCaptureOutputSettings' <$> (x .:? "nameModifier"))

instance Hashable FrameCaptureOutputSettings

instance NFData FrameCaptureOutputSettings

instance ToJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings' {..} =
    object (catMaybes [("nameModifier" .=) <$> _fcosNameModifier])
