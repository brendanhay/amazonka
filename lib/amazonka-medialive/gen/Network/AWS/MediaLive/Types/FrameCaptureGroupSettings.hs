{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.Prelude

-- | Frame Capture Group Settings
--
-- /See:/ 'frameCaptureGroupSettings' smart constructor.
newtype FrameCaptureGroupSettings = FrameCaptureGroupSettings'
  { _fcgsDestination ::
      OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FrameCaptureGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcgsDestination' - The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
frameCaptureGroupSettings ::
  -- | 'fcgsDestination'
  OutputLocationRef ->
  FrameCaptureGroupSettings
frameCaptureGroupSettings pDestination_ =
  FrameCaptureGroupSettings' {_fcgsDestination = pDestination_}

-- | The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
fcgsDestination :: Lens' FrameCaptureGroupSettings OutputLocationRef
fcgsDestination = lens _fcgsDestination (\s a -> s {_fcgsDestination = a})

instance FromJSON FrameCaptureGroupSettings where
  parseJSON =
    withObject
      "FrameCaptureGroupSettings"
      (\x -> FrameCaptureGroupSettings' <$> (x .: "destination"))

instance Hashable FrameCaptureGroupSettings

instance NFData FrameCaptureGroupSettings

instance ToJSON FrameCaptureGroupSettings where
  toJSON FrameCaptureGroupSettings' {..} =
    object (catMaybes [Just ("destination" .= _fcgsDestination)])
