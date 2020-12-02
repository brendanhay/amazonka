{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the output's video stream
--
-- /See:/ 'videoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { _vdHeightInPx :: !(Maybe Int),
    _vdWidthInPx :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdHeightInPx' - Height in pixels for the output
--
-- * 'vdWidthInPx' - Width in pixels for the output
videoDetail ::
  VideoDetail
videoDetail =
  VideoDetail' {_vdHeightInPx = Nothing, _vdWidthInPx = Nothing}

-- | Height in pixels for the output
vdHeightInPx :: Lens' VideoDetail (Maybe Int)
vdHeightInPx = lens _vdHeightInPx (\s a -> s {_vdHeightInPx = a})

-- | Width in pixels for the output
vdWidthInPx :: Lens' VideoDetail (Maybe Int)
vdWidthInPx = lens _vdWidthInPx (\s a -> s {_vdWidthInPx = a})

instance FromJSON VideoDetail where
  parseJSON =
    withObject
      "VideoDetail"
      ( \x ->
          VideoDetail' <$> (x .:? "heightInPx") <*> (x .:? "widthInPx")
      )

instance Hashable VideoDetail

instance NFData VideoDetail
