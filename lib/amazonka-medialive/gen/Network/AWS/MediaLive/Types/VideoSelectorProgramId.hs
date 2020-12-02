{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorProgramId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorProgramId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Video Selector Program Id
--
-- /See:/ 'videoSelectorProgramId' smart constructor.
newtype VideoSelectorProgramId = VideoSelectorProgramId'
  { _vspiProgramId ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelectorProgramId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vspiProgramId' - Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
videoSelectorProgramId ::
  VideoSelectorProgramId
videoSelectorProgramId =
  VideoSelectorProgramId' {_vspiProgramId = Nothing}

-- | Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
vspiProgramId :: Lens' VideoSelectorProgramId (Maybe Natural)
vspiProgramId = lens _vspiProgramId (\s a -> s {_vspiProgramId = a}) . mapping _Nat

instance FromJSON VideoSelectorProgramId where
  parseJSON =
    withObject
      "VideoSelectorProgramId"
      (\x -> VideoSelectorProgramId' <$> (x .:? "programId"))

instance Hashable VideoSelectorProgramId

instance NFData VideoSelectorProgramId

instance ToJSON VideoSelectorProgramId where
  toJSON VideoSelectorProgramId' {..} =
    object (catMaybes [("programId" .=) <$> _vspiProgramId])
