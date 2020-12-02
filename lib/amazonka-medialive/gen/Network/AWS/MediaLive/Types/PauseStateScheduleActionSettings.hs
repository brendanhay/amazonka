{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
import Network.AWS.Prelude

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'pauseStateScheduleActionSettings' smart constructor.
newtype PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'
  { _pssasPipelines ::
      Maybe
        [PipelinePauseStateSettings]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PauseStateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssasPipelines' - Undocumented member.
pauseStateScheduleActionSettings ::
  PauseStateScheduleActionSettings
pauseStateScheduleActionSettings =
  PauseStateScheduleActionSettings' {_pssasPipelines = Nothing}

-- | Undocumented member.
pssasPipelines :: Lens' PauseStateScheduleActionSettings [PipelinePauseStateSettings]
pssasPipelines = lens _pssasPipelines (\s a -> s {_pssasPipelines = a}) . _Default . _Coerce

instance FromJSON PauseStateScheduleActionSettings where
  parseJSON =
    withObject
      "PauseStateScheduleActionSettings"
      ( \x ->
          PauseStateScheduleActionSettings'
            <$> (x .:? "pipelines" .!= mempty)
      )

instance Hashable PauseStateScheduleActionSettings

instance NFData PauseStateScheduleActionSettings

instance ToJSON PauseStateScheduleActionSettings where
  toJSON PauseStateScheduleActionSettings' {..} =
    object (catMaybes [("pipelines" .=) <$> _pssasPipelines])
