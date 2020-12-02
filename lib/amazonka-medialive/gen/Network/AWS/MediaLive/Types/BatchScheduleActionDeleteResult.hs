{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import Network.AWS.Prelude

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'batchScheduleActionDeleteResult' smart constructor.
newtype BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'
  { _bsadrScheduleActions ::
      [ScheduleAction]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionDeleteResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsadrScheduleActions' - List of actions that have been deleted from the schedule.
batchScheduleActionDeleteResult ::
  BatchScheduleActionDeleteResult
batchScheduleActionDeleteResult =
  BatchScheduleActionDeleteResult' {_bsadrScheduleActions = mempty}

-- | List of actions that have been deleted from the schedule.
bsadrScheduleActions :: Lens' BatchScheduleActionDeleteResult [ScheduleAction]
bsadrScheduleActions = lens _bsadrScheduleActions (\s a -> s {_bsadrScheduleActions = a}) . _Coerce

instance FromJSON BatchScheduleActionDeleteResult where
  parseJSON =
    withObject
      "BatchScheduleActionDeleteResult"
      ( \x ->
          BatchScheduleActionDeleteResult'
            <$> (x .:? "scheduleActions" .!= mempty)
      )

instance Hashable BatchScheduleActionDeleteResult

instance NFData BatchScheduleActionDeleteResult
