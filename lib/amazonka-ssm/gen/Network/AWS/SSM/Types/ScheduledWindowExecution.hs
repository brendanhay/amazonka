{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ScheduledWindowExecution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a scheduled execution for a maintenance window.
--
--
--
-- /See:/ 'scheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { _sweExecutionTime ::
      !(Maybe Text),
    _sweName :: !(Maybe Text),
    _sweWindowId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sweExecutionTime' - The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
--
-- * 'sweName' - The name of the maintenance window to be run.
--
-- * 'sweWindowId' - The ID of the maintenance window to be run.
scheduledWindowExecution ::
  ScheduledWindowExecution
scheduledWindowExecution =
  ScheduledWindowExecution'
    { _sweExecutionTime = Nothing,
      _sweName = Nothing,
      _sweWindowId = Nothing
    }

-- | The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
sweExecutionTime :: Lens' ScheduledWindowExecution (Maybe Text)
sweExecutionTime = lens _sweExecutionTime (\s a -> s {_sweExecutionTime = a})

-- | The name of the maintenance window to be run.
sweName :: Lens' ScheduledWindowExecution (Maybe Text)
sweName = lens _sweName (\s a -> s {_sweName = a})

-- | The ID of the maintenance window to be run.
sweWindowId :: Lens' ScheduledWindowExecution (Maybe Text)
sweWindowId = lens _sweWindowId (\s a -> s {_sweWindowId = a})

instance FromJSON ScheduledWindowExecution where
  parseJSON =
    withObject
      "ScheduledWindowExecution"
      ( \x ->
          ScheduledWindowExecution'
            <$> (x .:? "ExecutionTime") <*> (x .:? "Name") <*> (x .:? "WindowId")
      )

instance Hashable ScheduledWindowExecution

instance NFData ScheduledWindowExecution
