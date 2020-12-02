{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Trigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Trigger where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a specific trigger.
--
--
--
-- /See:/ 'trigger' smart constructor.
data Trigger = Trigger'
  { _triWorkflowName :: !(Maybe Text),
    _triState :: !(Maybe TriggerState),
    _triActions :: !(Maybe [Action]),
    _triSchedule :: !(Maybe Text),
    _triPredicate :: !(Maybe Predicate),
    _triName :: !(Maybe Text),
    _triId :: !(Maybe Text),
    _triType :: !(Maybe TriggerType),
    _triDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'triWorkflowName' - The name of the workflow associated with the trigger.
--
-- * 'triState' - The current state of the trigger.
--
-- * 'triActions' - The actions initiated by this trigger.
--
-- * 'triSchedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'triPredicate' - The predicate of this trigger, which defines when it will fire.
--
-- * 'triName' - The name of the trigger.
--
-- * 'triId' - Reserved for future use.
--
-- * 'triType' - The type of trigger that this is.
--
-- * 'triDescription' - A description of this trigger.
trigger ::
  Trigger
trigger =
  Trigger'
    { _triWorkflowName = Nothing,
      _triState = Nothing,
      _triActions = Nothing,
      _triSchedule = Nothing,
      _triPredicate = Nothing,
      _triName = Nothing,
      _triId = Nothing,
      _triType = Nothing,
      _triDescription = Nothing
    }

-- | The name of the workflow associated with the trigger.
triWorkflowName :: Lens' Trigger (Maybe Text)
triWorkflowName = lens _triWorkflowName (\s a -> s {_triWorkflowName = a})

-- | The current state of the trigger.
triState :: Lens' Trigger (Maybe TriggerState)
triState = lens _triState (\s a -> s {_triState = a})

-- | The actions initiated by this trigger.
triActions :: Lens' Trigger [Action]
triActions = lens _triActions (\s a -> s {_triActions = a}) . _Default . _Coerce

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
triSchedule :: Lens' Trigger (Maybe Text)
triSchedule = lens _triSchedule (\s a -> s {_triSchedule = a})

-- | The predicate of this trigger, which defines when it will fire.
triPredicate :: Lens' Trigger (Maybe Predicate)
triPredicate = lens _triPredicate (\s a -> s {_triPredicate = a})

-- | The name of the trigger.
triName :: Lens' Trigger (Maybe Text)
triName = lens _triName (\s a -> s {_triName = a})

-- | Reserved for future use.
triId :: Lens' Trigger (Maybe Text)
triId = lens _triId (\s a -> s {_triId = a})

-- | The type of trigger that this is.
triType :: Lens' Trigger (Maybe TriggerType)
triType = lens _triType (\s a -> s {_triType = a})

-- | A description of this trigger.
triDescription :: Lens' Trigger (Maybe Text)
triDescription = lens _triDescription (\s a -> s {_triDescription = a})

instance FromJSON Trigger where
  parseJSON =
    withObject
      "Trigger"
      ( \x ->
          Trigger'
            <$> (x .:? "WorkflowName")
            <*> (x .:? "State")
            <*> (x .:? "Actions" .!= mempty)
            <*> (x .:? "Schedule")
            <*> (x .:? "Predicate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
      )

instance Hashable Trigger

instance NFData Trigger
