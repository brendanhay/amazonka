{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerUpdate where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure used to provide information used to update a trigger. This object updates the previous trigger definition by overwriting it completely.
--
--
--
-- /See:/ 'triggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { _tuActions ::
      !(Maybe [Action]),
    _tuSchedule :: !(Maybe Text),
    _tuPredicate :: !(Maybe Predicate),
    _tuName :: !(Maybe Text),
    _tuDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TriggerUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuActions' - The actions initiated by this trigger.
--
-- * 'tuSchedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'tuPredicate' - The predicate of this trigger, which defines when it will fire.
--
-- * 'tuName' - Reserved for future use.
--
-- * 'tuDescription' - A description of this trigger.
triggerUpdate ::
  TriggerUpdate
triggerUpdate =
  TriggerUpdate'
    { _tuActions = Nothing,
      _tuSchedule = Nothing,
      _tuPredicate = Nothing,
      _tuName = Nothing,
      _tuDescription = Nothing
    }

-- | The actions initiated by this trigger.
tuActions :: Lens' TriggerUpdate [Action]
tuActions = lens _tuActions (\s a -> s {_tuActions = a}) . _Default . _Coerce

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
tuSchedule :: Lens' TriggerUpdate (Maybe Text)
tuSchedule = lens _tuSchedule (\s a -> s {_tuSchedule = a})

-- | The predicate of this trigger, which defines when it will fire.
tuPredicate :: Lens' TriggerUpdate (Maybe Predicate)
tuPredicate = lens _tuPredicate (\s a -> s {_tuPredicate = a})

-- | Reserved for future use.
tuName :: Lens' TriggerUpdate (Maybe Text)
tuName = lens _tuName (\s a -> s {_tuName = a})

-- | A description of this trigger.
tuDescription :: Lens' TriggerUpdate (Maybe Text)
tuDescription = lens _tuDescription (\s a -> s {_tuDescription = a})

instance Hashable TriggerUpdate

instance NFData TriggerUpdate

instance ToJSON TriggerUpdate where
  toJSON TriggerUpdate' {..} =
    object
      ( catMaybes
          [ ("Actions" .=) <$> _tuActions,
            ("Schedule" .=) <$> _tuSchedule,
            ("Predicate" .=) <$> _tuPredicate,
            ("Name" .=) <$> _tuName,
            ("Description" .=) <$> _tuDescription
          ]
      )
