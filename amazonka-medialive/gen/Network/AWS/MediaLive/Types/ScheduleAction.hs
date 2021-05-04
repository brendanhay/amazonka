{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleAction where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleActionSettings
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on a single schedule action.
--
-- /See:/ 'newScheduleAction' smart constructor.
data ScheduleAction = ScheduleAction'
  { -- | The name of the action, must be unique within the schedule. This name
    -- provides the main reference to an action once it is added to the
    -- schedule. A name is unique if it is no longer in the schedule. The
    -- schedule is automatically cleaned up to remove actions with a start time
    -- of more than 1 hour ago (approximately) so at that point a name can be
    -- reused.
    actionName :: Prelude.Text,
    -- | The time for the action to start in the channel.
    scheduleActionStartSettings :: ScheduleActionStartSettings,
    -- | Settings for this schedule action.
    scheduleActionSettings :: ScheduleActionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduleAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'scheduleAction_actionName' - The name of the action, must be unique within the schedule. This name
-- provides the main reference to an action once it is added to the
-- schedule. A name is unique if it is no longer in the schedule. The
-- schedule is automatically cleaned up to remove actions with a start time
-- of more than 1 hour ago (approximately) so at that point a name can be
-- reused.
--
-- 'scheduleActionStartSettings', 'scheduleAction_scheduleActionStartSettings' - The time for the action to start in the channel.
--
-- 'scheduleActionSettings', 'scheduleAction_scheduleActionSettings' - Settings for this schedule action.
newScheduleAction ::
  -- | 'actionName'
  Prelude.Text ->
  -- | 'scheduleActionStartSettings'
  ScheduleActionStartSettings ->
  -- | 'scheduleActionSettings'
  ScheduleActionSettings ->
  ScheduleAction
newScheduleAction
  pActionName_
  pScheduleActionStartSettings_
  pScheduleActionSettings_ =
    ScheduleAction'
      { actionName = pActionName_,
        scheduleActionStartSettings =
          pScheduleActionStartSettings_,
        scheduleActionSettings = pScheduleActionSettings_
      }

-- | The name of the action, must be unique within the schedule. This name
-- provides the main reference to an action once it is added to the
-- schedule. A name is unique if it is no longer in the schedule. The
-- schedule is automatically cleaned up to remove actions with a start time
-- of more than 1 hour ago (approximately) so at that point a name can be
-- reused.
scheduleAction_actionName :: Lens.Lens' ScheduleAction Prelude.Text
scheduleAction_actionName = Lens.lens (\ScheduleAction' {actionName} -> actionName) (\s@ScheduleAction' {} a -> s {actionName = a} :: ScheduleAction)

-- | The time for the action to start in the channel.
scheduleAction_scheduleActionStartSettings :: Lens.Lens' ScheduleAction ScheduleActionStartSettings
scheduleAction_scheduleActionStartSettings = Lens.lens (\ScheduleAction' {scheduleActionStartSettings} -> scheduleActionStartSettings) (\s@ScheduleAction' {} a -> s {scheduleActionStartSettings = a} :: ScheduleAction)

-- | Settings for this schedule action.
scheduleAction_scheduleActionSettings :: Lens.Lens' ScheduleAction ScheduleActionSettings
scheduleAction_scheduleActionSettings = Lens.lens (\ScheduleAction' {scheduleActionSettings} -> scheduleActionSettings) (\s@ScheduleAction' {} a -> s {scheduleActionSettings = a} :: ScheduleAction)

instance Prelude.FromJSON ScheduleAction where
  parseJSON =
    Prelude.withObject
      "ScheduleAction"
      ( \x ->
          ScheduleAction'
            Prelude.<$> (x Prelude..: "actionName")
            Prelude.<*> (x Prelude..: "scheduleActionStartSettings")
            Prelude.<*> (x Prelude..: "scheduleActionSettings")
      )

instance Prelude.Hashable ScheduleAction

instance Prelude.NFData ScheduleAction

instance Prelude.ToJSON ScheduleAction where
  toJSON ScheduleAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("actionName" Prelude..= actionName),
            Prelude.Just
              ( "scheduleActionStartSettings"
                  Prelude..= scheduleActionStartSettings
              ),
            Prelude.Just
              ( "scheduleActionSettings"
                  Prelude..= scheduleActionSettings
              )
          ]
      )
