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
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionStartSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionStartSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings

-- | Settings to specify when an action should occur. Only one of the options
-- must be selected.
--
-- /See:/ 'newScheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'
  { -- | Option for specifying an action as relative to another action.
    followModeScheduleActionStartSettings :: Core.Maybe FollowModeScheduleActionStartSettings,
    -- | Option for specifying an action that should be applied immediately.
    immediateModeScheduleActionStartSettings :: Core.Maybe ImmediateModeScheduleActionStartSettings,
    -- | Option for specifying the start time for an action.
    fixedModeScheduleActionStartSettings :: Core.Maybe FixedModeScheduleActionStartSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'followModeScheduleActionStartSettings', 'scheduleActionStartSettings_followModeScheduleActionStartSettings' - Option for specifying an action as relative to another action.
--
-- 'immediateModeScheduleActionStartSettings', 'scheduleActionStartSettings_immediateModeScheduleActionStartSettings' - Option for specifying an action that should be applied immediately.
--
-- 'fixedModeScheduleActionStartSettings', 'scheduleActionStartSettings_fixedModeScheduleActionStartSettings' - Option for specifying the start time for an action.
newScheduleActionStartSettings ::
  ScheduleActionStartSettings
newScheduleActionStartSettings =
  ScheduleActionStartSettings'
    { followModeScheduleActionStartSettings =
        Core.Nothing,
      immediateModeScheduleActionStartSettings =
        Core.Nothing,
      fixedModeScheduleActionStartSettings =
        Core.Nothing
    }

-- | Option for specifying an action as relative to another action.
scheduleActionStartSettings_followModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe FollowModeScheduleActionStartSettings)
scheduleActionStartSettings_followModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {followModeScheduleActionStartSettings} -> followModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {followModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

-- | Option for specifying an action that should be applied immediately.
scheduleActionStartSettings_immediateModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe ImmediateModeScheduleActionStartSettings)
scheduleActionStartSettings_immediateModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {immediateModeScheduleActionStartSettings} -> immediateModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {immediateModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

-- | Option for specifying the start time for an action.
scheduleActionStartSettings_fixedModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe FixedModeScheduleActionStartSettings)
scheduleActionStartSettings_fixedModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {fixedModeScheduleActionStartSettings} -> fixedModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {fixedModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

instance Core.FromJSON ScheduleActionStartSettings where
  parseJSON =
    Core.withObject
      "ScheduleActionStartSettings"
      ( \x ->
          ScheduleActionStartSettings'
            Core.<$> (x Core..:? "followModeScheduleActionStartSettings")
            Core.<*> ( x
                         Core..:? "immediateModeScheduleActionStartSettings"
                     )
            Core.<*> (x Core..:? "fixedModeScheduleActionStartSettings")
      )

instance Core.Hashable ScheduleActionStartSettings

instance Core.NFData ScheduleActionStartSettings

instance Core.ToJSON ScheduleActionStartSettings where
  toJSON ScheduleActionStartSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("followModeScheduleActionStartSettings" Core..=)
              Core.<$> followModeScheduleActionStartSettings,
            ("immediateModeScheduleActionStartSettings" Core..=)
              Core.<$> immediateModeScheduleActionStartSettings,
            ("fixedModeScheduleActionStartSettings" Core..=)
              Core.<$> fixedModeScheduleActionStartSettings
          ]
      )
