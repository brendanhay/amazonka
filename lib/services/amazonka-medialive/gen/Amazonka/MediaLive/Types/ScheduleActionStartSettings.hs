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
-- Module      : Amazonka.MediaLive.Types.ScheduleActionStartSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ScheduleActionStartSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FixedModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.FollowModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings to specify when an action should occur. Only one of the options
-- must be selected.
--
-- /See:/ 'newScheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'
  { -- | Option for specifying the start time for an action.
    fixedModeScheduleActionStartSettings :: Prelude.Maybe FixedModeScheduleActionStartSettings,
    -- | Option for specifying an action as relative to another action.
    followModeScheduleActionStartSettings :: Prelude.Maybe FollowModeScheduleActionStartSettings,
    -- | Option for specifying an action that should be applied immediately.
    immediateModeScheduleActionStartSettings :: Prelude.Maybe ImmediateModeScheduleActionStartSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fixedModeScheduleActionStartSettings', 'scheduleActionStartSettings_fixedModeScheduleActionStartSettings' - Option for specifying the start time for an action.
--
-- 'followModeScheduleActionStartSettings', 'scheduleActionStartSettings_followModeScheduleActionStartSettings' - Option for specifying an action as relative to another action.
--
-- 'immediateModeScheduleActionStartSettings', 'scheduleActionStartSettings_immediateModeScheduleActionStartSettings' - Option for specifying an action that should be applied immediately.
newScheduleActionStartSettings ::
  ScheduleActionStartSettings
newScheduleActionStartSettings =
  ScheduleActionStartSettings'
    { fixedModeScheduleActionStartSettings =
        Prelude.Nothing,
      followModeScheduleActionStartSettings =
        Prelude.Nothing,
      immediateModeScheduleActionStartSettings =
        Prelude.Nothing
    }

-- | Option for specifying the start time for an action.
scheduleActionStartSettings_fixedModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Prelude.Maybe FixedModeScheduleActionStartSettings)
scheduleActionStartSettings_fixedModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {fixedModeScheduleActionStartSettings} -> fixedModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {fixedModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

-- | Option for specifying an action as relative to another action.
scheduleActionStartSettings_followModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Prelude.Maybe FollowModeScheduleActionStartSettings)
scheduleActionStartSettings_followModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {followModeScheduleActionStartSettings} -> followModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {followModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

-- | Option for specifying an action that should be applied immediately.
scheduleActionStartSettings_immediateModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Prelude.Maybe ImmediateModeScheduleActionStartSettings)
scheduleActionStartSettings_immediateModeScheduleActionStartSettings = Lens.lens (\ScheduleActionStartSettings' {immediateModeScheduleActionStartSettings} -> immediateModeScheduleActionStartSettings) (\s@ScheduleActionStartSettings' {} a -> s {immediateModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)

instance Data.FromJSON ScheduleActionStartSettings where
  parseJSON =
    Data.withObject
      "ScheduleActionStartSettings"
      ( \x ->
          ScheduleActionStartSettings'
            Prelude.<$> (x Data..:? "fixedModeScheduleActionStartSettings")
            Prelude.<*> (x Data..:? "followModeScheduleActionStartSettings")
            Prelude.<*> ( x
                            Data..:? "immediateModeScheduleActionStartSettings"
                        )
      )

instance Prelude.Hashable ScheduleActionStartSettings where
  hashWithSalt _salt ScheduleActionStartSettings' {..} =
    _salt
      `Prelude.hashWithSalt` fixedModeScheduleActionStartSettings
      `Prelude.hashWithSalt` followModeScheduleActionStartSettings
      `Prelude.hashWithSalt` immediateModeScheduleActionStartSettings

instance Prelude.NFData ScheduleActionStartSettings where
  rnf ScheduleActionStartSettings' {..} =
    Prelude.rnf fixedModeScheduleActionStartSettings
      `Prelude.seq` Prelude.rnf followModeScheduleActionStartSettings
      `Prelude.seq` Prelude.rnf immediateModeScheduleActionStartSettings

instance Data.ToJSON ScheduleActionStartSettings where
  toJSON ScheduleActionStartSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fixedModeScheduleActionStartSettings" Data..=)
              Prelude.<$> fixedModeScheduleActionStartSettings,
            ("followModeScheduleActionStartSettings" Data..=)
              Prelude.<$> followModeScheduleActionStartSettings,
            ("immediateModeScheduleActionStartSettings" Data..=)
              Prelude.<$> immediateModeScheduleActionStartSettings
          ]
      )
