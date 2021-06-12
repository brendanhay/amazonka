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
-- Module      : Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FailoverCondition
import Network.AWS.MediaLive.Types.InputPreference

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'newAutomaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'
  { -- | A list of failover conditions. If any of these conditions occur,
    -- MediaLive will perform a failover to the other input.
    failoverConditions :: Core.Maybe [FailoverCondition],
    -- | This clear time defines the requirement a recovered input must meet to
    -- be considered healthy. The input must have no failover conditions for
    -- this length of time. Enter a time in milliseconds. This value is
    -- particularly important if the input_preference for the failover pair is
    -- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
    -- switch back to the primary input.
    errorClearTimeMsec :: Core.Maybe Core.Natural,
    -- | Input preference when deciding which input to make active when a
    -- previously failed input has recovered.
    inputPreference :: Core.Maybe InputPreference,
    -- | The input ID of the secondary input in the automatic input failover
    -- pair.
    secondaryInputId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutomaticInputFailoverSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failoverConditions', 'automaticInputFailoverSettings_failoverConditions' - A list of failover conditions. If any of these conditions occur,
-- MediaLive will perform a failover to the other input.
--
-- 'errorClearTimeMsec', 'automaticInputFailoverSettings_errorClearTimeMsec' - This clear time defines the requirement a recovered input must meet to
-- be considered healthy. The input must have no failover conditions for
-- this length of time. Enter a time in milliseconds. This value is
-- particularly important if the input_preference for the failover pair is
-- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
-- switch back to the primary input.
--
-- 'inputPreference', 'automaticInputFailoverSettings_inputPreference' - Input preference when deciding which input to make active when a
-- previously failed input has recovered.
--
-- 'secondaryInputId', 'automaticInputFailoverSettings_secondaryInputId' - The input ID of the secondary input in the automatic input failover
-- pair.
newAutomaticInputFailoverSettings ::
  -- | 'secondaryInputId'
  Core.Text ->
  AutomaticInputFailoverSettings
newAutomaticInputFailoverSettings pSecondaryInputId_ =
  AutomaticInputFailoverSettings'
    { failoverConditions =
        Core.Nothing,
      errorClearTimeMsec = Core.Nothing,
      inputPreference = Core.Nothing,
      secondaryInputId = pSecondaryInputId_
    }

-- | A list of failover conditions. If any of these conditions occur,
-- MediaLive will perform a failover to the other input.
automaticInputFailoverSettings_failoverConditions :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe [FailoverCondition])
automaticInputFailoverSettings_failoverConditions = Lens.lens (\AutomaticInputFailoverSettings' {failoverConditions} -> failoverConditions) (\s@AutomaticInputFailoverSettings' {} a -> s {failoverConditions = a} :: AutomaticInputFailoverSettings) Core.. Lens.mapping Lens._Coerce

-- | This clear time defines the requirement a recovered input must meet to
-- be considered healthy. The input must have no failover conditions for
-- this length of time. Enter a time in milliseconds. This value is
-- particularly important if the input_preference for the failover pair is
-- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
-- switch back to the primary input.
automaticInputFailoverSettings_errorClearTimeMsec :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe Core.Natural)
automaticInputFailoverSettings_errorClearTimeMsec = Lens.lens (\AutomaticInputFailoverSettings' {errorClearTimeMsec} -> errorClearTimeMsec) (\s@AutomaticInputFailoverSettings' {} a -> s {errorClearTimeMsec = a} :: AutomaticInputFailoverSettings)

-- | Input preference when deciding which input to make active when a
-- previously failed input has recovered.
automaticInputFailoverSettings_inputPreference :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe InputPreference)
automaticInputFailoverSettings_inputPreference = Lens.lens (\AutomaticInputFailoverSettings' {inputPreference} -> inputPreference) (\s@AutomaticInputFailoverSettings' {} a -> s {inputPreference = a} :: AutomaticInputFailoverSettings)

-- | The input ID of the secondary input in the automatic input failover
-- pair.
automaticInputFailoverSettings_secondaryInputId :: Lens.Lens' AutomaticInputFailoverSettings Core.Text
automaticInputFailoverSettings_secondaryInputId = Lens.lens (\AutomaticInputFailoverSettings' {secondaryInputId} -> secondaryInputId) (\s@AutomaticInputFailoverSettings' {} a -> s {secondaryInputId = a} :: AutomaticInputFailoverSettings)

instance Core.FromJSON AutomaticInputFailoverSettings where
  parseJSON =
    Core.withObject
      "AutomaticInputFailoverSettings"
      ( \x ->
          AutomaticInputFailoverSettings'
            Core.<$> ( x Core..:? "failoverConditions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "errorClearTimeMsec")
            Core.<*> (x Core..:? "inputPreference")
            Core.<*> (x Core..: "secondaryInputId")
      )

instance Core.Hashable AutomaticInputFailoverSettings

instance Core.NFData AutomaticInputFailoverSettings

instance Core.ToJSON AutomaticInputFailoverSettings where
  toJSON AutomaticInputFailoverSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("failoverConditions" Core..=)
              Core.<$> failoverConditions,
            ("errorClearTimeMsec" Core..=)
              Core.<$> errorClearTimeMsec,
            ("inputPreference" Core..=) Core.<$> inputPreference,
            Core.Just
              ("secondaryInputId" Core..= secondaryInputId)
          ]
      )
