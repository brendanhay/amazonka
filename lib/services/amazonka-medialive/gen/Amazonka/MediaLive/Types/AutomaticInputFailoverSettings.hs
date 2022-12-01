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
-- Module      : Amazonka.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AutomaticInputFailoverSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.FailoverCondition
import Amazonka.MediaLive.Types.InputPreference
import qualified Amazonka.Prelude as Prelude

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'newAutomaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'
  { -- | This clear time defines the requirement a recovered input must meet to
    -- be considered healthy. The input must have no failover conditions for
    -- this length of time. Enter a time in milliseconds. This value is
    -- particularly important if the input_preference for the failover pair is
    -- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
    -- switch back to the primary input.
    errorClearTimeMsec :: Prelude.Maybe Prelude.Natural,
    -- | A list of failover conditions. If any of these conditions occur,
    -- MediaLive will perform a failover to the other input.
    failoverConditions :: Prelude.Maybe [FailoverCondition],
    -- | Input preference when deciding which input to make active when a
    -- previously failed input has recovered.
    inputPreference :: Prelude.Maybe InputPreference,
    -- | The input ID of the secondary input in the automatic input failover
    -- pair.
    secondaryInputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomaticInputFailoverSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorClearTimeMsec', 'automaticInputFailoverSettings_errorClearTimeMsec' - This clear time defines the requirement a recovered input must meet to
-- be considered healthy. The input must have no failover conditions for
-- this length of time. Enter a time in milliseconds. This value is
-- particularly important if the input_preference for the failover pair is
-- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
-- switch back to the primary input.
--
-- 'failoverConditions', 'automaticInputFailoverSettings_failoverConditions' - A list of failover conditions. If any of these conditions occur,
-- MediaLive will perform a failover to the other input.
--
-- 'inputPreference', 'automaticInputFailoverSettings_inputPreference' - Input preference when deciding which input to make active when a
-- previously failed input has recovered.
--
-- 'secondaryInputId', 'automaticInputFailoverSettings_secondaryInputId' - The input ID of the secondary input in the automatic input failover
-- pair.
newAutomaticInputFailoverSettings ::
  -- | 'secondaryInputId'
  Prelude.Text ->
  AutomaticInputFailoverSettings
newAutomaticInputFailoverSettings pSecondaryInputId_ =
  AutomaticInputFailoverSettings'
    { errorClearTimeMsec =
        Prelude.Nothing,
      failoverConditions = Prelude.Nothing,
      inputPreference = Prelude.Nothing,
      secondaryInputId = pSecondaryInputId_
    }

-- | This clear time defines the requirement a recovered input must meet to
-- be considered healthy. The input must have no failover conditions for
-- this length of time. Enter a time in milliseconds. This value is
-- particularly important if the input_preference for the failover pair is
-- set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will
-- switch back to the primary input.
automaticInputFailoverSettings_errorClearTimeMsec :: Lens.Lens' AutomaticInputFailoverSettings (Prelude.Maybe Prelude.Natural)
automaticInputFailoverSettings_errorClearTimeMsec = Lens.lens (\AutomaticInputFailoverSettings' {errorClearTimeMsec} -> errorClearTimeMsec) (\s@AutomaticInputFailoverSettings' {} a -> s {errorClearTimeMsec = a} :: AutomaticInputFailoverSettings)

-- | A list of failover conditions. If any of these conditions occur,
-- MediaLive will perform a failover to the other input.
automaticInputFailoverSettings_failoverConditions :: Lens.Lens' AutomaticInputFailoverSettings (Prelude.Maybe [FailoverCondition])
automaticInputFailoverSettings_failoverConditions = Lens.lens (\AutomaticInputFailoverSettings' {failoverConditions} -> failoverConditions) (\s@AutomaticInputFailoverSettings' {} a -> s {failoverConditions = a} :: AutomaticInputFailoverSettings) Prelude.. Lens.mapping Lens.coerced

-- | Input preference when deciding which input to make active when a
-- previously failed input has recovered.
automaticInputFailoverSettings_inputPreference :: Lens.Lens' AutomaticInputFailoverSettings (Prelude.Maybe InputPreference)
automaticInputFailoverSettings_inputPreference = Lens.lens (\AutomaticInputFailoverSettings' {inputPreference} -> inputPreference) (\s@AutomaticInputFailoverSettings' {} a -> s {inputPreference = a} :: AutomaticInputFailoverSettings)

-- | The input ID of the secondary input in the automatic input failover
-- pair.
automaticInputFailoverSettings_secondaryInputId :: Lens.Lens' AutomaticInputFailoverSettings Prelude.Text
automaticInputFailoverSettings_secondaryInputId = Lens.lens (\AutomaticInputFailoverSettings' {secondaryInputId} -> secondaryInputId) (\s@AutomaticInputFailoverSettings' {} a -> s {secondaryInputId = a} :: AutomaticInputFailoverSettings)

instance Core.FromJSON AutomaticInputFailoverSettings where
  parseJSON =
    Core.withObject
      "AutomaticInputFailoverSettings"
      ( \x ->
          AutomaticInputFailoverSettings'
            Prelude.<$> (x Core..:? "errorClearTimeMsec")
            Prelude.<*> ( x Core..:? "failoverConditions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "inputPreference")
            Prelude.<*> (x Core..: "secondaryInputId")
      )

instance
  Prelude.Hashable
    AutomaticInputFailoverSettings
  where
  hashWithSalt
    _salt
    AutomaticInputFailoverSettings' {..} =
      _salt `Prelude.hashWithSalt` errorClearTimeMsec
        `Prelude.hashWithSalt` failoverConditions
        `Prelude.hashWithSalt` inputPreference
        `Prelude.hashWithSalt` secondaryInputId

instance
  Prelude.NFData
    AutomaticInputFailoverSettings
  where
  rnf AutomaticInputFailoverSettings' {..} =
    Prelude.rnf errorClearTimeMsec
      `Prelude.seq` Prelude.rnf failoverConditions
      `Prelude.seq` Prelude.rnf inputPreference
      `Prelude.seq` Prelude.rnf secondaryInputId

instance Core.ToJSON AutomaticInputFailoverSettings where
  toJSON AutomaticInputFailoverSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("errorClearTimeMsec" Core..=)
              Prelude.<$> errorClearTimeMsec,
            ("failoverConditions" Core..=)
              Prelude.<$> failoverConditions,
            ("inputPreference" Core..=)
              Prelude.<$> inputPreference,
            Prelude.Just
              ("secondaryInputId" Core..= secondaryInputId)
          ]
      )
