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
-- Module      : Amazonka.MediaLive.Types.FailoverConditionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FailoverConditionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioSilenceFailoverSettings
import Amazonka.MediaLive.Types.InputLossFailoverSettings
import Amazonka.MediaLive.Types.VideoBlackFailoverSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings for one failover condition.
--
-- /See:/ 'newFailoverConditionSettings' smart constructor.
data FailoverConditionSettings = FailoverConditionSettings'
  { -- | MediaLive will perform a failover if the specified audio selector is
    -- silent for the specified period.
    audioSilenceSettings :: Prelude.Maybe AudioSilenceFailoverSettings,
    -- | MediaLive will perform a failover if content is not detected in this
    -- input for the specified period.
    inputLossSettings :: Prelude.Maybe InputLossFailoverSettings,
    -- | MediaLive will perform a failover if content is considered black for the
    -- specified period.
    videoBlackSettings :: Prelude.Maybe VideoBlackFailoverSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverConditionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioSilenceSettings', 'failoverConditionSettings_audioSilenceSettings' - MediaLive will perform a failover if the specified audio selector is
-- silent for the specified period.
--
-- 'inputLossSettings', 'failoverConditionSettings_inputLossSettings' - MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
--
-- 'videoBlackSettings', 'failoverConditionSettings_videoBlackSettings' - MediaLive will perform a failover if content is considered black for the
-- specified period.
newFailoverConditionSettings ::
  FailoverConditionSettings
newFailoverConditionSettings =
  FailoverConditionSettings'
    { audioSilenceSettings =
        Prelude.Nothing,
      inputLossSettings = Prelude.Nothing,
      videoBlackSettings = Prelude.Nothing
    }

-- | MediaLive will perform a failover if the specified audio selector is
-- silent for the specified period.
failoverConditionSettings_audioSilenceSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe AudioSilenceFailoverSettings)
failoverConditionSettings_audioSilenceSettings = Lens.lens (\FailoverConditionSettings' {audioSilenceSettings} -> audioSilenceSettings) (\s@FailoverConditionSettings' {} a -> s {audioSilenceSettings = a} :: FailoverConditionSettings)

-- | MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
failoverConditionSettings_inputLossSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe InputLossFailoverSettings)
failoverConditionSettings_inputLossSettings = Lens.lens (\FailoverConditionSettings' {inputLossSettings} -> inputLossSettings) (\s@FailoverConditionSettings' {} a -> s {inputLossSettings = a} :: FailoverConditionSettings)

-- | MediaLive will perform a failover if content is considered black for the
-- specified period.
failoverConditionSettings_videoBlackSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe VideoBlackFailoverSettings)
failoverConditionSettings_videoBlackSettings = Lens.lens (\FailoverConditionSettings' {videoBlackSettings} -> videoBlackSettings) (\s@FailoverConditionSettings' {} a -> s {videoBlackSettings = a} :: FailoverConditionSettings)

instance Data.FromJSON FailoverConditionSettings where
  parseJSON =
    Data.withObject
      "FailoverConditionSettings"
      ( \x ->
          FailoverConditionSettings'
            Prelude.<$> (x Data..:? "audioSilenceSettings")
            Prelude.<*> (x Data..:? "inputLossSettings")
            Prelude.<*> (x Data..:? "videoBlackSettings")
      )

instance Prelude.Hashable FailoverConditionSettings where
  hashWithSalt _salt FailoverConditionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` audioSilenceSettings
      `Prelude.hashWithSalt` inputLossSettings
      `Prelude.hashWithSalt` videoBlackSettings

instance Prelude.NFData FailoverConditionSettings where
  rnf FailoverConditionSettings' {..} =
    Prelude.rnf audioSilenceSettings
      `Prelude.seq` Prelude.rnf inputLossSettings
      `Prelude.seq` Prelude.rnf videoBlackSettings

instance Data.ToJSON FailoverConditionSettings where
  toJSON FailoverConditionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioSilenceSettings" Data..=)
              Prelude.<$> audioSilenceSettings,
            ("inputLossSettings" Data..=)
              Prelude.<$> inputLossSettings,
            ("videoBlackSettings" Data..=)
              Prelude.<$> videoBlackSettings
          ]
      )
