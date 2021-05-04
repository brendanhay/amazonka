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
-- Module      : Network.AWS.MediaLive.Types.FailoverConditionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverConditionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings
import Network.AWS.MediaLive.Types.InputLossFailoverSettings
import Network.AWS.MediaLive.Types.VideoBlackFailoverSettings
import qualified Network.AWS.Prelude as Prelude

-- | Settings for one failover condition.
--
-- /See:/ 'newFailoverConditionSettings' smart constructor.
data FailoverConditionSettings = FailoverConditionSettings'
  { -- | MediaLive will perform a failover if content is considered black for the
    -- specified period.
    videoBlackSettings :: Prelude.Maybe VideoBlackFailoverSettings,
    -- | MediaLive will perform a failover if content is not detected in this
    -- input for the specified period.
    inputLossSettings :: Prelude.Maybe InputLossFailoverSettings,
    -- | MediaLive will perform a failover if the specified audio selector is
    -- silent for the specified period.
    audioSilenceSettings :: Prelude.Maybe AudioSilenceFailoverSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailoverConditionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'videoBlackSettings', 'failoverConditionSettings_videoBlackSettings' - MediaLive will perform a failover if content is considered black for the
-- specified period.
--
-- 'inputLossSettings', 'failoverConditionSettings_inputLossSettings' - MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
--
-- 'audioSilenceSettings', 'failoverConditionSettings_audioSilenceSettings' - MediaLive will perform a failover if the specified audio selector is
-- silent for the specified period.
newFailoverConditionSettings ::
  FailoverConditionSettings
newFailoverConditionSettings =
  FailoverConditionSettings'
    { videoBlackSettings =
        Prelude.Nothing,
      inputLossSettings = Prelude.Nothing,
      audioSilenceSettings = Prelude.Nothing
    }

-- | MediaLive will perform a failover if content is considered black for the
-- specified period.
failoverConditionSettings_videoBlackSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe VideoBlackFailoverSettings)
failoverConditionSettings_videoBlackSettings = Lens.lens (\FailoverConditionSettings' {videoBlackSettings} -> videoBlackSettings) (\s@FailoverConditionSettings' {} a -> s {videoBlackSettings = a} :: FailoverConditionSettings)

-- | MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
failoverConditionSettings_inputLossSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe InputLossFailoverSettings)
failoverConditionSettings_inputLossSettings = Lens.lens (\FailoverConditionSettings' {inputLossSettings} -> inputLossSettings) (\s@FailoverConditionSettings' {} a -> s {inputLossSettings = a} :: FailoverConditionSettings)

-- | MediaLive will perform a failover if the specified audio selector is
-- silent for the specified period.
failoverConditionSettings_audioSilenceSettings :: Lens.Lens' FailoverConditionSettings (Prelude.Maybe AudioSilenceFailoverSettings)
failoverConditionSettings_audioSilenceSettings = Lens.lens (\FailoverConditionSettings' {audioSilenceSettings} -> audioSilenceSettings) (\s@FailoverConditionSettings' {} a -> s {audioSilenceSettings = a} :: FailoverConditionSettings)

instance Prelude.FromJSON FailoverConditionSettings where
  parseJSON =
    Prelude.withObject
      "FailoverConditionSettings"
      ( \x ->
          FailoverConditionSettings'
            Prelude.<$> (x Prelude..:? "videoBlackSettings")
            Prelude.<*> (x Prelude..:? "inputLossSettings")
            Prelude.<*> (x Prelude..:? "audioSilenceSettings")
      )

instance Prelude.Hashable FailoverConditionSettings

instance Prelude.NFData FailoverConditionSettings

instance Prelude.ToJSON FailoverConditionSettings where
  toJSON FailoverConditionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("videoBlackSettings" Prelude..=)
              Prelude.<$> videoBlackSettings,
            ("inputLossSettings" Prelude..=)
              Prelude.<$> inputLossSettings,
            ("audioSilenceSettings" Prelude..=)
              Prelude.<$> audioSilenceSettings
          ]
      )
