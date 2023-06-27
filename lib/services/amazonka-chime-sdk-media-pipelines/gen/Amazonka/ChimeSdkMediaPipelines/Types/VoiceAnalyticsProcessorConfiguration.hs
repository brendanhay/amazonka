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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsProcessorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsProcessorConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsConfigurationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for a voice analytics processor.
--
-- /See:/ 'newVoiceAnalyticsProcessorConfiguration' smart constructor.
data VoiceAnalyticsProcessorConfiguration = VoiceAnalyticsProcessorConfiguration'
  { -- | The status of the speaker search task.
    speakerSearchStatus :: Prelude.Maybe VoiceAnalyticsConfigurationStatus,
    -- | The status of the voice tone analysis task.
    voiceToneAnalysisStatus :: Prelude.Maybe VoiceAnalyticsConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceAnalyticsProcessorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speakerSearchStatus', 'voiceAnalyticsProcessorConfiguration_speakerSearchStatus' - The status of the speaker search task.
--
-- 'voiceToneAnalysisStatus', 'voiceAnalyticsProcessorConfiguration_voiceToneAnalysisStatus' - The status of the voice tone analysis task.
newVoiceAnalyticsProcessorConfiguration ::
  VoiceAnalyticsProcessorConfiguration
newVoiceAnalyticsProcessorConfiguration =
  VoiceAnalyticsProcessorConfiguration'
    { speakerSearchStatus =
        Prelude.Nothing,
      voiceToneAnalysisStatus =
        Prelude.Nothing
    }

-- | The status of the speaker search task.
voiceAnalyticsProcessorConfiguration_speakerSearchStatus :: Lens.Lens' VoiceAnalyticsProcessorConfiguration (Prelude.Maybe VoiceAnalyticsConfigurationStatus)
voiceAnalyticsProcessorConfiguration_speakerSearchStatus = Lens.lens (\VoiceAnalyticsProcessorConfiguration' {speakerSearchStatus} -> speakerSearchStatus) (\s@VoiceAnalyticsProcessorConfiguration' {} a -> s {speakerSearchStatus = a} :: VoiceAnalyticsProcessorConfiguration)

-- | The status of the voice tone analysis task.
voiceAnalyticsProcessorConfiguration_voiceToneAnalysisStatus :: Lens.Lens' VoiceAnalyticsProcessorConfiguration (Prelude.Maybe VoiceAnalyticsConfigurationStatus)
voiceAnalyticsProcessorConfiguration_voiceToneAnalysisStatus = Lens.lens (\VoiceAnalyticsProcessorConfiguration' {voiceToneAnalysisStatus} -> voiceToneAnalysisStatus) (\s@VoiceAnalyticsProcessorConfiguration' {} a -> s {voiceToneAnalysisStatus = a} :: VoiceAnalyticsProcessorConfiguration)

instance
  Data.FromJSON
    VoiceAnalyticsProcessorConfiguration
  where
  parseJSON =
    Data.withObject
      "VoiceAnalyticsProcessorConfiguration"
      ( \x ->
          VoiceAnalyticsProcessorConfiguration'
            Prelude.<$> (x Data..:? "SpeakerSearchStatus")
            Prelude.<*> (x Data..:? "VoiceToneAnalysisStatus")
      )

instance
  Prelude.Hashable
    VoiceAnalyticsProcessorConfiguration
  where
  hashWithSalt
    _salt
    VoiceAnalyticsProcessorConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` speakerSearchStatus
        `Prelude.hashWithSalt` voiceToneAnalysisStatus

instance
  Prelude.NFData
    VoiceAnalyticsProcessorConfiguration
  where
  rnf VoiceAnalyticsProcessorConfiguration' {..} =
    Prelude.rnf speakerSearchStatus
      `Prelude.seq` Prelude.rnf voiceToneAnalysisStatus

instance
  Data.ToJSON
    VoiceAnalyticsProcessorConfiguration
  where
  toJSON VoiceAnalyticsProcessorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpeakerSearchStatus" Data..=)
              Prelude.<$> speakerSearchStatus,
            ("VoiceToneAnalysisStatus" Data..=)
              Prelude.<$> voiceToneAnalysisStatus
          ]
      )
