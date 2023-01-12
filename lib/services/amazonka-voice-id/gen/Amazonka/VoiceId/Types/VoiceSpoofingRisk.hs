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
-- Module      : Amazonka.VoiceId.Types.VoiceSpoofingRisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.VoiceSpoofingRisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details resulting from \'Voice Spoofing Risk\' analysis of the
-- speaker.
--
-- /See:/ 'newVoiceSpoofingRisk' smart constructor.
data VoiceSpoofingRisk = VoiceSpoofingRisk'
  { -- | The score indicating the likelihood of speaker’s voice being spoofed.
    riskScore :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceSpoofingRisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'riskScore', 'voiceSpoofingRisk_riskScore' - The score indicating the likelihood of speaker’s voice being spoofed.
newVoiceSpoofingRisk ::
  -- | 'riskScore'
  Prelude.Natural ->
  VoiceSpoofingRisk
newVoiceSpoofingRisk pRiskScore_ =
  VoiceSpoofingRisk' {riskScore = pRiskScore_}

-- | The score indicating the likelihood of speaker’s voice being spoofed.
voiceSpoofingRisk_riskScore :: Lens.Lens' VoiceSpoofingRisk Prelude.Natural
voiceSpoofingRisk_riskScore = Lens.lens (\VoiceSpoofingRisk' {riskScore} -> riskScore) (\s@VoiceSpoofingRisk' {} a -> s {riskScore = a} :: VoiceSpoofingRisk)

instance Data.FromJSON VoiceSpoofingRisk where
  parseJSON =
    Data.withObject
      "VoiceSpoofingRisk"
      ( \x ->
          VoiceSpoofingRisk'
            Prelude.<$> (x Data..: "RiskScore")
      )

instance Prelude.Hashable VoiceSpoofingRisk where
  hashWithSalt _salt VoiceSpoofingRisk' {..} =
    _salt `Prelude.hashWithSalt` riskScore

instance Prelude.NFData VoiceSpoofingRisk where
  rnf VoiceSpoofingRisk' {..} = Prelude.rnf riskScore
