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
-- Module      : Amazonka.VoiceId.Types.KnownFraudsterRisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.KnownFraudsterRisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details produced as a result of performing known fraudster risk
-- analysis on a speaker.
--
-- /See:/ 'newKnownFraudsterRisk' smart constructor.
data KnownFraudsterRisk = KnownFraudsterRisk'
  { -- | The identifier of the fraudster that is the closest match to the
    -- speaker. If there are no fraudsters registered in a given domain, or if
    -- there are no fraudsters with a non-zero RiskScore, this value is @null@.
    generatedFraudsterId :: Prelude.Maybe Prelude.Text,
    -- | The score indicating the likelihood the speaker is a known fraudster.
    riskScore :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KnownFraudsterRisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedFraudsterId', 'knownFraudsterRisk_generatedFraudsterId' - The identifier of the fraudster that is the closest match to the
-- speaker. If there are no fraudsters registered in a given domain, or if
-- there are no fraudsters with a non-zero RiskScore, this value is @null@.
--
-- 'riskScore', 'knownFraudsterRisk_riskScore' - The score indicating the likelihood the speaker is a known fraudster.
newKnownFraudsterRisk ::
  -- | 'riskScore'
  Prelude.Natural ->
  KnownFraudsterRisk
newKnownFraudsterRisk pRiskScore_ =
  KnownFraudsterRisk'
    { generatedFraudsterId =
        Prelude.Nothing,
      riskScore = pRiskScore_
    }

-- | The identifier of the fraudster that is the closest match to the
-- speaker. If there are no fraudsters registered in a given domain, or if
-- there are no fraudsters with a non-zero RiskScore, this value is @null@.
knownFraudsterRisk_generatedFraudsterId :: Lens.Lens' KnownFraudsterRisk (Prelude.Maybe Prelude.Text)
knownFraudsterRisk_generatedFraudsterId = Lens.lens (\KnownFraudsterRisk' {generatedFraudsterId} -> generatedFraudsterId) (\s@KnownFraudsterRisk' {} a -> s {generatedFraudsterId = a} :: KnownFraudsterRisk)

-- | The score indicating the likelihood the speaker is a known fraudster.
knownFraudsterRisk_riskScore :: Lens.Lens' KnownFraudsterRisk Prelude.Natural
knownFraudsterRisk_riskScore = Lens.lens (\KnownFraudsterRisk' {riskScore} -> riskScore) (\s@KnownFraudsterRisk' {} a -> s {riskScore = a} :: KnownFraudsterRisk)

instance Data.FromJSON KnownFraudsterRisk where
  parseJSON =
    Data.withObject
      "KnownFraudsterRisk"
      ( \x ->
          KnownFraudsterRisk'
            Prelude.<$> (x Data..:? "GeneratedFraudsterId")
            Prelude.<*> (x Data..: "RiskScore")
      )

instance Prelude.Hashable KnownFraudsterRisk where
  hashWithSalt _salt KnownFraudsterRisk' {..} =
    _salt `Prelude.hashWithSalt` generatedFraudsterId
      `Prelude.hashWithSalt` riskScore

instance Prelude.NFData KnownFraudsterRisk where
  rnf KnownFraudsterRisk' {..} =
    Prelude.rnf generatedFraudsterId
      `Prelude.seq` Prelude.rnf riskScore
