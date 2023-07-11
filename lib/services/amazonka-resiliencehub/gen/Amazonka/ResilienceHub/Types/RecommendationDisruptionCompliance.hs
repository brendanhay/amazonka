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
-- Module      : Amazonka.ResilienceHub.Types.RecommendationDisruptionCompliance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.RecommendationDisruptionCompliance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.ComplianceStatus

-- | Defines a disruption compliance recommendation.
--
-- /See:/ 'newRecommendationDisruptionCompliance' smart constructor.
data RecommendationDisruptionCompliance = RecommendationDisruptionCompliance'
  { -- | The expected Recovery Point Objective (RPO) description after applying
    -- the recommended configuration change.
    expectedRpoDescription :: Prelude.Maybe Prelude.Text,
    -- | The expected RPO after applying the recommended configuration change.
    expectedRpoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The expected Recovery Time Objective (RTO) description after applying
    -- the recommended configuration change.
    expectedRtoDescription :: Prelude.Maybe Prelude.Text,
    -- | The expected RTO after applying the recommended configuration change.
    expectedRtoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The expected compliance status after applying the recommended
    -- configuration change.
    expectedComplianceStatus :: ComplianceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationDisruptionCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedRpoDescription', 'recommendationDisruptionCompliance_expectedRpoDescription' - The expected Recovery Point Objective (RPO) description after applying
-- the recommended configuration change.
--
-- 'expectedRpoInSecs', 'recommendationDisruptionCompliance_expectedRpoInSecs' - The expected RPO after applying the recommended configuration change.
--
-- 'expectedRtoDescription', 'recommendationDisruptionCompliance_expectedRtoDescription' - The expected Recovery Time Objective (RTO) description after applying
-- the recommended configuration change.
--
-- 'expectedRtoInSecs', 'recommendationDisruptionCompliance_expectedRtoInSecs' - The expected RTO after applying the recommended configuration change.
--
-- 'expectedComplianceStatus', 'recommendationDisruptionCompliance_expectedComplianceStatus' - The expected compliance status after applying the recommended
-- configuration change.
newRecommendationDisruptionCompliance ::
  -- | 'expectedComplianceStatus'
  ComplianceStatus ->
  RecommendationDisruptionCompliance
newRecommendationDisruptionCompliance
  pExpectedComplianceStatus_ =
    RecommendationDisruptionCompliance'
      { expectedRpoDescription =
          Prelude.Nothing,
        expectedRpoInSecs = Prelude.Nothing,
        expectedRtoDescription =
          Prelude.Nothing,
        expectedRtoInSecs = Prelude.Nothing,
        expectedComplianceStatus =
          pExpectedComplianceStatus_
      }

-- | The expected Recovery Point Objective (RPO) description after applying
-- the recommended configuration change.
recommendationDisruptionCompliance_expectedRpoDescription :: Lens.Lens' RecommendationDisruptionCompliance (Prelude.Maybe Prelude.Text)
recommendationDisruptionCompliance_expectedRpoDescription = Lens.lens (\RecommendationDisruptionCompliance' {expectedRpoDescription} -> expectedRpoDescription) (\s@RecommendationDisruptionCompliance' {} a -> s {expectedRpoDescription = a} :: RecommendationDisruptionCompliance)

-- | The expected RPO after applying the recommended configuration change.
recommendationDisruptionCompliance_expectedRpoInSecs :: Lens.Lens' RecommendationDisruptionCompliance (Prelude.Maybe Prelude.Natural)
recommendationDisruptionCompliance_expectedRpoInSecs = Lens.lens (\RecommendationDisruptionCompliance' {expectedRpoInSecs} -> expectedRpoInSecs) (\s@RecommendationDisruptionCompliance' {} a -> s {expectedRpoInSecs = a} :: RecommendationDisruptionCompliance)

-- | The expected Recovery Time Objective (RTO) description after applying
-- the recommended configuration change.
recommendationDisruptionCompliance_expectedRtoDescription :: Lens.Lens' RecommendationDisruptionCompliance (Prelude.Maybe Prelude.Text)
recommendationDisruptionCompliance_expectedRtoDescription = Lens.lens (\RecommendationDisruptionCompliance' {expectedRtoDescription} -> expectedRtoDescription) (\s@RecommendationDisruptionCompliance' {} a -> s {expectedRtoDescription = a} :: RecommendationDisruptionCompliance)

-- | The expected RTO after applying the recommended configuration change.
recommendationDisruptionCompliance_expectedRtoInSecs :: Lens.Lens' RecommendationDisruptionCompliance (Prelude.Maybe Prelude.Natural)
recommendationDisruptionCompliance_expectedRtoInSecs = Lens.lens (\RecommendationDisruptionCompliance' {expectedRtoInSecs} -> expectedRtoInSecs) (\s@RecommendationDisruptionCompliance' {} a -> s {expectedRtoInSecs = a} :: RecommendationDisruptionCompliance)

-- | The expected compliance status after applying the recommended
-- configuration change.
recommendationDisruptionCompliance_expectedComplianceStatus :: Lens.Lens' RecommendationDisruptionCompliance ComplianceStatus
recommendationDisruptionCompliance_expectedComplianceStatus = Lens.lens (\RecommendationDisruptionCompliance' {expectedComplianceStatus} -> expectedComplianceStatus) (\s@RecommendationDisruptionCompliance' {} a -> s {expectedComplianceStatus = a} :: RecommendationDisruptionCompliance)

instance
  Data.FromJSON
    RecommendationDisruptionCompliance
  where
  parseJSON =
    Data.withObject
      "RecommendationDisruptionCompliance"
      ( \x ->
          RecommendationDisruptionCompliance'
            Prelude.<$> (x Data..:? "expectedRpoDescription")
            Prelude.<*> (x Data..:? "expectedRpoInSecs")
            Prelude.<*> (x Data..:? "expectedRtoDescription")
            Prelude.<*> (x Data..:? "expectedRtoInSecs")
            Prelude.<*> (x Data..: "expectedComplianceStatus")
      )

instance
  Prelude.Hashable
    RecommendationDisruptionCompliance
  where
  hashWithSalt
    _salt
    RecommendationDisruptionCompliance' {..} =
      _salt
        `Prelude.hashWithSalt` expectedRpoDescription
        `Prelude.hashWithSalt` expectedRpoInSecs
        `Prelude.hashWithSalt` expectedRtoDescription
        `Prelude.hashWithSalt` expectedRtoInSecs
        `Prelude.hashWithSalt` expectedComplianceStatus

instance
  Prelude.NFData
    RecommendationDisruptionCompliance
  where
  rnf RecommendationDisruptionCompliance' {..} =
    Prelude.rnf expectedRpoDescription
      `Prelude.seq` Prelude.rnf expectedRpoInSecs
      `Prelude.seq` Prelude.rnf expectedRtoDescription
      `Prelude.seq` Prelude.rnf expectedRtoInSecs
      `Prelude.seq` Prelude.rnf expectedComplianceStatus
