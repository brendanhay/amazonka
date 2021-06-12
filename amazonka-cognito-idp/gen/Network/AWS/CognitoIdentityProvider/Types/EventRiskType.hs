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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventRiskType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventRiskType where

import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The event risk type.
--
-- /See:/ 'newEventRiskType' smart constructor.
data EventRiskType = EventRiskType'
  { -- | Indicates whether compromised credentials were detected during an
    -- authentication event.
    compromisedCredentialsDetected :: Core.Maybe Core.Bool,
    -- | The risk decision.
    riskDecision :: Core.Maybe RiskDecisionType,
    -- | The risk level.
    riskLevel :: Core.Maybe RiskLevelType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventRiskType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compromisedCredentialsDetected', 'eventRiskType_compromisedCredentialsDetected' - Indicates whether compromised credentials were detected during an
-- authentication event.
--
-- 'riskDecision', 'eventRiskType_riskDecision' - The risk decision.
--
-- 'riskLevel', 'eventRiskType_riskLevel' - The risk level.
newEventRiskType ::
  EventRiskType
newEventRiskType =
  EventRiskType'
    { compromisedCredentialsDetected =
        Core.Nothing,
      riskDecision = Core.Nothing,
      riskLevel = Core.Nothing
    }

-- | Indicates whether compromised credentials were detected during an
-- authentication event.
eventRiskType_compromisedCredentialsDetected :: Lens.Lens' EventRiskType (Core.Maybe Core.Bool)
eventRiskType_compromisedCredentialsDetected = Lens.lens (\EventRiskType' {compromisedCredentialsDetected} -> compromisedCredentialsDetected) (\s@EventRiskType' {} a -> s {compromisedCredentialsDetected = a} :: EventRiskType)

-- | The risk decision.
eventRiskType_riskDecision :: Lens.Lens' EventRiskType (Core.Maybe RiskDecisionType)
eventRiskType_riskDecision = Lens.lens (\EventRiskType' {riskDecision} -> riskDecision) (\s@EventRiskType' {} a -> s {riskDecision = a} :: EventRiskType)

-- | The risk level.
eventRiskType_riskLevel :: Lens.Lens' EventRiskType (Core.Maybe RiskLevelType)
eventRiskType_riskLevel = Lens.lens (\EventRiskType' {riskLevel} -> riskLevel) (\s@EventRiskType' {} a -> s {riskLevel = a} :: EventRiskType)

instance Core.FromJSON EventRiskType where
  parseJSON =
    Core.withObject
      "EventRiskType"
      ( \x ->
          EventRiskType'
            Core.<$> (x Core..:? "CompromisedCredentialsDetected")
            Core.<*> (x Core..:? "RiskDecision")
            Core.<*> (x Core..:? "RiskLevel")
      )

instance Core.Hashable EventRiskType

instance Core.NFData EventRiskType
