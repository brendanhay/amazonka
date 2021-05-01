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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventRiskType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventRiskType where

import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The event risk type.
--
-- /See:/ 'newEventRiskType' smart constructor.
data EventRiskType = EventRiskType'
  { -- | Indicates whether compromised credentials were detected during an
    -- authentication event.
    compromisedCredentialsDetected :: Prelude.Maybe Prelude.Bool,
    -- | The risk decision.
    riskDecision :: Prelude.Maybe RiskDecisionType,
    -- | The risk level.
    riskLevel :: Prelude.Maybe RiskLevelType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      riskDecision = Prelude.Nothing,
      riskLevel = Prelude.Nothing
    }

-- | Indicates whether compromised credentials were detected during an
-- authentication event.
eventRiskType_compromisedCredentialsDetected :: Lens.Lens' EventRiskType (Prelude.Maybe Prelude.Bool)
eventRiskType_compromisedCredentialsDetected = Lens.lens (\EventRiskType' {compromisedCredentialsDetected} -> compromisedCredentialsDetected) (\s@EventRiskType' {} a -> s {compromisedCredentialsDetected = a} :: EventRiskType)

-- | The risk decision.
eventRiskType_riskDecision :: Lens.Lens' EventRiskType (Prelude.Maybe RiskDecisionType)
eventRiskType_riskDecision = Lens.lens (\EventRiskType' {riskDecision} -> riskDecision) (\s@EventRiskType' {} a -> s {riskDecision = a} :: EventRiskType)

-- | The risk level.
eventRiskType_riskLevel :: Lens.Lens' EventRiskType (Prelude.Maybe RiskLevelType)
eventRiskType_riskLevel = Lens.lens (\EventRiskType' {riskLevel} -> riskLevel) (\s@EventRiskType' {} a -> s {riskLevel = a} :: EventRiskType)

instance Prelude.FromJSON EventRiskType where
  parseJSON =
    Prelude.withObject
      "EventRiskType"
      ( \x ->
          EventRiskType'
            Prelude.<$> (x Prelude..:? "CompromisedCredentialsDetected")
            Prelude.<*> (x Prelude..:? "RiskDecision")
            Prelude.<*> (x Prelude..:? "RiskLevel")
      )

instance Prelude.Hashable EventRiskType

instance Prelude.NFData EventRiskType
