{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventRiskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.EventRiskType
  ( EventRiskType (..)
  -- * Smart constructor
  , mkEventRiskType
  -- * Lenses
  , ertCompromisedCredentialsDetected
  , ertRiskDecision
  , ertRiskLevel
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.RiskLevelType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The event risk type.
--
-- /See:/ 'mkEventRiskType' smart constructor.
data EventRiskType = EventRiskType'
  { compromisedCredentialsDetected :: Core.Maybe Core.Bool
    -- ^ Indicates whether compromised credentials were detected during an authentication event.
  , riskDecision :: Core.Maybe Types.RiskDecisionType
    -- ^ The risk decision.
  , riskLevel :: Core.Maybe Types.RiskLevelType
    -- ^ The risk level.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventRiskType' value with any optional fields omitted.
mkEventRiskType
    :: EventRiskType
mkEventRiskType
  = EventRiskType'{compromisedCredentialsDetected = Core.Nothing,
                   riskDecision = Core.Nothing, riskLevel = Core.Nothing}

-- | Indicates whether compromised credentials were detected during an authentication event.
--
-- /Note:/ Consider using 'compromisedCredentialsDetected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertCompromisedCredentialsDetected :: Lens.Lens' EventRiskType (Core.Maybe Core.Bool)
ertCompromisedCredentialsDetected = Lens.field @"compromisedCredentialsDetected"
{-# INLINEABLE ertCompromisedCredentialsDetected #-}
{-# DEPRECATED compromisedCredentialsDetected "Use generic-lens or generic-optics with 'compromisedCredentialsDetected' instead"  #-}

-- | The risk decision.
--
-- /Note:/ Consider using 'riskDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertRiskDecision :: Lens.Lens' EventRiskType (Core.Maybe Types.RiskDecisionType)
ertRiskDecision = Lens.field @"riskDecision"
{-# INLINEABLE ertRiskDecision #-}
{-# DEPRECATED riskDecision "Use generic-lens or generic-optics with 'riskDecision' instead"  #-}

-- | The risk level.
--
-- /Note:/ Consider using 'riskLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertRiskLevel :: Lens.Lens' EventRiskType (Core.Maybe Types.RiskLevelType)
ertRiskLevel = Lens.field @"riskLevel"
{-# INLINEABLE ertRiskLevel #-}
{-# DEPRECATED riskLevel "Use generic-lens or generic-optics with 'riskLevel' instead"  #-}

instance Core.FromJSON EventRiskType where
        parseJSON
          = Core.withObject "EventRiskType" Core.$
              \ x ->
                EventRiskType' Core.<$>
                  (x Core..:? "CompromisedCredentialsDetected") Core.<*>
                    x Core..:? "RiskDecision"
                    Core.<*> x Core..:? "RiskLevel"
