-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventRiskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventRiskType
  ( EventRiskType (..),

    -- * Smart constructor
    mkEventRiskType,

    -- * Lenses
    ertCompromisedCredentialsDetected,
    ertRiskLevel,
    ertRiskDecision,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The event risk type.
--
-- /See:/ 'mkEventRiskType' smart constructor.
data EventRiskType = EventRiskType'
  { compromisedCredentialsDetected ::
      Lude.Maybe Lude.Bool,
    riskLevel :: Lude.Maybe RiskLevelType,
    riskDecision :: Lude.Maybe RiskDecisionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventRiskType' with the minimum fields required to make a request.
--
-- * 'compromisedCredentialsDetected' - Indicates whether compromised credentials were detected during an authentication event.
-- * 'riskDecision' - The risk decision.
-- * 'riskLevel' - The risk level.
mkEventRiskType ::
  EventRiskType
mkEventRiskType =
  EventRiskType'
    { compromisedCredentialsDetected = Lude.Nothing,
      riskLevel = Lude.Nothing,
      riskDecision = Lude.Nothing
    }

-- | Indicates whether compromised credentials were detected during an authentication event.
--
-- /Note:/ Consider using 'compromisedCredentialsDetected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertCompromisedCredentialsDetected :: Lens.Lens' EventRiskType (Lude.Maybe Lude.Bool)
ertCompromisedCredentialsDetected = Lens.lens (compromisedCredentialsDetected :: EventRiskType -> Lude.Maybe Lude.Bool) (\s a -> s {compromisedCredentialsDetected = a} :: EventRiskType)
{-# DEPRECATED ertCompromisedCredentialsDetected "Use generic-lens or generic-optics with 'compromisedCredentialsDetected' instead." #-}

-- | The risk level.
--
-- /Note:/ Consider using 'riskLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertRiskLevel :: Lens.Lens' EventRiskType (Lude.Maybe RiskLevelType)
ertRiskLevel = Lens.lens (riskLevel :: EventRiskType -> Lude.Maybe RiskLevelType) (\s a -> s {riskLevel = a} :: EventRiskType)
{-# DEPRECATED ertRiskLevel "Use generic-lens or generic-optics with 'riskLevel' instead." #-}

-- | The risk decision.
--
-- /Note:/ Consider using 'riskDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ertRiskDecision :: Lens.Lens' EventRiskType (Lude.Maybe RiskDecisionType)
ertRiskDecision = Lens.lens (riskDecision :: EventRiskType -> Lude.Maybe RiskDecisionType) (\s a -> s {riskDecision = a} :: EventRiskType)
{-# DEPRECATED ertRiskDecision "Use generic-lens or generic-optics with 'riskDecision' instead." #-}

instance Lude.FromJSON EventRiskType where
  parseJSON =
    Lude.withObject
      "EventRiskType"
      ( \x ->
          EventRiskType'
            Lude.<$> (x Lude..:? "CompromisedCredentialsDetected")
            Lude.<*> (x Lude..:? "RiskLevel")
            Lude.<*> (x Lude..:? "RiskDecision")
      )
