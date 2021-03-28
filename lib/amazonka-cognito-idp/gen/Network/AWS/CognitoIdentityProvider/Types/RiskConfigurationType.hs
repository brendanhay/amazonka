{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
  ( RiskConfigurationType (..)
  -- * Smart constructor
  , mkRiskConfigurationType
  -- * Lenses
  , rctAccountTakeoverRiskConfiguration
  , rctClientId
  , rctCompromisedCredentialsRiskConfiguration
  , rctLastModifiedDate
  , rctRiskExceptionConfiguration
  , rctUserPoolId
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientIdType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The risk configuration type.
--
-- /See:/ 'mkRiskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { accountTakeoverRiskConfiguration :: Core.Maybe Types.AccountTakeoverRiskConfigurationType
    -- ^ The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
  , clientId :: Core.Maybe Types.ClientIdType
    -- ^ The app client ID.
  , compromisedCredentialsRiskConfiguration :: Core.Maybe Types.CompromisedCredentialsRiskConfigurationType
    -- ^ The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@ 
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified date.
  , riskExceptionConfiguration :: Core.Maybe Types.RiskExceptionConfigurationType
    -- ^ The configuration to override the risk decision.
  , userPoolId :: Core.Maybe Types.UserPoolIdType
    -- ^ The user pool ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RiskConfigurationType' value with any optional fields omitted.
mkRiskConfigurationType
    :: RiskConfigurationType
mkRiskConfigurationType
  = RiskConfigurationType'{accountTakeoverRiskConfiguration =
                             Core.Nothing,
                           clientId = Core.Nothing,
                           compromisedCredentialsRiskConfiguration = Core.Nothing,
                           lastModifiedDate = Core.Nothing,
                           riskExceptionConfiguration = Core.Nothing,
                           userPoolId = Core.Nothing}

-- | The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
--
-- /Note:/ Consider using 'accountTakeoverRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctAccountTakeoverRiskConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe Types.AccountTakeoverRiskConfigurationType)
rctAccountTakeoverRiskConfiguration = Lens.field @"accountTakeoverRiskConfiguration"
{-# INLINEABLE rctAccountTakeoverRiskConfiguration #-}
{-# DEPRECATED accountTakeoverRiskConfiguration "Use generic-lens or generic-optics with 'accountTakeoverRiskConfiguration' instead"  #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctClientId :: Lens.Lens' RiskConfigurationType (Core.Maybe Types.ClientIdType)
rctClientId = Lens.field @"clientId"
{-# INLINEABLE rctClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@ 
--
-- /Note:/ Consider using 'compromisedCredentialsRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctCompromisedCredentialsRiskConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe Types.CompromisedCredentialsRiskConfigurationType)
rctCompromisedCredentialsRiskConfiguration = Lens.field @"compromisedCredentialsRiskConfiguration"
{-# INLINEABLE rctCompromisedCredentialsRiskConfiguration #-}
{-# DEPRECATED compromisedCredentialsRiskConfiguration "Use generic-lens or generic-optics with 'compromisedCredentialsRiskConfiguration' instead"  #-}

-- | The last modified date.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctLastModifiedDate :: Lens.Lens' RiskConfigurationType (Core.Maybe Core.NominalDiffTime)
rctLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE rctLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The configuration to override the risk decision.
--
-- /Note:/ Consider using 'riskExceptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRiskExceptionConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe Types.RiskExceptionConfigurationType)
rctRiskExceptionConfiguration = Lens.field @"riskExceptionConfiguration"
{-# INLINEABLE rctRiskExceptionConfiguration #-}
{-# DEPRECATED riskExceptionConfiguration "Use generic-lens or generic-optics with 'riskExceptionConfiguration' instead"  #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctUserPoolId :: Lens.Lens' RiskConfigurationType (Core.Maybe Types.UserPoolIdType)
rctUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE rctUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.FromJSON RiskConfigurationType where
        parseJSON
          = Core.withObject "RiskConfigurationType" Core.$
              \ x ->
                RiskConfigurationType' Core.<$>
                  (x Core..:? "AccountTakeoverRiskConfiguration") Core.<*>
                    x Core..:? "ClientId"
                    Core.<*> x Core..:? "CompromisedCredentialsRiskConfiguration"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "RiskExceptionConfiguration"
                    Core.<*> x Core..:? "UserPoolId"
