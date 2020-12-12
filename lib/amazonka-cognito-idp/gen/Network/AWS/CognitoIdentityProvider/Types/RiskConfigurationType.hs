{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
  ( RiskConfigurationType (..),

    -- * Smart constructor
    mkRiskConfigurationType,

    -- * Lenses
    rctRiskExceptionConfiguration,
    rctClientId,
    rctAccountTakeoverRiskConfiguration,
    rctLastModifiedDate,
    rctUserPoolId,
    rctCompromisedCredentialsRiskConfiguration,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The risk configuration type.
--
-- /See:/ 'mkRiskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { riskExceptionConfiguration ::
      Lude.Maybe RiskExceptionConfigurationType,
    clientId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    accountTakeoverRiskConfiguration ::
      Lude.Maybe AccountTakeoverRiskConfigurationType,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    userPoolId :: Lude.Maybe Lude.Text,
    compromisedCredentialsRiskConfiguration ::
      Lude.Maybe
        CompromisedCredentialsRiskConfigurationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RiskConfigurationType' with the minimum fields required to make a request.
--
-- * 'accountTakeoverRiskConfiguration' - The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
-- * 'clientId' - The app client ID.
-- * 'compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@
-- * 'lastModifiedDate' - The last modified date.
-- * 'riskExceptionConfiguration' - The configuration to override the risk decision.
-- * 'userPoolId' - The user pool ID.
mkRiskConfigurationType ::
  RiskConfigurationType
mkRiskConfigurationType =
  RiskConfigurationType'
    { riskExceptionConfiguration = Lude.Nothing,
      clientId = Lude.Nothing,
      accountTakeoverRiskConfiguration = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      userPoolId = Lude.Nothing,
      compromisedCredentialsRiskConfiguration = Lude.Nothing
    }

-- | The configuration to override the risk decision.
--
-- /Note:/ Consider using 'riskExceptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRiskExceptionConfiguration :: Lens.Lens' RiskConfigurationType (Lude.Maybe RiskExceptionConfigurationType)
rctRiskExceptionConfiguration = Lens.lens (riskExceptionConfiguration :: RiskConfigurationType -> Lude.Maybe RiskExceptionConfigurationType) (\s a -> s {riskExceptionConfiguration = a} :: RiskConfigurationType)
{-# DEPRECATED rctRiskExceptionConfiguration "Use generic-lens or generic-optics with 'riskExceptionConfiguration' instead." #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctClientId :: Lens.Lens' RiskConfigurationType (Lude.Maybe (Lude.Sensitive Lude.Text))
rctClientId = Lens.lens (clientId :: RiskConfigurationType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: RiskConfigurationType)
{-# DEPRECATED rctClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
--
-- /Note:/ Consider using 'accountTakeoverRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctAccountTakeoverRiskConfiguration :: Lens.Lens' RiskConfigurationType (Lude.Maybe AccountTakeoverRiskConfigurationType)
rctAccountTakeoverRiskConfiguration = Lens.lens (accountTakeoverRiskConfiguration :: RiskConfigurationType -> Lude.Maybe AccountTakeoverRiskConfigurationType) (\s a -> s {accountTakeoverRiskConfiguration = a} :: RiskConfigurationType)
{-# DEPRECATED rctAccountTakeoverRiskConfiguration "Use generic-lens or generic-optics with 'accountTakeoverRiskConfiguration' instead." #-}

-- | The last modified date.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctLastModifiedDate :: Lens.Lens' RiskConfigurationType (Lude.Maybe Lude.Timestamp)
rctLastModifiedDate = Lens.lens (lastModifiedDate :: RiskConfigurationType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: RiskConfigurationType)
{-# DEPRECATED rctLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctUserPoolId :: Lens.Lens' RiskConfigurationType (Lude.Maybe Lude.Text)
rctUserPoolId = Lens.lens (userPoolId :: RiskConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: RiskConfigurationType)
{-# DEPRECATED rctUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@
--
-- /Note:/ Consider using 'compromisedCredentialsRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctCompromisedCredentialsRiskConfiguration :: Lens.Lens' RiskConfigurationType (Lude.Maybe CompromisedCredentialsRiskConfigurationType)
rctCompromisedCredentialsRiskConfiguration = Lens.lens (compromisedCredentialsRiskConfiguration :: RiskConfigurationType -> Lude.Maybe CompromisedCredentialsRiskConfigurationType) (\s a -> s {compromisedCredentialsRiskConfiguration = a} :: RiskConfigurationType)
{-# DEPRECATED rctCompromisedCredentialsRiskConfiguration "Use generic-lens or generic-optics with 'compromisedCredentialsRiskConfiguration' instead." #-}

instance Lude.FromJSON RiskConfigurationType where
  parseJSON =
    Lude.withObject
      "RiskConfigurationType"
      ( \x ->
          RiskConfigurationType'
            Lude.<$> (x Lude..:? "RiskExceptionConfiguration")
            Lude.<*> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "AccountTakeoverRiskConfiguration")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "CompromisedCredentialsRiskConfiguration")
      )
