{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures actions on detected risks. To delete the risk configuration for @UserPoolId@ or @ClientId@ , pass null values for all four configuration types.
--
-- To enable Amazon Cognito advanced security features, update the user pool to include the @UserPoolAddOns@ key@AdvancedSecurityMode@ .
module Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
  ( -- * Creating a request
    SetRiskConfiguration (..),
    mkSetRiskConfiguration,

    -- ** Request lenses
    srcRiskExceptionConfiguration,
    srcClientId,
    srcAccountTakeoverRiskConfiguration,
    srcUserPoolId,
    srcCompromisedCredentialsRiskConfiguration,

    -- * Destructuring the response
    SetRiskConfigurationResponse (..),
    mkSetRiskConfigurationResponse,

    -- ** Response lenses
    srcrsRiskConfiguration,
    srcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetRiskConfiguration' smart constructor.
data SetRiskConfiguration = SetRiskConfiguration'
  { -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Lude.Maybe RiskExceptionConfigurationType,
    -- | The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool.
    --
    -- Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
    clientId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The account takeover risk configuration.
    accountTakeoverRiskConfiguration :: Lude.Maybe AccountTakeoverRiskConfigurationType,
    -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The compromised credentials risk configuration.
    compromisedCredentialsRiskConfiguration :: Lude.Maybe CompromisedCredentialsRiskConfigurationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRiskConfiguration' with the minimum fields required to make a request.
--
-- * 'riskExceptionConfiguration' - The configuration to override the risk decision.
-- * 'clientId' - The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool.
--
-- Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
-- * 'accountTakeoverRiskConfiguration' - The account takeover risk configuration.
-- * 'userPoolId' - The user pool ID.
-- * 'compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration.
mkSetRiskConfiguration ::
  -- | 'userPoolId'
  Lude.Text ->
  SetRiskConfiguration
mkSetRiskConfiguration pUserPoolId_ =
  SetRiskConfiguration'
    { riskExceptionConfiguration = Lude.Nothing,
      clientId = Lude.Nothing,
      accountTakeoverRiskConfiguration = Lude.Nothing,
      userPoolId = pUserPoolId_,
      compromisedCredentialsRiskConfiguration = Lude.Nothing
    }

-- | The configuration to override the risk decision.
--
-- /Note:/ Consider using 'riskExceptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcRiskExceptionConfiguration :: Lens.Lens' SetRiskConfiguration (Lude.Maybe RiskExceptionConfigurationType)
srcRiskExceptionConfiguration = Lens.lens (riskExceptionConfiguration :: SetRiskConfiguration -> Lude.Maybe RiskExceptionConfigurationType) (\s a -> s {riskExceptionConfiguration = a} :: SetRiskConfiguration)
{-# DEPRECATED srcRiskExceptionConfiguration "Use generic-lens or generic-optics with 'riskExceptionConfiguration' instead." #-}

-- | The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool.
--
-- Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcClientId :: Lens.Lens' SetRiskConfiguration (Lude.Maybe (Lude.Sensitive Lude.Text))
srcClientId = Lens.lens (clientId :: SetRiskConfiguration -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: SetRiskConfiguration)
{-# DEPRECATED srcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The account takeover risk configuration.
--
-- /Note:/ Consider using 'accountTakeoverRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcAccountTakeoverRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Lude.Maybe AccountTakeoverRiskConfigurationType)
srcAccountTakeoverRiskConfiguration = Lens.lens (accountTakeoverRiskConfiguration :: SetRiskConfiguration -> Lude.Maybe AccountTakeoverRiskConfigurationType) (\s a -> s {accountTakeoverRiskConfiguration = a} :: SetRiskConfiguration)
{-# DEPRECATED srcAccountTakeoverRiskConfiguration "Use generic-lens or generic-optics with 'accountTakeoverRiskConfiguration' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcUserPoolId :: Lens.Lens' SetRiskConfiguration Lude.Text
srcUserPoolId = Lens.lens (userPoolId :: SetRiskConfiguration -> Lude.Text) (\s a -> s {userPoolId = a} :: SetRiskConfiguration)
{-# DEPRECATED srcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The compromised credentials risk configuration.
--
-- /Note:/ Consider using 'compromisedCredentialsRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcCompromisedCredentialsRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Lude.Maybe CompromisedCredentialsRiskConfigurationType)
srcCompromisedCredentialsRiskConfiguration = Lens.lens (compromisedCredentialsRiskConfiguration :: SetRiskConfiguration -> Lude.Maybe CompromisedCredentialsRiskConfigurationType) (\s a -> s {compromisedCredentialsRiskConfiguration = a} :: SetRiskConfiguration)
{-# DEPRECATED srcCompromisedCredentialsRiskConfiguration "Use generic-lens or generic-optics with 'compromisedCredentialsRiskConfiguration' instead." #-}

instance Lude.AWSRequest SetRiskConfiguration where
  type Rs SetRiskConfiguration = SetRiskConfigurationResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetRiskConfigurationResponse'
            Lude.<$> (x Lude..:> "RiskConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetRiskConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.SetRiskConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetRiskConfiguration where
  toJSON SetRiskConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RiskExceptionConfiguration" Lude..=)
              Lude.<$> riskExceptionConfiguration,
            ("ClientId" Lude..=) Lude.<$> clientId,
            ("AccountTakeoverRiskConfiguration" Lude..=)
              Lude.<$> accountTakeoverRiskConfiguration,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("CompromisedCredentialsRiskConfiguration" Lude..=)
              Lude.<$> compromisedCredentialsRiskConfiguration
          ]
      )

instance Lude.ToPath SetRiskConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery SetRiskConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetRiskConfigurationResponse' smart constructor.
data SetRiskConfigurationResponse = SetRiskConfigurationResponse'
  { -- | The risk configuration.
    riskConfiguration :: RiskConfigurationType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRiskConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'riskConfiguration' - The risk configuration.
-- * 'responseStatus' - The response status code.
mkSetRiskConfigurationResponse ::
  -- | 'riskConfiguration'
  RiskConfigurationType ->
  -- | 'responseStatus'
  Lude.Int ->
  SetRiskConfigurationResponse
mkSetRiskConfigurationResponse pRiskConfiguration_ pResponseStatus_ =
  SetRiskConfigurationResponse'
    { riskConfiguration =
        pRiskConfiguration_,
      responseStatus = pResponseStatus_
    }

-- | The risk configuration.
--
-- /Note:/ Consider using 'riskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsRiskConfiguration :: Lens.Lens' SetRiskConfigurationResponse RiskConfigurationType
srcrsRiskConfiguration = Lens.lens (riskConfiguration :: SetRiskConfigurationResponse -> RiskConfigurationType) (\s a -> s {riskConfiguration = a} :: SetRiskConfigurationResponse)
{-# DEPRECATED srcrsRiskConfiguration "Use generic-lens or generic-optics with 'riskConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsResponseStatus :: Lens.Lens' SetRiskConfigurationResponse Lude.Int
srcrsResponseStatus = Lens.lens (responseStatus :: SetRiskConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetRiskConfigurationResponse)
{-# DEPRECATED srcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
