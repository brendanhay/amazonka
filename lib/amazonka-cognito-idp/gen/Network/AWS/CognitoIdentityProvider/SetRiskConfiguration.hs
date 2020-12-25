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
    srcUserPoolId,
    srcAccountTakeoverRiskConfiguration,
    srcClientId,
    srcCompromisedCredentialsRiskConfiguration,
    srcRiskExceptionConfiguration,

    -- * Destructuring the response
    SetRiskConfigurationResponse (..),
    mkSetRiskConfigurationResponse,

    -- ** Response lenses
    srcrrsRiskConfiguration,
    srcrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetRiskConfiguration' smart constructor.
data SetRiskConfiguration = SetRiskConfiguration'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The account takeover risk configuration.
    accountTakeoverRiskConfiguration :: Core.Maybe Types.AccountTakeoverRiskConfigurationType,
    -- | The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool.
    --
    -- Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
    clientId :: Core.Maybe Types.ClientId,
    -- | The compromised credentials risk configuration.
    compromisedCredentialsRiskConfiguration :: Core.Maybe Types.CompromisedCredentialsRiskConfigurationType,
    -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Core.Maybe Types.RiskExceptionConfigurationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRiskConfiguration' value with any optional fields omitted.
mkSetRiskConfiguration ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  SetRiskConfiguration
mkSetRiskConfiguration userPoolId =
  SetRiskConfiguration'
    { userPoolId,
      accountTakeoverRiskConfiguration = Core.Nothing,
      clientId = Core.Nothing,
      compromisedCredentialsRiskConfiguration = Core.Nothing,
      riskExceptionConfiguration = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcUserPoolId :: Lens.Lens' SetRiskConfiguration Types.UserPoolId
srcUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED srcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The account takeover risk configuration.
--
-- /Note:/ Consider using 'accountTakeoverRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcAccountTakeoverRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe Types.AccountTakeoverRiskConfigurationType)
srcAccountTakeoverRiskConfiguration = Lens.field @"accountTakeoverRiskConfiguration"
{-# DEPRECATED srcAccountTakeoverRiskConfiguration "Use generic-lens or generic-optics with 'accountTakeoverRiskConfiguration' instead." #-}

-- | The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool.
--
-- Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcClientId :: Lens.Lens' SetRiskConfiguration (Core.Maybe Types.ClientId)
srcClientId = Lens.field @"clientId"
{-# DEPRECATED srcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The compromised credentials risk configuration.
--
-- /Note:/ Consider using 'compromisedCredentialsRiskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcCompromisedCredentialsRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe Types.CompromisedCredentialsRiskConfigurationType)
srcCompromisedCredentialsRiskConfiguration = Lens.field @"compromisedCredentialsRiskConfiguration"
{-# DEPRECATED srcCompromisedCredentialsRiskConfiguration "Use generic-lens or generic-optics with 'compromisedCredentialsRiskConfiguration' instead." #-}

-- | The configuration to override the risk decision.
--
-- /Note:/ Consider using 'riskExceptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcRiskExceptionConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe Types.RiskExceptionConfigurationType)
srcRiskExceptionConfiguration = Lens.field @"riskExceptionConfiguration"
{-# DEPRECATED srcRiskExceptionConfiguration "Use generic-lens or generic-optics with 'riskExceptionConfiguration' instead." #-}

instance Core.FromJSON SetRiskConfiguration where
  toJSON SetRiskConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            ("AccountTakeoverRiskConfiguration" Core..=)
              Core.<$> accountTakeoverRiskConfiguration,
            ("ClientId" Core..=) Core.<$> clientId,
            ("CompromisedCredentialsRiskConfiguration" Core..=)
              Core.<$> compromisedCredentialsRiskConfiguration,
            ("RiskExceptionConfiguration" Core..=)
              Core.<$> riskExceptionConfiguration
          ]
      )

instance Core.AWSRequest SetRiskConfiguration where
  type Rs SetRiskConfiguration = SetRiskConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.SetRiskConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRiskConfigurationResponse'
            Core.<$> (x Core..: "RiskConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetRiskConfigurationResponse' smart constructor.
data SetRiskConfigurationResponse = SetRiskConfigurationResponse'
  { -- | The risk configuration.
    riskConfiguration :: Types.RiskConfigurationType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SetRiskConfigurationResponse' value with any optional fields omitted.
mkSetRiskConfigurationResponse ::
  -- | 'riskConfiguration'
  Types.RiskConfigurationType ->
  -- | 'responseStatus'
  Core.Int ->
  SetRiskConfigurationResponse
mkSetRiskConfigurationResponse riskConfiguration responseStatus =
  SetRiskConfigurationResponse' {riskConfiguration, responseStatus}

-- | The risk configuration.
--
-- /Note:/ Consider using 'riskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsRiskConfiguration :: Lens.Lens' SetRiskConfigurationResponse Types.RiskConfigurationType
srcrrsRiskConfiguration = Lens.field @"riskConfiguration"
{-# DEPRECATED srcrrsRiskConfiguration "Use generic-lens or generic-optics with 'riskConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsResponseStatus :: Lens.Lens' SetRiskConfigurationResponse Core.Int
srcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
