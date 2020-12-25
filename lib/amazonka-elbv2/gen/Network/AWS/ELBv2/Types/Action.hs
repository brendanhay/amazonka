{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aType,
    aAuthenticateCognitoConfig,
    aAuthenticateOidcConfig,
    aFixedResponseConfig,
    aForwardConfig,
    aOrder,
    aRedirectConfig,
    aTargetGroupArn,
  )
where

import qualified Network.AWS.ELBv2.Types.ActionTypeEnum as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig as Types
import qualified Network.AWS.ELBv2.Types.FixedResponseActionConfig as Types
import qualified Network.AWS.ELBv2.Types.ForwardActionConfig as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionConfig as Types
import qualified Network.AWS.ELBv2.Types.TargetGroupArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an action.
--
-- Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ , and it must be the last action to be performed.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | The type of action.
    type' :: Types.ActionTypeEnum,
    -- | [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
    authenticateCognitoConfig :: Core.Maybe Types.AuthenticateCognitoActionConfig,
    -- | [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
    authenticateOidcConfig :: Core.Maybe Types.AuthenticateOidcActionConfig,
    -- | [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
    fixedResponseConfig :: Core.Maybe Types.FixedResponseActionConfig,
    -- | Information for creating an action that distributes requests among one or more target groups. For Network Load Balancers, you can specify a single target group. Specify only when @Type@ is @forward@ . If you specify both @ForwardConfig@ and @TargetGroupArn@ , you can specify only one target group using @ForwardConfig@ and it must be the same target group specified in @TargetGroupArn@ .
    forwardConfig :: Core.Maybe Types.ForwardActionConfig,
    -- | The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first.
    order :: Core.Maybe Core.Natural,
    -- | [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
    redirectConfig :: Core.Maybe Types.RedirectActionConfig,
    -- | The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ and you want to route to a single target group. To route to one or more target groups, use @ForwardConfig@ instead.
    targetGroupArn :: Core.Maybe Types.TargetGroupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Action' value with any optional fields omitted.
mkAction ::
  -- | 'type\''
  Types.ActionTypeEnum ->
  Action
mkAction type' =
  Action'
    { type',
      authenticateCognitoConfig = Core.Nothing,
      authenticateOidcConfig = Core.Nothing,
      fixedResponseConfig = Core.Nothing,
      forwardConfig = Core.Nothing,
      order = Core.Nothing,
      redirectConfig = Core.Nothing,
      targetGroupArn = Core.Nothing
    }

-- | The type of action.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Action Types.ActionTypeEnum
aType = Lens.field @"type'"
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
--
-- /Note:/ Consider using 'authenticateCognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthenticateCognitoConfig :: Lens.Lens' Action (Core.Maybe Types.AuthenticateCognitoActionConfig)
aAuthenticateCognitoConfig = Lens.field @"authenticateCognitoConfig"
{-# DEPRECATED aAuthenticateCognitoConfig "Use generic-lens or generic-optics with 'authenticateCognitoConfig' instead." #-}

-- | [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
--
-- /Note:/ Consider using 'authenticateOidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthenticateOidcConfig :: Lens.Lens' Action (Core.Maybe Types.AuthenticateOidcActionConfig)
aAuthenticateOidcConfig = Lens.field @"authenticateOidcConfig"
{-# DEPRECATED aAuthenticateOidcConfig "Use generic-lens or generic-optics with 'authenticateOidcConfig' instead." #-}

-- | [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
--
-- /Note:/ Consider using 'fixedResponseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFixedResponseConfig :: Lens.Lens' Action (Core.Maybe Types.FixedResponseActionConfig)
aFixedResponseConfig = Lens.field @"fixedResponseConfig"
{-# DEPRECATED aFixedResponseConfig "Use generic-lens or generic-optics with 'fixedResponseConfig' instead." #-}

-- | Information for creating an action that distributes requests among one or more target groups. For Network Load Balancers, you can specify a single target group. Specify only when @Type@ is @forward@ . If you specify both @ForwardConfig@ and @TargetGroupArn@ , you can specify only one target group using @ForwardConfig@ and it must be the same target group specified in @TargetGroupArn@ .
--
-- /Note:/ Consider using 'forwardConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aForwardConfig :: Lens.Lens' Action (Core.Maybe Types.ForwardActionConfig)
aForwardConfig = Lens.field @"forwardConfig"
{-# DEPRECATED aForwardConfig "Use generic-lens or generic-optics with 'forwardConfig' instead." #-}

-- | The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOrder :: Lens.Lens' Action (Core.Maybe Core.Natural)
aOrder = Lens.field @"order"
{-# DEPRECATED aOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
--
-- /Note:/ Consider using 'redirectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRedirectConfig :: Lens.Lens' Action (Core.Maybe Types.RedirectActionConfig)
aRedirectConfig = Lens.field @"redirectConfig"
{-# DEPRECATED aRedirectConfig "Use generic-lens or generic-optics with 'redirectConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ and you want to route to a single target group. To route to one or more target groups, use @ForwardConfig@ instead.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetGroupArn :: Lens.Lens' Action (Core.Maybe Types.TargetGroupArn)
aTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED aTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

instance Core.FromXML Action where
  parseXML x =
    Action'
      Core.<$> (x Core..@ "Type")
      Core.<*> (x Core..@? "AuthenticateCognitoConfig")
      Core.<*> (x Core..@? "AuthenticateOidcConfig")
      Core.<*> (x Core..@? "FixedResponseConfig")
      Core.<*> (x Core..@? "ForwardConfig")
      Core.<*> (x Core..@? "Order")
      Core.<*> (x Core..@? "RedirectConfig")
      Core.<*> (x Core..@? "TargetGroupArn")
