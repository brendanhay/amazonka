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
    aFixedResponseConfig,
    aTargetGroupARN,
    aForwardConfig,
    aRedirectConfig,
    aAuthenticateCognitoConfig,
    aOrder,
    aAuthenticateOidcConfig,
    aType,
  )
where

import Network.AWS.ELBv2.Types.ActionTypeEnum
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
import Network.AWS.ELBv2.Types.FixedResponseActionConfig
import Network.AWS.ELBv2.Types.ForwardActionConfig
import Network.AWS.ELBv2.Types.RedirectActionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an action.
--
-- Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ , and it must be the last action to be performed.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { fixedResponseConfig ::
      Lude.Maybe FixedResponseActionConfig,
    targetGroupARN :: Lude.Maybe Lude.Text,
    forwardConfig :: Lude.Maybe ForwardActionConfig,
    redirectConfig :: Lude.Maybe RedirectActionConfig,
    authenticateCognitoConfig ::
      Lude.Maybe AuthenticateCognitoActionConfig,
    order :: Lude.Maybe Lude.Natural,
    authenticateOidcConfig :: Lude.Maybe AuthenticateOidcActionConfig,
    type' :: ActionTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- * 'authenticateCognitoConfig' - [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
-- * 'authenticateOidcConfig' - [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
-- * 'fixedResponseConfig' - [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
-- * 'forwardConfig' - Information for creating an action that distributes requests among one or more target groups. For Network Load Balancers, you can specify a single target group. Specify only when @Type@ is @forward@ . If you specify both @ForwardConfig@ and @TargetGroupArn@ , you can specify only one target group using @ForwardConfig@ and it must be the same target group specified in @TargetGroupArn@ .
-- * 'order' - The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first.
-- * 'redirectConfig' - [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ and you want to route to a single target group. To route to one or more target groups, use @ForwardConfig@ instead.
-- * 'type'' - The type of action.
mkAction ::
  -- | 'type''
  ActionTypeEnum ->
  Action
mkAction pType_ =
  Action'
    { fixedResponseConfig = Lude.Nothing,
      targetGroupARN = Lude.Nothing,
      forwardConfig = Lude.Nothing,
      redirectConfig = Lude.Nothing,
      authenticateCognitoConfig = Lude.Nothing,
      order = Lude.Nothing,
      authenticateOidcConfig = Lude.Nothing,
      type' = pType_
    }

-- | [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
--
-- /Note:/ Consider using 'fixedResponseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFixedResponseConfig :: Lens.Lens' Action (Lude.Maybe FixedResponseActionConfig)
aFixedResponseConfig = Lens.lens (fixedResponseConfig :: Action -> Lude.Maybe FixedResponseActionConfig) (\s a -> s {fixedResponseConfig = a} :: Action)
{-# DEPRECATED aFixedResponseConfig "Use generic-lens or generic-optics with 'fixedResponseConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ and you want to route to a single target group. To route to one or more target groups, use @ForwardConfig@ instead.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetGroupARN :: Lens.Lens' Action (Lude.Maybe Lude.Text)
aTargetGroupARN = Lens.lens (targetGroupARN :: Action -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupARN = a} :: Action)
{-# DEPRECATED aTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | Information for creating an action that distributes requests among one or more target groups. For Network Load Balancers, you can specify a single target group. Specify only when @Type@ is @forward@ . If you specify both @ForwardConfig@ and @TargetGroupArn@ , you can specify only one target group using @ForwardConfig@ and it must be the same target group specified in @TargetGroupArn@ .
--
-- /Note:/ Consider using 'forwardConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aForwardConfig :: Lens.Lens' Action (Lude.Maybe ForwardActionConfig)
aForwardConfig = Lens.lens (forwardConfig :: Action -> Lude.Maybe ForwardActionConfig) (\s a -> s {forwardConfig = a} :: Action)
{-# DEPRECATED aForwardConfig "Use generic-lens or generic-optics with 'forwardConfig' instead." #-}

-- | [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
--
-- /Note:/ Consider using 'redirectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRedirectConfig :: Lens.Lens' Action (Lude.Maybe RedirectActionConfig)
aRedirectConfig = Lens.lens (redirectConfig :: Action -> Lude.Maybe RedirectActionConfig) (\s a -> s {redirectConfig = a} :: Action)
{-# DEPRECATED aRedirectConfig "Use generic-lens or generic-optics with 'redirectConfig' instead." #-}

-- | [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
--
-- /Note:/ Consider using 'authenticateCognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthenticateCognitoConfig :: Lens.Lens' Action (Lude.Maybe AuthenticateCognitoActionConfig)
aAuthenticateCognitoConfig = Lens.lens (authenticateCognitoConfig :: Action -> Lude.Maybe AuthenticateCognitoActionConfig) (\s a -> s {authenticateCognitoConfig = a} :: Action)
{-# DEPRECATED aAuthenticateCognitoConfig "Use generic-lens or generic-optics with 'authenticateCognitoConfig' instead." #-}

-- | The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOrder :: Lens.Lens' Action (Lude.Maybe Lude.Natural)
aOrder = Lens.lens (order :: Action -> Lude.Maybe Lude.Natural) (\s a -> s {order = a} :: Action)
{-# DEPRECATED aOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
--
-- /Note:/ Consider using 'authenticateOidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthenticateOidcConfig :: Lens.Lens' Action (Lude.Maybe AuthenticateOidcActionConfig)
aAuthenticateOidcConfig = Lens.lens (authenticateOidcConfig :: Action -> Lude.Maybe AuthenticateOidcActionConfig) (\s a -> s {authenticateOidcConfig = a} :: Action)
{-# DEPRECATED aAuthenticateOidcConfig "Use generic-lens or generic-optics with 'authenticateOidcConfig' instead." #-}

-- | The type of action.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Action ActionTypeEnum
aType = Lens.lens (type' :: Action -> ActionTypeEnum) (\s a -> s {type' = a} :: Action)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML Action where
  parseXML x =
    Action'
      Lude.<$> (x Lude..@? "FixedResponseConfig")
      Lude.<*> (x Lude..@? "TargetGroupArn")
      Lude.<*> (x Lude..@? "ForwardConfig")
      Lude.<*> (x Lude..@? "RedirectConfig")
      Lude.<*> (x Lude..@? "AuthenticateCognitoConfig")
      Lude.<*> (x Lude..@? "Order")
      Lude.<*> (x Lude..@? "AuthenticateOidcConfig")
      Lude.<*> (x Lude..@ "Type")

instance Lude.ToQuery Action where
  toQuery Action' {..} =
    Lude.mconcat
      [ "FixedResponseConfig" Lude.=: fixedResponseConfig,
        "TargetGroupArn" Lude.=: targetGroupARN,
        "ForwardConfig" Lude.=: forwardConfig,
        "RedirectConfig" Lude.=: redirectConfig,
        "AuthenticateCognitoConfig" Lude.=: authenticateCognitoConfig,
        "Order" Lude.=: order,
        "AuthenticateOidcConfig" Lude.=: authenticateOidcConfig,
        "Type" Lude.=: type'
      ]
