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
-- Module      : Amazonka.ELBV2.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.ActionTypeEnum
import Amazonka.ELBV2.Types.AuthenticateCognitoActionConfig
import Amazonka.ELBV2.Types.AuthenticateOidcActionConfig
import Amazonka.ELBV2.Types.FixedResponseActionConfig
import Amazonka.ELBV2.Types.ForwardActionConfig
import Amazonka.ELBV2.Types.RedirectActionConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about an action.
--
-- Each rule must include exactly one of the following types of actions:
-- @forward@, @fixed-response@, or @redirect@, and it must be the last
-- action to be performed.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | [HTTPS listeners] Information for using Amazon Cognito to authenticate
    -- users. Specify only when @Type@ is @authenticate-cognito@.
    authenticateCognitoConfig :: Prelude.Maybe AuthenticateCognitoActionConfig,
    -- | [HTTPS listeners] Information about an identity provider that is
    -- compliant with OpenID Connect (OIDC). Specify only when @Type@ is
    -- @authenticate-oidc@.
    authenticateOidcConfig :: Prelude.Maybe AuthenticateOidcActionConfig,
    -- | [Application Load Balancer] Information for creating an action that
    -- returns a custom HTTP response. Specify only when @Type@ is
    -- @fixed-response@.
    fixedResponseConfig :: Prelude.Maybe FixedResponseActionConfig,
    -- | Information for creating an action that distributes requests among one
    -- or more target groups. For Network Load Balancers, you can specify a
    -- single target group. Specify only when @Type@ is @forward@. If you
    -- specify both @ForwardConfig@ and @TargetGroupArn@, you can specify only
    -- one target group using @ForwardConfig@ and it must be the same target
    -- group specified in @TargetGroupArn@.
    forwardConfig :: Prelude.Maybe ForwardActionConfig,
    -- | The order for the action. This value is required for rules with multiple
    -- actions. The action with the lowest value for order is performed first.
    order :: Prelude.Maybe Prelude.Natural,
    -- | [Application Load Balancer] Information for creating a redirect action.
    -- Specify only when @Type@ is @redirect@.
    redirectConfig :: Prelude.Maybe RedirectActionConfig,
    -- | The Amazon Resource Name (ARN) of the target group. Specify only when
    -- @Type@ is @forward@ and you want to route to a single target group. To
    -- route to one or more target groups, use @ForwardConfig@ instead.
    targetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The type of action.
    type' :: ActionTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticateCognitoConfig', 'action_authenticateCognitoConfig' - [HTTPS listeners] Information for using Amazon Cognito to authenticate
-- users. Specify only when @Type@ is @authenticate-cognito@.
--
-- 'authenticateOidcConfig', 'action_authenticateOidcConfig' - [HTTPS listeners] Information about an identity provider that is
-- compliant with OpenID Connect (OIDC). Specify only when @Type@ is
-- @authenticate-oidc@.
--
-- 'fixedResponseConfig', 'action_fixedResponseConfig' - [Application Load Balancer] Information for creating an action that
-- returns a custom HTTP response. Specify only when @Type@ is
-- @fixed-response@.
--
-- 'forwardConfig', 'action_forwardConfig' - Information for creating an action that distributes requests among one
-- or more target groups. For Network Load Balancers, you can specify a
-- single target group. Specify only when @Type@ is @forward@. If you
-- specify both @ForwardConfig@ and @TargetGroupArn@, you can specify only
-- one target group using @ForwardConfig@ and it must be the same target
-- group specified in @TargetGroupArn@.
--
-- 'order', 'action_order' - The order for the action. This value is required for rules with multiple
-- actions. The action with the lowest value for order is performed first.
--
-- 'redirectConfig', 'action_redirectConfig' - [Application Load Balancer] Information for creating a redirect action.
-- Specify only when @Type@ is @redirect@.
--
-- 'targetGroupArn', 'action_targetGroupArn' - The Amazon Resource Name (ARN) of the target group. Specify only when
-- @Type@ is @forward@ and you want to route to a single target group. To
-- route to one or more target groups, use @ForwardConfig@ instead.
--
-- 'type'', 'action_type' - The type of action.
newAction ::
  -- | 'type''
  ActionTypeEnum ->
  Action
newAction pType_ =
  Action'
    { authenticateCognitoConfig =
        Prelude.Nothing,
      authenticateOidcConfig = Prelude.Nothing,
      fixedResponseConfig = Prelude.Nothing,
      forwardConfig = Prelude.Nothing,
      order = Prelude.Nothing,
      redirectConfig = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing,
      type' = pType_
    }

-- | [HTTPS listeners] Information for using Amazon Cognito to authenticate
-- users. Specify only when @Type@ is @authenticate-cognito@.
action_authenticateCognitoConfig :: Lens.Lens' Action (Prelude.Maybe AuthenticateCognitoActionConfig)
action_authenticateCognitoConfig = Lens.lens (\Action' {authenticateCognitoConfig} -> authenticateCognitoConfig) (\s@Action' {} a -> s {authenticateCognitoConfig = a} :: Action)

-- | [HTTPS listeners] Information about an identity provider that is
-- compliant with OpenID Connect (OIDC). Specify only when @Type@ is
-- @authenticate-oidc@.
action_authenticateOidcConfig :: Lens.Lens' Action (Prelude.Maybe AuthenticateOidcActionConfig)
action_authenticateOidcConfig = Lens.lens (\Action' {authenticateOidcConfig} -> authenticateOidcConfig) (\s@Action' {} a -> s {authenticateOidcConfig = a} :: Action)

-- | [Application Load Balancer] Information for creating an action that
-- returns a custom HTTP response. Specify only when @Type@ is
-- @fixed-response@.
action_fixedResponseConfig :: Lens.Lens' Action (Prelude.Maybe FixedResponseActionConfig)
action_fixedResponseConfig = Lens.lens (\Action' {fixedResponseConfig} -> fixedResponseConfig) (\s@Action' {} a -> s {fixedResponseConfig = a} :: Action)

-- | Information for creating an action that distributes requests among one
-- or more target groups. For Network Load Balancers, you can specify a
-- single target group. Specify only when @Type@ is @forward@. If you
-- specify both @ForwardConfig@ and @TargetGroupArn@, you can specify only
-- one target group using @ForwardConfig@ and it must be the same target
-- group specified in @TargetGroupArn@.
action_forwardConfig :: Lens.Lens' Action (Prelude.Maybe ForwardActionConfig)
action_forwardConfig = Lens.lens (\Action' {forwardConfig} -> forwardConfig) (\s@Action' {} a -> s {forwardConfig = a} :: Action)

-- | The order for the action. This value is required for rules with multiple
-- actions. The action with the lowest value for order is performed first.
action_order :: Lens.Lens' Action (Prelude.Maybe Prelude.Natural)
action_order = Lens.lens (\Action' {order} -> order) (\s@Action' {} a -> s {order = a} :: Action)

-- | [Application Load Balancer] Information for creating a redirect action.
-- Specify only when @Type@ is @redirect@.
action_redirectConfig :: Lens.Lens' Action (Prelude.Maybe RedirectActionConfig)
action_redirectConfig = Lens.lens (\Action' {redirectConfig} -> redirectConfig) (\s@Action' {} a -> s {redirectConfig = a} :: Action)

-- | The Amazon Resource Name (ARN) of the target group. Specify only when
-- @Type@ is @forward@ and you want to route to a single target group. To
-- route to one or more target groups, use @ForwardConfig@ instead.
action_targetGroupArn :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_targetGroupArn = Lens.lens (\Action' {targetGroupArn} -> targetGroupArn) (\s@Action' {} a -> s {targetGroupArn = a} :: Action)

-- | The type of action.
action_type :: Lens.Lens' Action ActionTypeEnum
action_type = Lens.lens (\Action' {type'} -> type') (\s@Action' {} a -> s {type' = a} :: Action)

instance Data.FromXML Action where
  parseXML x =
    Action'
      Prelude.<$> (x Data..@? "AuthenticateCognitoConfig")
      Prelude.<*> (x Data..@? "AuthenticateOidcConfig")
      Prelude.<*> (x Data..@? "FixedResponseConfig")
      Prelude.<*> (x Data..@? "ForwardConfig")
      Prelude.<*> (x Data..@? "Order")
      Prelude.<*> (x Data..@? "RedirectConfig")
      Prelude.<*> (x Data..@? "TargetGroupArn")
      Prelude.<*> (x Data..@ "Type")

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt
      `Prelude.hashWithSalt` authenticateCognitoConfig
      `Prelude.hashWithSalt` authenticateOidcConfig
      `Prelude.hashWithSalt` fixedResponseConfig
      `Prelude.hashWithSalt` forwardConfig
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` redirectConfig
      `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf authenticateCognitoConfig `Prelude.seq`
      Prelude.rnf authenticateOidcConfig `Prelude.seq`
        Prelude.rnf fixedResponseConfig `Prelude.seq`
          Prelude.rnf forwardConfig `Prelude.seq`
            Prelude.rnf order `Prelude.seq`
              Prelude.rnf redirectConfig `Prelude.seq`
                Prelude.rnf targetGroupArn `Prelude.seq`
                  Prelude.rnf type'

instance Data.ToQuery Action where
  toQuery Action' {..} =
    Prelude.mconcat
      [ "AuthenticateCognitoConfig"
          Data.=: authenticateCognitoConfig,
        "AuthenticateOidcConfig"
          Data.=: authenticateOidcConfig,
        "FixedResponseConfig" Data.=: fixedResponseConfig,
        "ForwardConfig" Data.=: forwardConfig,
        "Order" Data.=: order,
        "RedirectConfig" Data.=: redirectConfig,
        "TargetGroupArn" Data.=: targetGroupArn,
        "Type" Data.=: type'
      ]
