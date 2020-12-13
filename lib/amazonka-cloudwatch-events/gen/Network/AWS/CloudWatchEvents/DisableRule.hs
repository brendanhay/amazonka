{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DisableRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified rule. A disabled rule won't match any events, and won't self-trigger if it has a schedule expression.
--
-- When you disable a rule, incoming events might continue to match to the disabled rule. Allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.DisableRule
  ( -- * Creating a request
    DisableRule (..),
    mkDisableRule,

    -- ** Request lenses
    drfEventBusName,
    drfName,

    -- * Destructuring the response
    DisableRuleResponse (..),
    mkDisableRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableRule' smart constructor.
data DisableRule = DisableRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The name of the rule.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableRule' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'name' - The name of the rule.
mkDisableRule ::
  -- | 'name'
  Lude.Text ->
  DisableRule
mkDisableRule pName_ =
  DisableRule' {eventBusName = Lude.Nothing, name = pName_}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfEventBusName :: Lens.Lens' DisableRule (Lude.Maybe Lude.Text)
drfEventBusName = Lens.lens (eventBusName :: DisableRule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: DisableRule)
{-# DEPRECATED drfEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfName :: Lens.Lens' DisableRule Lude.Text
drfName = Lens.lens (name :: DisableRule -> Lude.Text) (\s a -> s {name = a} :: DisableRule)
{-# DEPRECATED drfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DisableRule where
  type Rs DisableRule = DisableRuleResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull DisableRuleResponse'

instance Lude.ToHeaders DisableRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DisableRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableRule where
  toJSON DisableRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DisableRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableRuleResponse' smart constructor.
data DisableRuleResponse = DisableRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableRuleResponse' with the minimum fields required to make a request.
mkDisableRuleResponse ::
  DisableRuleResponse
mkDisableRuleResponse = DisableRuleResponse'
