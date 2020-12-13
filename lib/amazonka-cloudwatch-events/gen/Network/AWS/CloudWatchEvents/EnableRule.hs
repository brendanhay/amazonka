{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.EnableRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified rule. If the rule does not exist, the operation fails.
--
-- When you enable a rule, incoming events might not immediately start matching to a newly enabled rule. Allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.EnableRule
  ( -- * Creating a request
    EnableRule (..),
    mkEnableRule,

    -- ** Request lenses
    erEventBusName,
    erName,

    -- * Destructuring the response
    EnableRuleResponse (..),
    mkEnableRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableRule' smart constructor.
data EnableRule = EnableRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The name of the rule.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableRule' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'name' - The name of the rule.
mkEnableRule ::
  -- | 'name'
  Lude.Text ->
  EnableRule
mkEnableRule pName_ =
  EnableRule' {eventBusName = Lude.Nothing, name = pName_}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEventBusName :: Lens.Lens' EnableRule (Lude.Maybe Lude.Text)
erEventBusName = Lens.lens (eventBusName :: EnableRule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: EnableRule)
{-# DEPRECATED erEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erName :: Lens.Lens' EnableRule Lude.Text
erName = Lens.lens (name :: EnableRule -> Lude.Text) (\s a -> s {name = a} :: EnableRule)
{-# DEPRECATED erName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest EnableRule where
  type Rs EnableRule = EnableRuleResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull EnableRuleResponse'

instance Lude.ToHeaders EnableRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.EnableRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableRule where
  toJSON EnableRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath EnableRule where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableRuleResponse' smart constructor.
data EnableRuleResponse = EnableRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableRuleResponse' with the minimum fields required to make a request.
mkEnableRuleResponse ::
  EnableRuleResponse
mkEnableRuleResponse = EnableRuleResponse'
