{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- Before you can delete the rule, you must remove all targets, using 'RemoveTargets' .
-- When you delete a rule, incoming events might continue to match to the deleted rule. Allow a short period of time for changes to take effect.
-- Managed rules are rules created and managed by another AWS service on your behalf. These rules are created by those other AWS services to support functionality in those services. You can delete these rules using the @Force@ option, but you should do so only if you are sure the other service is not still using that rule.
module Network.AWS.CloudWatchEvents.DeleteRule
  ( -- * Creating a request
    DeleteRule (..),
    mkDeleteRule,

    -- ** Request lenses
    drgForce,
    drgEventBusName,
    drgName,

    -- * Destructuring the response
    DeleteRuleResponse (..),
    mkDeleteRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
    force :: Lude.Maybe Lude.Bool,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The name of the rule.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- * 'force' - If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'name' - The name of the rule.
mkDeleteRule ::
  -- | 'name'
  Lude.Text ->
  DeleteRule
mkDeleteRule pName_ =
  DeleteRule'
    { force = Lude.Nothing,
      eventBusName = Lude.Nothing,
      name = pName_
    }

-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgForce :: Lens.Lens' DeleteRule (Lude.Maybe Lude.Bool)
drgForce = Lens.lens (force :: DeleteRule -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteRule)
{-# DEPRECATED drgForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgEventBusName :: Lens.Lens' DeleteRule (Lude.Maybe Lude.Text)
drgEventBusName = Lens.lens (eventBusName :: DeleteRule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: DeleteRule)
{-# DEPRECATED drgEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgName :: Lens.Lens' DeleteRule Lude.Text
drgName = Lens.lens (name :: DeleteRule -> Lude.Text) (\s a -> s {name = a} :: DeleteRule)
{-# DEPRECATED drgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteRule where
  type Rs DeleteRule = DeleteRuleResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull DeleteRuleResponse'

instance Lude.ToHeaders DeleteRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DeleteRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Force" Lude..=) Lude.<$> force,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DeleteRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
mkDeleteRuleResponse ::
  DeleteRuleResponse
mkDeleteRuleResponse = DeleteRuleResponse'
