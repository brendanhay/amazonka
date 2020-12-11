{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.
--
-- When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Allow a short period of time for changes to take effect.
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
module Network.AWS.CloudWatchEvents.RemoveTargets
  ( -- * Creating a request
    RemoveTargets (..),
    mkRemoveTargets,

    -- ** Request lenses
    rtForce,
    rtEventBusName,
    rtRule,
    rtIds,

    -- * Destructuring the response
    RemoveTargetsResponse (..),
    mkRemoveTargetsResponse,

    -- ** Response lenses
    rtrsFailedEntryCount,
    rtrsFailedEntries,
    rtrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { force :: Lude.Maybe Lude.Bool,
    eventBusName :: Lude.Maybe Lude.Text,
    rule :: Lude.Text,
    ids :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTargets' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'force' - If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
-- * 'ids' - The IDs of the targets to remove from the rule.
-- * 'rule' - The name of the rule.
mkRemoveTargets ::
  -- | 'rule'
  Lude.Text ->
  -- | 'ids'
  Lude.NonEmpty Lude.Text ->
  RemoveTargets
mkRemoveTargets pRule_ pIds_ =
  RemoveTargets'
    { force = Lude.Nothing,
      eventBusName = Lude.Nothing,
      rule = pRule_,
      ids = pIds_
    }

-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtForce :: Lens.Lens' RemoveTargets (Lude.Maybe Lude.Bool)
rtForce = Lens.lens (force :: RemoveTargets -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: RemoveTargets)
{-# DEPRECATED rtForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEventBusName :: Lens.Lens' RemoveTargets (Lude.Maybe Lude.Text)
rtEventBusName = Lens.lens (eventBusName :: RemoveTargets -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: RemoveTargets)
{-# DEPRECATED rtEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRule :: Lens.Lens' RemoveTargets Lude.Text
rtRule = Lens.lens (rule :: RemoveTargets -> Lude.Text) (\s a -> s {rule = a} :: RemoveTargets)
{-# DEPRECATED rtRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The IDs of the targets to remove from the rule.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtIds :: Lens.Lens' RemoveTargets (Lude.NonEmpty Lude.Text)
rtIds = Lens.lens (ids :: RemoveTargets -> Lude.NonEmpty Lude.Text) (\s a -> s {ids = a} :: RemoveTargets)
{-# DEPRECATED rtIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Lude.AWSRequest RemoveTargets where
  type Rs RemoveTargets = RemoveTargetsResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveTargetsResponse'
            Lude.<$> (x Lude..?> "FailedEntryCount")
            Lude.<*> (x Lude..?> "FailedEntries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.RemoveTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTargets where
  toJSON RemoveTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Force" Lude..=) Lude.<$> force,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Rule" Lude..= rule),
            Lude.Just ("Ids" Lude..= ids)
          ]
      )

instance Lude.ToPath RemoveTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { failedEntryCount ::
      Lude.Maybe Lude.Int,
    failedEntries ::
      Lude.Maybe [RemoveTargetsResultEntry],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTargetsResponse' with the minimum fields required to make a request.
--
-- * 'failedEntries' - The failed target entries.
-- * 'failedEntryCount' - The number of failed entries.
-- * 'responseStatus' - The response status code.
mkRemoveTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTargetsResponse
mkRemoveTargetsResponse pResponseStatus_ =
  RemoveTargetsResponse'
    { failedEntryCount = Lude.Nothing,
      failedEntries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsFailedEntryCount :: Lens.Lens' RemoveTargetsResponse (Lude.Maybe Lude.Int)
rtrsFailedEntryCount = Lens.lens (failedEntryCount :: RemoveTargetsResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedEntryCount = a} :: RemoveTargetsResponse)
{-# DEPRECATED rtrsFailedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead." #-}

-- | The failed target entries.
--
-- /Note:/ Consider using 'failedEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsFailedEntries :: Lens.Lens' RemoveTargetsResponse (Lude.Maybe [RemoveTargetsResultEntry])
rtrsFailedEntries = Lens.lens (failedEntries :: RemoveTargetsResponse -> Lude.Maybe [RemoveTargetsResultEntry]) (\s a -> s {failedEntries = a} :: RemoveTargetsResponse)
{-# DEPRECATED rtrsFailedEntries "Use generic-lens or generic-optics with 'failedEntries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RemoveTargetsResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RemoveTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTargetsResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
