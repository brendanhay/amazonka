{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a source identifier to an existing RDS event notification subscription.
module Network.AWS.RDS.AddSourceIdentifierToSubscription
  ( -- * Creating a request
    AddSourceIdentifierToSubscription (..),
    mkAddSourceIdentifierToSubscription,

    -- ** Request lenses
    asitsSubscriptionName,
    asitsSourceIdentifier,

    -- * Destructuring the response
    AddSourceIdentifierToSubscriptionResponse (..),
    mkAddSourceIdentifierToSubscriptionResponse,

    -- ** Response lenses
    asitsrsEventSubscription,
    asitsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAddSourceIdentifierToSubscription' smart constructor.
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
  { -- | The name of the RDS event notification subscription you want to add a source identifier to.
    subscriptionName :: Lude.Text,
    -- | The identifier of the event source to be added.
    --
    -- Constraints:
    --
    --     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
    --
    --
    --     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
    --
    --
    --     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
    --
    --
    --     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
    --
    --
    --     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
    --
    --
    --     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
    sourceIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddSourceIdentifierToSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name of the RDS event notification subscription you want to add a source identifier to.
-- * 'sourceIdentifier' - The identifier of the event source to be added.
--
-- Constraints:
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
mkAddSourceIdentifierToSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  -- | 'sourceIdentifier'
  Lude.Text ->
  AddSourceIdentifierToSubscription
mkAddSourceIdentifierToSubscription
  pSubscriptionName_
  pSourceIdentifier_ =
    AddSourceIdentifierToSubscription'
      { subscriptionName =
          pSubscriptionName_,
        sourceIdentifier = pSourceIdentifier_
      }

-- | The name of the RDS event notification subscription you want to add a source identifier to.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsSubscriptionName :: Lens.Lens' AddSourceIdentifierToSubscription Lude.Text
asitsSubscriptionName = Lens.lens (subscriptionName :: AddSourceIdentifierToSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: AddSourceIdentifierToSubscription)
{-# DEPRECATED asitsSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The identifier of the event source to be added.
--
-- Constraints:
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
--
--
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsSourceIdentifier :: Lens.Lens' AddSourceIdentifierToSubscription Lude.Text
asitsSourceIdentifier = Lens.lens (sourceIdentifier :: AddSourceIdentifierToSubscription -> Lude.Text) (\s a -> s {sourceIdentifier = a} :: AddSourceIdentifierToSubscription)
{-# DEPRECATED asitsSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Lude.AWSRequest AddSourceIdentifierToSubscription where
  type
    Rs AddSourceIdentifierToSubscription =
      AddSourceIdentifierToSubscriptionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "AddSourceIdentifierToSubscriptionResult"
      ( \s h x ->
          AddSourceIdentifierToSubscriptionResponse'
            Lude.<$> (x Lude..@? "EventSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddSourceIdentifierToSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddSourceIdentifierToSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery AddSourceIdentifierToSubscription where
  toQuery AddSourceIdentifierToSubscription' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AddSourceIdentifierToSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName,
        "SourceIdentifier" Lude.=: sourceIdentifier
      ]

-- | /See:/ 'mkAddSourceIdentifierToSubscriptionResponse' smart constructor.
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
  { eventSubscription :: Lude.Maybe EventSubscription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddSourceIdentifierToSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscription' -
-- * 'responseStatus' - The response status code.
mkAddSourceIdentifierToSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddSourceIdentifierToSubscriptionResponse
mkAddSourceIdentifierToSubscriptionResponse pResponseStatus_ =
  AddSourceIdentifierToSubscriptionResponse'
    { eventSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrsEventSubscription :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse (Lude.Maybe EventSubscription)
asitsrsEventSubscription = Lens.lens (eventSubscription :: AddSourceIdentifierToSubscriptionResponse -> Lude.Maybe EventSubscription) (\s a -> s {eventSubscription = a} :: AddSourceIdentifierToSubscriptionResponse)
{-# DEPRECATED asitsrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrsResponseStatus :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse Lude.Int
asitsrsResponseStatus = Lens.lens (responseStatus :: AddSourceIdentifierToSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddSourceIdentifierToSubscriptionResponse)
{-# DEPRECATED asitsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
