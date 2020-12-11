{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an RDS event notification subscription. This action requires a topic Amazon Resource Name (ARN) created by either the RDS console, the SNS console, or the SNS API. To obtain an ARN with SNS, you must create a topic in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS console.
--
-- You can specify the type of source (@SourceType@ ) that you want to be notified of and provide a list of RDS sources (@SourceIds@ ) that triggers the events. You can also provide a list of event categories (@EventCategories@ ) for events that you want to be notified of. For example, you can specify @SourceType@ = @db-instance@ , @SourceIds@ = @mydbinstance1@ , @mydbinstance2@ and @EventCategories@ = @Availability@ , @Backup@ .
-- If you specify both the @SourceType@ and @SourceIds@ , such as @SourceType@ = @db-instance@ and @SourceIdentifier@ = @myDBInstance1@ , you are notified of all the @db-instance@ events for the specified source. If you specify a @SourceType@ but do not specify a @SourceIdentifier@ , you receive notice of the events for that source type for all your RDS sources. If you don't specify either the SourceType or the @SourceIdentifier@ , you are notified of events generated from all RDS sources belonging to your customer account.
module Network.AWS.RDS.CreateEventSubscription
  ( -- * Creating a request
    CreateEventSubscription (..),
    mkCreateEventSubscription,

    -- ** Request lenses
    cesEnabled,
    cesSourceType,
    cesEventCategories,
    cesSourceIds,
    cesTags,
    cesSubscriptionName,
    cesSNSTopicARN,

    -- * Destructuring the response
    CreateEventSubscriptionResponse (..),
    mkCreateEventSubscriptionResponse,

    -- ** Response lenses
    cesrsEventSubscription,
    cesrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { enabled ::
      Lude.Maybe Lude.Bool,
    sourceType :: Lude.Maybe Lude.Text,
    eventCategories :: Lude.Maybe [Lude.Text],
    sourceIds :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    subscriptionName :: Lude.Text,
    snsTopicARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEventSubscription' with the minimum fields required to make a request.
--
-- * 'enabled' - A value that indicates whether to activate the subscription. If the event notification subscription isn't activated, the subscription is created but not active.
-- * 'eventCategories' - A list of event categories for a particular source type (@SourceType@ ) that you want to subscribe to. You can see a list of the categories for a given source type in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> in the /Amazon RDS User Guide/ or by using the @DescribeEventCategories@ operation.
-- * 'snsTopicARN' - The Amazon Resource Name (ARN) of the SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
-- * 'sourceIds' - The list of identifiers of the event sources for which events are returned. If not specified, then all sources are included in the response. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens. It can't end with a hyphen or contain two consecutive hyphens.
--
-- Constraints:
--
--     * If a @SourceIds@ value is supplied, @SourceType@ must also be provided.
--
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
-- * 'sourceType' - The type of source that is generating the events. For example, if you want to be notified of events generated by a DB instance, you set this parameter to @db-instance@ . If this value isn't specified, all events are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
-- * 'subscriptionName' - The name of the subscription.
--
-- Constraints: The name must be less than 255 characters.
-- * 'tags' - Undocumented field.
mkCreateEventSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  -- | 'snsTopicARN'
  Lude.Text ->
  CreateEventSubscription
mkCreateEventSubscription pSubscriptionName_ pSNSTopicARN_ =
  CreateEventSubscription'
    { enabled = Lude.Nothing,
      sourceType = Lude.Nothing,
      eventCategories = Lude.Nothing,
      sourceIds = Lude.Nothing,
      tags = Lude.Nothing,
      subscriptionName = pSubscriptionName_,
      snsTopicARN = pSNSTopicARN_
    }

-- | A value that indicates whether to activate the subscription. If the event notification subscription isn't activated, the subscription is created but not active.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesEnabled :: Lens.Lens' CreateEventSubscription (Lude.Maybe Lude.Bool)
cesEnabled = Lens.lens (enabled :: CreateEventSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CreateEventSubscription)
{-# DEPRECATED cesEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The type of source that is generating the events. For example, if you want to be notified of events generated by a DB instance, you set this parameter to @db-instance@ . If this value isn't specified, all events are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSourceType :: Lens.Lens' CreateEventSubscription (Lude.Maybe Lude.Text)
cesSourceType = Lens.lens (sourceType :: CreateEventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: CreateEventSubscription)
{-# DEPRECATED cesSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | A list of event categories for a particular source type (@SourceType@ ) that you want to subscribe to. You can see a list of the categories for a given source type in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> in the /Amazon RDS User Guide/ or by using the @DescribeEventCategories@ operation.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesEventCategories :: Lens.Lens' CreateEventSubscription (Lude.Maybe [Lude.Text])
cesEventCategories = Lens.lens (eventCategories :: CreateEventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: CreateEventSubscription)
{-# DEPRECATED cesEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The list of identifiers of the event sources for which events are returned. If not specified, then all sources are included in the response. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens. It can't end with a hyphen or contain two consecutive hyphens.
--
-- Constraints:
--
--     * If a @SourceIds@ value is supplied, @SourceType@ must also be provided.
--
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
-- /Note:/ Consider using 'sourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSourceIds :: Lens.Lens' CreateEventSubscription (Lude.Maybe [Lude.Text])
cesSourceIds = Lens.lens (sourceIds :: CreateEventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {sourceIds = a} :: CreateEventSubscription)
{-# DEPRECATED cesSourceIds "Use generic-lens or generic-optics with 'sourceIds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesTags :: Lens.Lens' CreateEventSubscription (Lude.Maybe [Tag])
cesTags = Lens.lens (tags :: CreateEventSubscription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEventSubscription)
{-# DEPRECATED cesTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the subscription.
--
-- Constraints: The name must be less than 255 characters.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSubscriptionName :: Lens.Lens' CreateEventSubscription Lude.Text
cesSubscriptionName = Lens.lens (subscriptionName :: CreateEventSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: CreateEventSubscription)
{-# DEPRECATED cesSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSNSTopicARN :: Lens.Lens' CreateEventSubscription Lude.Text
cesSNSTopicARN = Lens.lens (snsTopicARN :: CreateEventSubscription -> Lude.Text) (\s a -> s {snsTopicARN = a} :: CreateEventSubscription)
{-# DEPRECATED cesSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Lude.AWSRequest CreateEventSubscription where
  type Rs CreateEventSubscription = CreateEventSubscriptionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateEventSubscriptionResult"
      ( \s h x ->
          CreateEventSubscriptionResponse'
            Lude.<$> (x Lude..@? "EventSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEventSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateEventSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEventSubscription where
  toQuery CreateEventSubscription' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateEventSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Enabled" Lude.=: enabled,
        "SourceType" Lude.=: sourceType,
        "EventCategories"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "EventCategory" Lude.<$> eventCategories),
        "SourceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "SourceId" Lude.<$> sourceIds),
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "SubscriptionName" Lude.=: subscriptionName,
        "SnsTopicArn" Lude.=: snsTopicARN
      ]

-- | /See:/ 'mkCreateEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
  { eventSubscription ::
      Lude.Maybe
        EventSubscription,
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

-- | Creates a value of 'CreateEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscription' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateEventSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEventSubscriptionResponse
mkCreateEventSubscriptionResponse pResponseStatus_ =
  CreateEventSubscriptionResponse'
    { eventSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesrsEventSubscription :: Lens.Lens' CreateEventSubscriptionResponse (Lude.Maybe EventSubscription)
cesrsEventSubscription = Lens.lens (eventSubscription :: CreateEventSubscriptionResponse -> Lude.Maybe EventSubscription) (\s a -> s {eventSubscription = a} :: CreateEventSubscriptionResponse)
{-# DEPRECATED cesrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesrsResponseStatus :: Lens.Lens' CreateEventSubscriptionResponse Lude.Int
cesrsResponseStatus = Lens.lens (responseStatus :: CreateEventSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEventSubscriptionResponse)
{-# DEPRECATED cesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
