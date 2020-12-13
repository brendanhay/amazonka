{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified targets to the specified rule, or updates the targets if they are already associated with the rule.
--
-- Targets are the resources that are invoked when a rule is triggered.
-- You can configure the following as targets for Events:
--
--     * EC2 instances
--
--
--     * SSM Run Command
--
--
--     * SSM Automation
--
--
--     * AWS Lambda functions
--
--
--     * Data streams in Amazon Kinesis Data Streams
--
--
--     * Data delivery streams in Amazon Kinesis Data Firehose
--
--
--     * Amazon ECS tasks
--
--
--     * AWS Step Functions state machines
--
--
--     * AWS Batch jobs
--
--
--     * AWS CodeBuild projects
--
--
--     * Pipelines in AWS CodePipeline
--
--
--     * Amazon Inspector assessment templates
--
--
--     * Amazon SNS topics
--
--
--     * Amazon SQS queues, including FIFO queues
--
--
--     * The default event bus of another AWS account
--
--
--     * Amazon API Gateway REST APIs
--
--
--     * Redshift Clusters to invoke Data API ExecuteStatement on
--
--
-- Creating rules with built-in targets is supported only in the AWS Management Console. The built-in targets are @EC2 CreateSnapshot API call@ , @EC2 RebootInstances API call@ , @EC2 StopInstances API call@ , and @EC2 TerminateInstances API call@ .
-- For some target types, @PutTargets@ provides target-specific parameters. If the target is a Kinesis data stream, you can optionally specify which shard the event goes to by using the @KinesisParameters@ argument. To invoke a command on multiple EC2 instances with one rule, you can use the @RunCommandParameters@ field.
-- To be able to make API calls against the resources that you own, Amazon EventBridge (CloudWatch Events) needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, EventBridge relies on resource-based policies. For EC2 instances, Kinesis data streams, AWS Step Functions state machines and API Gateway REST APIs, EventBridge relies on IAM roles that you specify in the @RoleARN@ argument in @PutTargets@ . For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/auth-and-access-control-eventbridge.html Authentication and Access Control> in the /Amazon EventBridge User Guide/ .
-- If another AWS account is in the same region and has granted you permission (using @PutPermission@ ), you can send events to that account. Set that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the @Arn@ value when you run @PutTargets@ . If your account sends events to another account, your account is charged for each sent event. Each event sent to another account is charged as a custom event. The account receiving the event is not charged. For more information, see <https://aws.amazon.com/eventbridge/pricing/ Amazon EventBridge (CloudWatch Events) Pricing> .
-- If you are setting the event bus of another account as the target, and that account granted permission to your account through an organization instead of directly by the account ID, then you must specify a @RoleArn@ with proper permissions in the @Target@ structure. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
-- For more information about enabling cross-account events, see 'PutPermission' .
-- __Input__ , __InputPath__ , and __InputTransformer__ are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:
--
--     * If none of the following arguments are specified for a target, then the entire event is passed to the target in JSON format (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).
--
--
--     * If __Input__ is specified in the form of valid JSON, then the matched event is overridden with this constant.
--
--
--     * If __InputPath__ is specified in the form of JSONPath (for example, @> .detail@ ), then only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).
--
--
--     * If __InputTransformer__ is specified, then one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.
--
--
-- When you specify @InputPath@ or @InputTransformer@ , you must use JSON dot notation, not bracket notation.
-- When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Allow a short period of time for changes to take effect.
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
module Network.AWS.CloudWatchEvents.PutTargets
  ( -- * Creating a request
    PutTargets (..),
    mkPutTargets,

    -- ** Request lenses
    ptRule,
    ptEventBusName,
    ptTargets,

    -- * Destructuring the response
    PutTargetsResponse (..),
    mkPutTargetsResponse,

    -- ** Response lenses
    ptrsFailedEntryCount,
    ptrsFailedEntries,
    ptrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutTargets' smart constructor.
data PutTargets = PutTargets'
  { -- | The name of the rule.
    rule :: Lude.Text,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The targets to update or add to the rule.
    targets :: Lude.NonEmpty Target
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTargets' with the minimum fields required to make a request.
--
-- * 'rule' - The name of the rule.
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'targets' - The targets to update or add to the rule.
mkPutTargets ::
  -- | 'rule'
  Lude.Text ->
  -- | 'targets'
  Lude.NonEmpty Target ->
  PutTargets
mkPutTargets pRule_ pTargets_ =
  PutTargets'
    { rule = pRule_,
      eventBusName = Lude.Nothing,
      targets = pTargets_
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptRule :: Lens.Lens' PutTargets Lude.Text
ptRule = Lens.lens (rule :: PutTargets -> Lude.Text) (\s a -> s {rule = a} :: PutTargets)
{-# DEPRECATED ptRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptEventBusName :: Lens.Lens' PutTargets (Lude.Maybe Lude.Text)
ptEventBusName = Lens.lens (eventBusName :: PutTargets -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: PutTargets)
{-# DEPRECATED ptEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The targets to update or add to the rule.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptTargets :: Lens.Lens' PutTargets (Lude.NonEmpty Target)
ptTargets = Lens.lens (targets :: PutTargets -> Lude.NonEmpty Target) (\s a -> s {targets = a} :: PutTargets)
{-# DEPRECATED ptTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Lude.AWSRequest PutTargets where
  type Rs PutTargets = PutTargetsResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutTargetsResponse'
            Lude.<$> (x Lude..?> "FailedEntryCount")
            Lude.<*> (x Lude..?> "FailedEntries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.PutTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutTargets where
  toJSON PutTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Rule" Lude..= rule),
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Targets" Lude..= targets)
          ]
      )

instance Lude.ToPath PutTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery PutTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutTargetsResponse' smart constructor.
data PutTargetsResponse = PutTargetsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Lude.Maybe Lude.Int,
    -- | The failed target entries.
    failedEntries :: Lude.Maybe [PutTargetsResultEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTargetsResponse' with the minimum fields required to make a request.
--
-- * 'failedEntryCount' - The number of failed entries.
-- * 'failedEntries' - The failed target entries.
-- * 'responseStatus' - The response status code.
mkPutTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutTargetsResponse
mkPutTargetsResponse pResponseStatus_ =
  PutTargetsResponse'
    { failedEntryCount = Lude.Nothing,
      failedEntries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsFailedEntryCount :: Lens.Lens' PutTargetsResponse (Lude.Maybe Lude.Int)
ptrsFailedEntryCount = Lens.lens (failedEntryCount :: PutTargetsResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedEntryCount = a} :: PutTargetsResponse)
{-# DEPRECATED ptrsFailedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead." #-}

-- | The failed target entries.
--
-- /Note:/ Consider using 'failedEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsFailedEntries :: Lens.Lens' PutTargetsResponse (Lude.Maybe [PutTargetsResultEntry])
ptrsFailedEntries = Lens.lens (failedEntries :: PutTargetsResponse -> Lude.Maybe [PutTargetsResultEntry]) (\s a -> s {failedEntries = a} :: PutTargetsResponse)
{-# DEPRECATED ptrsFailedEntries "Use generic-lens or generic-optics with 'failedEntries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsResponseStatus :: Lens.Lens' PutTargetsResponse Lude.Int
ptrsResponseStatus = Lens.lens (responseStatus :: PutTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutTargetsResponse)
{-# DEPRECATED ptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
