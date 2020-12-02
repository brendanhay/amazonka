{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified targets to the specified rule, or updates the targets if they're already associated with the rule.
--
--
-- Targets are the resources that are invoked when a rule is triggered.
--
-- You can configure the following as targets in EventBridge:
--
--     * EC2 instances
--
--     * SSM Run Command
--
--     * SSM Automation
--
--     * AWS Lambda functions
--
--     * Data streams in Amazon Kinesis Data Streams
--
--     * Data delivery streams in Amazon Kinesis Data Firehose
--
--     * Amazon ECS tasks
--
--     * AWS Step Functions state machines
--
--     * AWS Batch jobs
--
--     * AWS CodeBuild projects
--
--     * Pipelines in AWS CodePipeline
--
--     * Amazon Inspector assessment templates
--
--     * Amazon SNS topics
--
--     * Amazon SQS queues, including FIFO queues
--
--     * The default event bus of another AWS account
--
--
--
-- Creating rules with built-in targets is supported only on the AWS Management Console. The built-in targets are @EC2 CreateSnapshot API call@ , @EC2 RebootInstances API call@ , @EC2 StopInstances API call@ , and @EC2 TerminateInstances API call@ .
--
-- For some target types, @PutTargets@ provides target-specific parameters. If the target is a Kinesis data stream, you can optionally specify which shard the event goes to by using the @KinesisParameters@ argument. To invoke a command on multiple EC2 instances with one rule, you can use the @RunCommandParameters@ field.
--
-- To be able to make API calls against the resources that you own, Amazon EventBridge needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, EventBridge relies on resource-based policies. For EC2 instances, Kinesis data streams, and AWS Step Functions state machines, EventBridge relies on IAM roles that you specify in the @RoleARN@ argument in @PutTargets@ . For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/auth-and-access-control-eventbridge.html Authentication and Access Control> in the /Amazon EventBridge User Guide/ .
--
-- If another AWS account is in the same Region and has granted you permission (using @PutPermission@ ), you can send events to that account. Set that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the @Arn@ value when you run @PutTargets@ . If your account sends events to another account, your account is charged for each sent event. Each event sent to another account is charged as a custom event. The account receiving the event isn't charged. For more information, see <https://aws.amazon.com/eventbridge/pricing/ Amazon EventBridge Pricing> .
--
-- If you're setting an event bus in another account as the target and that account granted permission to your account through an organization instead of directly by the account ID, you must specify a @RoleArn@ with proper permissions in the @Target@ structure. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
--
-- For more information about enabling cross-account events, see 'PutPermission' .
--
-- @Input@ , @InputPath@ , and @InputTransformer@ are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:
--
--     * If none of the following arguments are specified for a target, the entire event is passed to the target in JSON format (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).
--
--     * If @Input@ is specified in the form of valid JSON, then the matched event is overridden with this constant.
--
--     * If @InputPath@ is specified in the form of JSONPath (for example, @> .detail@ ), only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).
--
--     * If @InputTransformer@ is specified, one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.
--
--
--
-- When you specify @InputPath@ or @InputTransformer@ , you must use JSON dot notation, not bracket notation.
--
-- When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Allow a short period of time for changes to take effect.
--
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is nonzero in the response, and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
--
module Network.AWS.CloudWatchEvents.PutTargets
    (
    -- * Creating a Request
      putTargets
    , PutTargets
    -- * Request Lenses
    , ptEventBusName
    , ptRule
    , ptTargets

    -- * Destructuring the Response
    , putTargetsResponse
    , PutTargetsResponse
    -- * Response Lenses
    , ptrsFailedEntryCount
    , ptrsFailedEntries
    , ptrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putTargets' smart constructor.
data PutTargets = PutTargets'
  { _ptEventBusName :: !(Maybe Text)
  , _ptRule         :: !Text
  , _ptTargets      :: !(List1 Target)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptEventBusName' - The name of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'ptRule' - The name of the rule.
--
-- * 'ptTargets' - The targets to update or add to the rule.
putTargets
    :: Text -- ^ 'ptRule'
    -> NonEmpty Target -- ^ 'ptTargets'
    -> PutTargets
putTargets pRule_ pTargets_ =
  PutTargets'
    { _ptEventBusName = Nothing
    , _ptRule = pRule_
    , _ptTargets = _List1 # pTargets_
    }


-- | The name of the event bus associated with the rule. If you omit this, the default event bus is used.
ptEventBusName :: Lens' PutTargets (Maybe Text)
ptEventBusName = lens _ptEventBusName (\ s a -> s{_ptEventBusName = a})

-- | The name of the rule.
ptRule :: Lens' PutTargets Text
ptRule = lens _ptRule (\ s a -> s{_ptRule = a})

-- | The targets to update or add to the rule.
ptTargets :: Lens' PutTargets (NonEmpty Target)
ptTargets = lens _ptTargets (\ s a -> s{_ptTargets = a}) . _List1

instance AWSRequest PutTargets where
        type Rs PutTargets = PutTargetsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutTargetsResponse' <$>
                   (x .?> "FailedEntryCount") <*>
                     (x .?> "FailedEntries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PutTargets where

instance NFData PutTargets where

instance ToHeaders PutTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutTargets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutTargets where
        toJSON PutTargets'{..}
          = object
              (catMaybes
                 [("EventBusName" .=) <$> _ptEventBusName,
                  Just ("Rule" .= _ptRule),
                  Just ("Targets" .= _ptTargets)])

instance ToPath PutTargets where
        toPath = const "/"

instance ToQuery PutTargets where
        toQuery = const mempty

-- | /See:/ 'putTargetsResponse' smart constructor.
data PutTargetsResponse = PutTargetsResponse'
  { _ptrsFailedEntryCount :: !(Maybe Int)
  , _ptrsFailedEntries    :: !(Maybe [PutTargetsResultEntry])
  , _ptrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrsFailedEntryCount' - The number of failed entries.
--
-- * 'ptrsFailedEntries' - The failed target entries.
--
-- * 'ptrsResponseStatus' - -- | The response status code.
putTargetsResponse
    :: Int -- ^ 'ptrsResponseStatus'
    -> PutTargetsResponse
putTargetsResponse pResponseStatus_ =
  PutTargetsResponse'
    { _ptrsFailedEntryCount = Nothing
    , _ptrsFailedEntries = Nothing
    , _ptrsResponseStatus = pResponseStatus_
    }


-- | The number of failed entries.
ptrsFailedEntryCount :: Lens' PutTargetsResponse (Maybe Int)
ptrsFailedEntryCount = lens _ptrsFailedEntryCount (\ s a -> s{_ptrsFailedEntryCount = a})

-- | The failed target entries.
ptrsFailedEntries :: Lens' PutTargetsResponse [PutTargetsResultEntry]
ptrsFailedEntries = lens _ptrsFailedEntries (\ s a -> s{_ptrsFailedEntries = a}) . _Default . _Coerce

-- | -- | The response status code.
ptrsResponseStatus :: Lens' PutTargetsResponse Int
ptrsResponseStatus = lens _ptrsResponseStatus (\ s a -> s{_ptrsResponseStatus = a})

instance NFData PutTargetsResponse where
