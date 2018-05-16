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
-- Adds the specified targets to the specified rule, or updates the targets if they are already associated with the rule.
--
--
-- Targets are the resources that are invoked when a rule is triggered.
--
-- You can configure the following as targets for CloudWatch Events:
--
--     * EC2 instances
--
--     * AWS Lambda functions
--
--     * Streams in Amazon Kinesis Streams
--
--     * Delivery streams in Amazon Kinesis Firehose
--
--     * Amazon ECS tasks
--
--     * AWS Step Functions state machines
--
--     * AWS Batch jobs
--
--     * Pipelines in Amazon Code Pipeline
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
-- Note that creating rules with built-in targets is supported only in the AWS Management Console.
--
-- For some target types, @PutTargets@ provides target-specific parameters. If the target is an Amazon Kinesis stream, you can optionally specify which shard the event goes to by using the @KinesisParameters@ argument. To invoke a command on multiple EC2 instances with one rule, you can use the @RunCommandParameters@ field.
--
-- To be able to make API calls against the resources that you own, Amazon CloudWatch Events needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, CloudWatch Events relies on resource-based policies. For EC2 instances, Amazon Kinesis streams, and AWS Step Functions state machines, CloudWatch Events relies on IAM roles that you specify in the @RoleARN@ argument in @PutTargets@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/auth-and-access-control-cwe.html Authentication and Access Control> in the /Amazon CloudWatch Events User Guide/ .
--
-- If another AWS account is in the same region and has granted you permission (using @PutPermission@ ), you can send events to that account by setting that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the @Arn@ when you run @PutTargets@ . If your account sends events to another account, your account is charged for each sent event. Each event sent to antoher account is charged as a custom event. The account receiving the event is not charged. For more information on pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
--
-- For more information about enabling cross-account events, see 'PutPermission' .
--
-- __Input__ , __InputPath__ and __InputTransformer__ are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:
--
--     * If none of the following arguments are specified for a target, then the entire event is passed to the target in JSON form (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).
--
--     * If __Input__ is specified in the form of valid JSON, then the matched event is overridden with this constant.
--
--     * If __InputPath__ is specified in the form of JSONPath (for example, @> .detail@ ), then only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).
--
--     * If __InputTransformer__ is specified, then one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.
--
--
--
-- When you specify @InputPath@ or @InputTransformer@ , you must use JSON dot notation, not bracket notation.
--
-- When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Please allow a short period of time for changes to take effect.
--
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
--
module Network.AWS.CloudWatchEvents.PutTargets
    (
    -- * Creating a Request
      putTargets
    , PutTargets
    -- * Request Lenses
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
  { _ptRule    :: !Text
  , _ptTargets :: !(List1 Target)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptRule' - The name of the rule.
--
-- * 'ptTargets' - The targets to update or add to the rule.
putTargets
    :: Text -- ^ 'ptRule'
    -> NonEmpty Target -- ^ 'ptTargets'
    -> PutTargets
putTargets pRule_ pTargets_ =
  PutTargets' {_ptRule = pRule_, _ptTargets = _List1 # pTargets_}


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
                 [Just ("Rule" .= _ptRule),
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
