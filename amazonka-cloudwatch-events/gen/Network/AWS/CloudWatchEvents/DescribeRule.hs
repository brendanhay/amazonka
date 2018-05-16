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
-- Module      : Network.AWS.CloudWatchEvents.DescribeRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rule.
--
--
module Network.AWS.CloudWatchEvents.DescribeRule
    (
    -- * Creating a Request
      describeRule
    , DescribeRule
    -- * Request Lenses
    , desName

    -- * Destructuring the Response
    , describeRuleResponse
    , DescribeRuleResponse
    -- * Response Lenses
    , drrsEventPattern
    , drrsState
    , drrsARN
    , drrsScheduleExpression
    , drrsName
    , drrsDescription
    , drrsRoleARN
    , drrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRule' smart constructor.
newtype DescribeRule = DescribeRule'
  { _desName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desName' - The name of the rule.
describeRule
    :: Text -- ^ 'desName'
    -> DescribeRule
describeRule pName_ = DescribeRule' {_desName = pName_}


-- | The name of the rule.
desName :: Lens' DescribeRule Text
desName = lens _desName (\ s a -> s{_desName = a})

instance AWSRequest DescribeRule where
        type Rs DescribeRule = DescribeRuleResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRuleResponse' <$>
                   (x .?> "EventPattern") <*> (x .?> "State") <*>
                     (x .?> "Arn")
                     <*> (x .?> "ScheduleExpression")
                     <*> (x .?> "Name")
                     <*> (x .?> "Description")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRule where

instance NFData DescribeRule where

instance ToHeaders DescribeRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DescribeRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRule where
        toJSON DescribeRule'{..}
          = object (catMaybes [Just ("Name" .= _desName)])

instance ToPath DescribeRule where
        toPath = const "/"

instance ToQuery DescribeRule where
        toQuery = const mempty

-- | /See:/ 'describeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { _drrsEventPattern       :: !(Maybe Text)
  , _drrsState              :: !(Maybe RuleState)
  , _drrsARN                :: !(Maybe Text)
  , _drrsScheduleExpression :: !(Maybe Text)
  , _drrsName               :: !(Maybe Text)
  , _drrsDescription        :: !(Maybe Text)
  , _drrsRoleARN            :: !(Maybe Text)
  , _drrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsEventPattern' - The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
--
-- * 'drrsState' - Specifies whether the rule is enabled or disabled.
--
-- * 'drrsARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'drrsScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- * 'drrsName' - The name of the rule.
--
-- * 'drrsDescription' - The description of the rule.
--
-- * 'drrsRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRuleResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRuleResponse
describeRuleResponse pResponseStatus_ =
  DescribeRuleResponse'
    { _drrsEventPattern = Nothing
    , _drrsState = Nothing
    , _drrsARN = Nothing
    , _drrsScheduleExpression = Nothing
    , _drrsName = Nothing
    , _drrsDescription = Nothing
    , _drrsRoleARN = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }


-- | The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
drrsEventPattern :: Lens' DescribeRuleResponse (Maybe Text)
drrsEventPattern = lens _drrsEventPattern (\ s a -> s{_drrsEventPattern = a})

-- | Specifies whether the rule is enabled or disabled.
drrsState :: Lens' DescribeRuleResponse (Maybe RuleState)
drrsState = lens _drrsState (\ s a -> s{_drrsState = a})

-- | The Amazon Resource Name (ARN) of the rule.
drrsARN :: Lens' DescribeRuleResponse (Maybe Text)
drrsARN = lens _drrsARN (\ s a -> s{_drrsARN = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
drrsScheduleExpression :: Lens' DescribeRuleResponse (Maybe Text)
drrsScheduleExpression = lens _drrsScheduleExpression (\ s a -> s{_drrsScheduleExpression = a})

-- | The name of the rule.
drrsName :: Lens' DescribeRuleResponse (Maybe Text)
drrsName = lens _drrsName (\ s a -> s{_drrsName = a})

-- | The description of the rule.
drrsDescription :: Lens' DescribeRuleResponse (Maybe Text)
drrsDescription = lens _drrsDescription (\ s a -> s{_drrsDescription = a})

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
drrsRoleARN :: Lens' DescribeRuleResponse (Maybe Text)
drrsRoleARN = lens _drrsRoleARN (\ s a -> s{_drrsRoleARN = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRuleResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRuleResponse where
