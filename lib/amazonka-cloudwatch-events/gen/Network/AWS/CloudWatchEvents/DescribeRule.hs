{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rule.
--
--
-- DescribeRule does not list the targets of a rule. To see the targets associated with a rule, use 'ListTargetsByRule' .
module Network.AWS.CloudWatchEvents.DescribeRule
  ( -- * Creating a Request
    describeRule,
    DescribeRule,

    -- * Request Lenses
    desEventBusName,
    desName,

    -- * Destructuring the Response
    describeRuleResponse,
    DescribeRuleResponse,

    -- * Response Lenses
    drrsEventPattern,
    drrsState,
    drrsARN,
    drrsCreatedBy,
    drrsEventBusName,
    drrsScheduleExpression,
    drrsName,
    drrsDescription,
    drrsManagedBy,
    drrsRoleARN,
    drrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRule' smart constructor.
data DescribeRule = DescribeRule'
  { _desEventBusName ::
      !(Maybe Text),
    _desName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desEventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'desName' - The name of the rule.
describeRule ::
  -- | 'desName'
  Text ->
  DescribeRule
describeRule pName_ =
  DescribeRule' {_desEventBusName = Nothing, _desName = pName_}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
desEventBusName :: Lens' DescribeRule (Maybe Text)
desEventBusName = lens _desEventBusName (\s a -> s {_desEventBusName = a})

-- | The name of the rule.
desName :: Lens' DescribeRule Text
desName = lens _desName (\s a -> s {_desName = a})

instance AWSRequest DescribeRule where
  type Rs DescribeRule = DescribeRuleResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          DescribeRuleResponse'
            <$> (x .?> "EventPattern")
            <*> (x .?> "State")
            <*> (x .?> "Arn")
            <*> (x .?> "CreatedBy")
            <*> (x .?> "EventBusName")
            <*> (x .?> "ScheduleExpression")
            <*> (x .?> "Name")
            <*> (x .?> "Description")
            <*> (x .?> "ManagedBy")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeRule

instance NFData DescribeRule

instance ToHeaders DescribeRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.DescribeRule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeRule where
  toJSON DescribeRule' {..} =
    object
      ( catMaybes
          [ ("EventBusName" .=) <$> _desEventBusName,
            Just ("Name" .= _desName)
          ]
      )

instance ToPath DescribeRule where
  toPath = const "/"

instance ToQuery DescribeRule where
  toQuery = const mempty

-- | /See:/ 'describeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { _drrsEventPattern ::
      !(Maybe Text),
    _drrsState :: !(Maybe RuleState),
    _drrsARN :: !(Maybe Text),
    _drrsCreatedBy :: !(Maybe Text),
    _drrsEventBusName :: !(Maybe Text),
    _drrsScheduleExpression :: !(Maybe Text),
    _drrsName :: !(Maybe Text),
    _drrsDescription :: !(Maybe Text),
    _drrsManagedBy :: !(Maybe Text),
    _drrsRoleARN :: !(Maybe Text),
    _drrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsEventPattern' - The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- * 'drrsState' - Specifies whether the rule is enabled or disabled.
--
-- * 'drrsARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'drrsCreatedBy' - The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
--
-- * 'drrsEventBusName' - The name of the event bus associated with the rule.
--
-- * 'drrsScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- * 'drrsName' - The name of the rule.
--
-- * 'drrsDescription' - The description of the rule.
--
-- * 'drrsManagedBy' - If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
--
-- * 'drrsRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRuleResponse ::
  -- | 'drrsResponseStatus'
  Int ->
  DescribeRuleResponse
describeRuleResponse pResponseStatus_ =
  DescribeRuleResponse'
    { _drrsEventPattern = Nothing,
      _drrsState = Nothing,
      _drrsARN = Nothing,
      _drrsCreatedBy = Nothing,
      _drrsEventBusName = Nothing,
      _drrsScheduleExpression = Nothing,
      _drrsName = Nothing,
      _drrsDescription = Nothing,
      _drrsManagedBy = Nothing,
      _drrsRoleARN = Nothing,
      _drrsResponseStatus = pResponseStatus_
    }

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
drrsEventPattern :: Lens' DescribeRuleResponse (Maybe Text)
drrsEventPattern = lens _drrsEventPattern (\s a -> s {_drrsEventPattern = a})

-- | Specifies whether the rule is enabled or disabled.
drrsState :: Lens' DescribeRuleResponse (Maybe RuleState)
drrsState = lens _drrsState (\s a -> s {_drrsState = a})

-- | The Amazon Resource Name (ARN) of the rule.
drrsARN :: Lens' DescribeRuleResponse (Maybe Text)
drrsARN = lens _drrsARN (\s a -> s {_drrsARN = a})

-- | The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
drrsCreatedBy :: Lens' DescribeRuleResponse (Maybe Text)
drrsCreatedBy = lens _drrsCreatedBy (\s a -> s {_drrsCreatedBy = a})

-- | The name of the event bus associated with the rule.
drrsEventBusName :: Lens' DescribeRuleResponse (Maybe Text)
drrsEventBusName = lens _drrsEventBusName (\s a -> s {_drrsEventBusName = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
drrsScheduleExpression :: Lens' DescribeRuleResponse (Maybe Text)
drrsScheduleExpression = lens _drrsScheduleExpression (\s a -> s {_drrsScheduleExpression = a})

-- | The name of the rule.
drrsName :: Lens' DescribeRuleResponse (Maybe Text)
drrsName = lens _drrsName (\s a -> s {_drrsName = a})

-- | The description of the rule.
drrsDescription :: Lens' DescribeRuleResponse (Maybe Text)
drrsDescription = lens _drrsDescription (\s a -> s {_drrsDescription = a})

-- | If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
drrsManagedBy :: Lens' DescribeRuleResponse (Maybe Text)
drrsManagedBy = lens _drrsManagedBy (\s a -> s {_drrsManagedBy = a})

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
drrsRoleARN :: Lens' DescribeRuleResponse (Maybe Text)
drrsRoleARN = lens _drrsRoleARN (\s a -> s {_drrsRoleARN = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRuleResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\s a -> s {_drrsResponseStatus = a})

instance NFData DescribeRuleResponse
