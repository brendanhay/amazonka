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
-- Module      : Network.AWS.CloudWatchEvents.PutRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the specified rule. Rules are enabled by default, or based on value of the state. You can disable a rule using 'DisableRule' .
--
--
-- If you are updating an existing rule, the rule is completely replaced with what you specify in this @PutRule@ command. If you omit arguments in @PutRule@ , the old values for those arguments are not kept. Instead, they are replaced with null values.
--
-- When you create or update a rule, incoming events might not immediately start matching to new or updated rules. Please allow a short period of time for changes to take effect.
--
-- A rule must contain at least an EventPattern or ScheduleExpression. Rules with EventPatterns are triggered when a matching event is observed. Rules with ScheduleExpressions self-trigger based on the given schedule. A rule can have both an EventPattern and a ScheduleExpression, in which case the rule triggers on matching events as well as on a schedule.
--
-- Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.
--
module Network.AWS.CloudWatchEvents.PutRule
    (
    -- * Creating a Request
      putRule
    , PutRule
    -- * Request Lenses
    , prEventPattern
    , prState
    , prScheduleExpression
    , prDescription
    , prRoleARN
    , prName

    -- * Destructuring the Response
    , putRuleResponse
    , PutRuleResponse
    -- * Response Lenses
    , prrsRuleARN
    , prrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRule' smart constructor.
data PutRule = PutRule'
  { _prEventPattern       :: !(Maybe Text)
  , _prState              :: !(Maybe RuleState)
  , _prScheduleExpression :: !(Maybe Text)
  , _prDescription        :: !(Maybe Text)
  , _prRoleARN            :: !(Maybe Text)
  , _prName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prEventPattern' - The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
--
-- * 'prState' - Indicates whether the rule is enabled or disabled.
--
-- * 'prScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)" or "rate(5 minutes)".
--
-- * 'prDescription' - A description of the rule.
--
-- * 'prRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- * 'prName' - The name of the rule that you are creating or updating.
putRule
    :: Text -- ^ 'prName'
    -> PutRule
putRule pName_ =
  PutRule'
    { _prEventPattern = Nothing
    , _prState = Nothing
    , _prScheduleExpression = Nothing
    , _prDescription = Nothing
    , _prRoleARN = Nothing
    , _prName = pName_
    }


-- | The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
prEventPattern :: Lens' PutRule (Maybe Text)
prEventPattern = lens _prEventPattern (\ s a -> s{_prEventPattern = a})

-- | Indicates whether the rule is enabled or disabled.
prState :: Lens' PutRule (Maybe RuleState)
prState = lens _prState (\ s a -> s{_prState = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)" or "rate(5 minutes)".
prScheduleExpression :: Lens' PutRule (Maybe Text)
prScheduleExpression = lens _prScheduleExpression (\ s a -> s{_prScheduleExpression = a})

-- | A description of the rule.
prDescription :: Lens' PutRule (Maybe Text)
prDescription = lens _prDescription (\ s a -> s{_prDescription = a})

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
prRoleARN :: Lens' PutRule (Maybe Text)
prRoleARN = lens _prRoleARN (\ s a -> s{_prRoleARN = a})

-- | The name of the rule that you are creating or updating.
prName :: Lens' PutRule Text
prName = lens _prName (\ s a -> s{_prName = a})

instance AWSRequest PutRule where
        type Rs PutRule = PutRuleResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutRuleResponse' <$>
                   (x .?> "RuleArn") <*> (pure (fromEnum s)))

instance Hashable PutRule where

instance NFData PutRule where

instance ToHeaders PutRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRule where
        toJSON PutRule'{..}
          = object
              (catMaybes
                 [("EventPattern" .=) <$> _prEventPattern,
                  ("State" .=) <$> _prState,
                  ("ScheduleExpression" .=) <$> _prScheduleExpression,
                  ("Description" .=) <$> _prDescription,
                  ("RoleArn" .=) <$> _prRoleARN,
                  Just ("Name" .= _prName)])

instance ToPath PutRule where
        toPath = const "/"

instance ToQuery PutRule where
        toQuery = const mempty

-- | /See:/ 'putRuleResponse' smart constructor.
data PutRuleResponse = PutRuleResponse'
  { _prrsRuleARN        :: !(Maybe Text)
  , _prrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prrsRuleARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'prrsResponseStatus' - -- | The response status code.
putRuleResponse
    :: Int -- ^ 'prrsResponseStatus'
    -> PutRuleResponse
putRuleResponse pResponseStatus_ =
  PutRuleResponse'
    {_prrsRuleARN = Nothing, _prrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the rule.
prrsRuleARN :: Lens' PutRuleResponse (Maybe Text)
prrsRuleARN = lens _prrsRuleARN (\ s a -> s{_prrsRuleARN = a})

-- | -- | The response status code.
prrsResponseStatus :: Lens' PutRuleResponse Int
prrsResponseStatus = lens _prrsResponseStatus (\ s a -> s{_prrsResponseStatus = a})

instance NFData PutRuleResponse where
