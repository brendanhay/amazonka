{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Rule where

import Network.AWS.CloudWatchEvents.Types.RuleState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a rule in Amazon EventBridge.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rEventPattern :: !(Maybe Text),
    _rState :: !(Maybe RuleState),
    _rARN :: !(Maybe Text),
    _rEventBusName :: !(Maybe Text),
    _rScheduleExpression :: !(Maybe Text),
    _rName :: !(Maybe Text),
    _rDescription :: !(Maybe Text),
    _rManagedBy :: !(Maybe Text),
    _rRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEventPattern' - The event pattern of the rule. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- * 'rState' - The state of the rule.
--
-- * 'rARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'rEventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'rScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- * 'rName' - The name of the rule.
--
-- * 'rDescription' - The description of the rule.
--
-- * 'rManagedBy' - If the rule was created on behalf of your account by an AWS service, this field displays the principal name of the service that created the rule.
--
-- * 'rRoleARN' - The Amazon Resource Name (ARN) of the role that is used for target invocation.
rule ::
  Rule
rule =
  Rule'
    { _rEventPattern = Nothing,
      _rState = Nothing,
      _rARN = Nothing,
      _rEventBusName = Nothing,
      _rScheduleExpression = Nothing,
      _rName = Nothing,
      _rDescription = Nothing,
      _rManagedBy = Nothing,
      _rRoleARN = Nothing
    }

-- | The event pattern of the rule. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
rEventPattern :: Lens' Rule (Maybe Text)
rEventPattern = lens _rEventPattern (\s a -> s {_rEventPattern = a})

-- | The state of the rule.
rState :: Lens' Rule (Maybe RuleState)
rState = lens _rState (\s a -> s {_rState = a})

-- | The Amazon Resource Name (ARN) of the rule.
rARN :: Lens' Rule (Maybe Text)
rARN = lens _rARN (\s a -> s {_rARN = a})

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
rEventBusName :: Lens' Rule (Maybe Text)
rEventBusName = lens _rEventBusName (\s a -> s {_rEventBusName = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
rScheduleExpression :: Lens' Rule (Maybe Text)
rScheduleExpression = lens _rScheduleExpression (\s a -> s {_rScheduleExpression = a})

-- | The name of the rule.
rName :: Lens' Rule (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | The description of the rule.
rDescription :: Lens' Rule (Maybe Text)
rDescription = lens _rDescription (\s a -> s {_rDescription = a})

-- | If the rule was created on behalf of your account by an AWS service, this field displays the principal name of the service that created the rule.
rManagedBy :: Lens' Rule (Maybe Text)
rManagedBy = lens _rManagedBy (\s a -> s {_rManagedBy = a})

-- | The Amazon Resource Name (ARN) of the role that is used for target invocation.
rRoleARN :: Lens' Rule (Maybe Text)
rRoleARN = lens _rRoleARN (\s a -> s {_rRoleARN = a})

instance FromJSON Rule where
  parseJSON =
    withObject
      "Rule"
      ( \x ->
          Rule'
            <$> (x .:? "EventPattern")
            <*> (x .:? "State")
            <*> (x .:? "Arn")
            <*> (x .:? "EventBusName")
            <*> (x .:? "ScheduleExpression")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
            <*> (x .:? "ManagedBy")
            <*> (x .:? "RoleArn")
      )

instance Hashable Rule

instance NFData Rule
