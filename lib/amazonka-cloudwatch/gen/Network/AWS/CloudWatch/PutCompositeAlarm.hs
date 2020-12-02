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
-- Module      : Network.AWS.CloudWatch.PutCompositeAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a /composite alarm/ . When you create a composite alarm, you specify a rule expression for the alarm that takes into account the alarm states of other alarms that you have created. The composite alarm goes into ALARM state only if all conditions of the rule are met.
--
--
-- The alarms specified in a composite alarm's rule expression can include metric alarms and other composite alarms.
--
-- Using composite alarms can reduce alarm noise. You can create multiple metric alarms, and also create a composite alarm and set up alerts only for the composite alarm. For example, you could create a composite alarm that goes into ALARM state only when more than one of the underlying metric alarms are in ALARM state.
--
-- Currently, the only alarm actions that can be taken by composite alarms are notifying SNS topics.
--
-- When this operation creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is then evaluated and its state is set appropriately. Any actions associated with the new state are then executed. For a composite alarm, this initial time after creation is the only time that the alarm can be in @INSUFFICIENT_DATA@ state.
--
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.
module Network.AWS.CloudWatch.PutCompositeAlarm
  ( -- * Creating a Request
    putCompositeAlarm,
    PutCompositeAlarm,

    -- * Request Lenses
    pcaAlarmDescription,
    pcaOKActions,
    pcaActionsEnabled,
    pcaInsufficientDataActions,
    pcaAlarmActions,
    pcaTags,
    pcaAlarmName,
    pcaAlarmRule,

    -- * Destructuring the Response
    putCompositeAlarmResponse,
    PutCompositeAlarmResponse,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putCompositeAlarm' smart constructor.
data PutCompositeAlarm = PutCompositeAlarm'
  { _pcaAlarmDescription ::
      !(Maybe Text),
    _pcaOKActions :: !(Maybe [Text]),
    _pcaActionsEnabled :: !(Maybe Bool),
    _pcaInsufficientDataActions :: !(Maybe [Text]),
    _pcaAlarmActions :: !(Maybe [Text]),
    _pcaTags :: !(Maybe [Tag]),
    _pcaAlarmName :: !Text,
    _pcaAlarmRule :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutCompositeAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcaAlarmDescription' - The description for the composite alarm.
--
-- * 'pcaOKActions' - The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- * 'pcaActionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
--
-- * 'pcaInsufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- * 'pcaAlarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- * 'pcaTags' - A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm. Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
--
-- * 'pcaAlarmName' - The name for the composite alarm. This name must be unique within the Region.
--
-- * 'pcaAlarmRule' - An expression that specifies which other alarms are to be evaluated to determine this composite alarm's state. For each alarm that you reference, you designate a function that specifies whether that alarm needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You can use operators (AND, OR and NOT) to combine multiple functions in a single expression. You can use parenthesis to logically group the functions in your expression. You can use either alarm names or ARNs to reference the other alarms that are to be evaluated. Functions can include the following:     * @ALARM("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in ALARM state.     * @OK("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in OK state.     * @INSUFFICIENT_DATA("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in INSUFFICIENT_DATA state.     * @TRUE@ always evaluates to TRUE.     * @FALSE@ always evaluates to FALSE. TRUE and FALSE are useful for testing a complex @AlarmRule@ structure, and for testing your alarm actions. Alarm names specified in @AlarmRule@ can be surrounded with double-quotes ("), but do not have to be. The following are some examples of @AlarmRule@ :     * @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@ specifies that the composite alarm goes into ALARM state only if both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in ALARM state.     * @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@ specifies that the alarm goes to ALARM state if CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is not in ALARM state. This example reduces alarm noise during a known deployment window.     * @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@ goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh is in ALARM state, and if NetworkOutTooHigh is in OK state. This provides another example of using a composite alarm to prevent noise. This rule ensures that you are not notified with an alarm action on high CPU or disk usage if a known network problem is also occurring. The @AlarmRule@ can specify as many as 100 "children" alarms. The @AlarmRule@ expression can have as many as 500 elements. Elements are child alarms, TRUE or FALSE statements, and parentheses.
putCompositeAlarm ::
  -- | 'pcaAlarmName'
  Text ->
  -- | 'pcaAlarmRule'
  Text ->
  PutCompositeAlarm
putCompositeAlarm pAlarmName_ pAlarmRule_ =
  PutCompositeAlarm'
    { _pcaAlarmDescription = Nothing,
      _pcaOKActions = Nothing,
      _pcaActionsEnabled = Nothing,
      _pcaInsufficientDataActions = Nothing,
      _pcaAlarmActions = Nothing,
      _pcaTags = Nothing,
      _pcaAlarmName = pAlarmName_,
      _pcaAlarmRule = pAlarmRule_
    }

-- | The description for the composite alarm.
pcaAlarmDescription :: Lens' PutCompositeAlarm (Maybe Text)
pcaAlarmDescription = lens _pcaAlarmDescription (\s a -> s {_pcaAlarmDescription = a})

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
pcaOKActions :: Lens' PutCompositeAlarm [Text]
pcaOKActions = lens _pcaOKActions (\s a -> s {_pcaOKActions = a}) . _Default . _Coerce

-- | Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
pcaActionsEnabled :: Lens' PutCompositeAlarm (Maybe Bool)
pcaActionsEnabled = lens _pcaActionsEnabled (\s a -> s {_pcaActionsEnabled = a})

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
pcaInsufficientDataActions :: Lens' PutCompositeAlarm [Text]
pcaInsufficientDataActions = lens _pcaInsufficientDataActions (\s a -> s {_pcaInsufficientDataActions = a}) . _Default . _Coerce

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
pcaAlarmActions :: Lens' PutCompositeAlarm [Text]
pcaAlarmActions = lens _pcaAlarmActions (\s a -> s {_pcaAlarmActions = a}) . _Default . _Coerce

-- | A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm. Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
pcaTags :: Lens' PutCompositeAlarm [Tag]
pcaTags = lens _pcaTags (\s a -> s {_pcaTags = a}) . _Default . _Coerce

-- | The name for the composite alarm. This name must be unique within the Region.
pcaAlarmName :: Lens' PutCompositeAlarm Text
pcaAlarmName = lens _pcaAlarmName (\s a -> s {_pcaAlarmName = a})

-- | An expression that specifies which other alarms are to be evaluated to determine this composite alarm's state. For each alarm that you reference, you designate a function that specifies whether that alarm needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You can use operators (AND, OR and NOT) to combine multiple functions in a single expression. You can use parenthesis to logically group the functions in your expression. You can use either alarm names or ARNs to reference the other alarms that are to be evaluated. Functions can include the following:     * @ALARM("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in ALARM state.     * @OK("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in OK state.     * @INSUFFICIENT_DATA("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in INSUFFICIENT_DATA state.     * @TRUE@ always evaluates to TRUE.     * @FALSE@ always evaluates to FALSE. TRUE and FALSE are useful for testing a complex @AlarmRule@ structure, and for testing your alarm actions. Alarm names specified in @AlarmRule@ can be surrounded with double-quotes ("), but do not have to be. The following are some examples of @AlarmRule@ :     * @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@ specifies that the composite alarm goes into ALARM state only if both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in ALARM state.     * @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@ specifies that the alarm goes to ALARM state if CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is not in ALARM state. This example reduces alarm noise during a known deployment window.     * @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@ goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh is in ALARM state, and if NetworkOutTooHigh is in OK state. This provides another example of using a composite alarm to prevent noise. This rule ensures that you are not notified with an alarm action on high CPU or disk usage if a known network problem is also occurring. The @AlarmRule@ can specify as many as 100 "children" alarms. The @AlarmRule@ expression can have as many as 500 elements. Elements are child alarms, TRUE or FALSE statements, and parentheses.
pcaAlarmRule :: Lens' PutCompositeAlarm Text
pcaAlarmRule = lens _pcaAlarmRule (\s a -> s {_pcaAlarmRule = a})

instance AWSRequest PutCompositeAlarm where
  type Rs PutCompositeAlarm = PutCompositeAlarmResponse
  request = postQuery cloudWatch
  response = receiveNull PutCompositeAlarmResponse'

instance Hashable PutCompositeAlarm

instance NFData PutCompositeAlarm

instance ToHeaders PutCompositeAlarm where
  toHeaders = const mempty

instance ToPath PutCompositeAlarm where
  toPath = const "/"

instance ToQuery PutCompositeAlarm where
  toQuery PutCompositeAlarm' {..} =
    mconcat
      [ "Action" =: ("PutCompositeAlarm" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "AlarmDescription" =: _pcaAlarmDescription,
        "OKActions" =: toQuery (toQueryList "member" <$> _pcaOKActions),
        "ActionsEnabled" =: _pcaActionsEnabled,
        "InsufficientDataActions"
          =: toQuery (toQueryList "member" <$> _pcaInsufficientDataActions),
        "AlarmActions"
          =: toQuery (toQueryList "member" <$> _pcaAlarmActions),
        "Tags" =: toQuery (toQueryList "member" <$> _pcaTags),
        "AlarmName" =: _pcaAlarmName,
        "AlarmRule" =: _pcaAlarmRule
      ]

-- | /See:/ 'putCompositeAlarmResponse' smart constructor.
data PutCompositeAlarmResponse = PutCompositeAlarmResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutCompositeAlarmResponse' with the minimum fields required to make a request.
putCompositeAlarmResponse ::
  PutCompositeAlarmResponse
putCompositeAlarmResponse = PutCompositeAlarmResponse'

instance NFData PutCompositeAlarmResponse
