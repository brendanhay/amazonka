{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRulePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRulePayload where

import Network.AWS.IoT.Types.Action
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a rule.
--
--
--
-- /See:/ 'topicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { _trpAwsIotSqlVersion ::
      !(Maybe Text),
    _trpErrorAction :: !(Maybe Action),
    _trpRuleDisabled :: !(Maybe Bool),
    _trpDescription :: !(Maybe Text),
    _trpSql :: !Text,
    _trpActions :: ![Action]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicRulePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpAwsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- * 'trpErrorAction' - The action to take when an error occurs.
--
-- * 'trpRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'trpDescription' - The description of the rule.
--
-- * 'trpSql' - The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
--
-- * 'trpActions' - The actions associated with the rule.
topicRulePayload ::
  -- | 'trpSql'
  Text ->
  TopicRulePayload
topicRulePayload pSql_ =
  TopicRulePayload'
    { _trpAwsIotSqlVersion = Nothing,
      _trpErrorAction = Nothing,
      _trpRuleDisabled = Nothing,
      _trpDescription = Nothing,
      _trpSql = pSql_,
      _trpActions = mempty
    }

-- | The version of the SQL rules engine to use when evaluating the rule.
trpAwsIotSqlVersion :: Lens' TopicRulePayload (Maybe Text)
trpAwsIotSqlVersion = lens _trpAwsIotSqlVersion (\s a -> s {_trpAwsIotSqlVersion = a})

-- | The action to take when an error occurs.
trpErrorAction :: Lens' TopicRulePayload (Maybe Action)
trpErrorAction = lens _trpErrorAction (\s a -> s {_trpErrorAction = a})

-- | Specifies whether the rule is disabled.
trpRuleDisabled :: Lens' TopicRulePayload (Maybe Bool)
trpRuleDisabled = lens _trpRuleDisabled (\s a -> s {_trpRuleDisabled = a})

-- | The description of the rule.
trpDescription :: Lens' TopicRulePayload (Maybe Text)
trpDescription = lens _trpDescription (\s a -> s {_trpDescription = a})

-- | The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
trpSql :: Lens' TopicRulePayload Text
trpSql = lens _trpSql (\s a -> s {_trpSql = a})

-- | The actions associated with the rule.
trpActions :: Lens' TopicRulePayload [Action]
trpActions = lens _trpActions (\s a -> s {_trpActions = a}) . _Coerce

instance Hashable TopicRulePayload

instance NFData TopicRulePayload

instance ToJSON TopicRulePayload where
  toJSON TopicRulePayload' {..} =
    object
      ( catMaybes
          [ ("awsIotSqlVersion" .=) <$> _trpAwsIotSqlVersion,
            ("errorAction" .=) <$> _trpErrorAction,
            ("ruleDisabled" .=) <$> _trpRuleDisabled,
            ("description" .=) <$> _trpDescription,
            Just ("sql" .= _trpSql),
            Just ("actions" .= _trpActions)
          ]
      )
