{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRule where

import Network.AWS.IoT.Types.Action
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a rule.
--
--
--
-- /See:/ 'topicRule' smart constructor.
data TopicRule = TopicRule'
  { _trCreatedAt :: !(Maybe POSIX),
    _trActions :: !(Maybe [Action]),
    _trAwsIotSqlVersion :: !(Maybe Text),
    _trErrorAction :: !(Maybe Action),
    _trRuleDisabled :: !(Maybe Bool),
    _trRuleName :: !(Maybe Text),
    _trSql :: !(Maybe Text),
    _trDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trCreatedAt' - The date and time the rule was created.
--
-- * 'trActions' - The actions associated with the rule.
--
-- * 'trAwsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- * 'trErrorAction' - The action to perform when an error occurs.
--
-- * 'trRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'trRuleName' - The name of the rule.
--
-- * 'trSql' - The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
--
-- * 'trDescription' - The description of the rule.
topicRule ::
  TopicRule
topicRule =
  TopicRule'
    { _trCreatedAt = Nothing,
      _trActions = Nothing,
      _trAwsIotSqlVersion = Nothing,
      _trErrorAction = Nothing,
      _trRuleDisabled = Nothing,
      _trRuleName = Nothing,
      _trSql = Nothing,
      _trDescription = Nothing
    }

-- | The date and time the rule was created.
trCreatedAt :: Lens' TopicRule (Maybe UTCTime)
trCreatedAt = lens _trCreatedAt (\s a -> s {_trCreatedAt = a}) . mapping _Time

-- | The actions associated with the rule.
trActions :: Lens' TopicRule [Action]
trActions = lens _trActions (\s a -> s {_trActions = a}) . _Default . _Coerce

-- | The version of the SQL rules engine to use when evaluating the rule.
trAwsIotSqlVersion :: Lens' TopicRule (Maybe Text)
trAwsIotSqlVersion = lens _trAwsIotSqlVersion (\s a -> s {_trAwsIotSqlVersion = a})

-- | The action to perform when an error occurs.
trErrorAction :: Lens' TopicRule (Maybe Action)
trErrorAction = lens _trErrorAction (\s a -> s {_trErrorAction = a})

-- | Specifies whether the rule is disabled.
trRuleDisabled :: Lens' TopicRule (Maybe Bool)
trRuleDisabled = lens _trRuleDisabled (\s a -> s {_trRuleDisabled = a})

-- | The name of the rule.
trRuleName :: Lens' TopicRule (Maybe Text)
trRuleName = lens _trRuleName (\s a -> s {_trRuleName = a})

-- | The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
trSql :: Lens' TopicRule (Maybe Text)
trSql = lens _trSql (\s a -> s {_trSql = a})

-- | The description of the rule.
trDescription :: Lens' TopicRule (Maybe Text)
trDescription = lens _trDescription (\s a -> s {_trDescription = a})

instance FromJSON TopicRule where
  parseJSON =
    withObject
      "TopicRule"
      ( \x ->
          TopicRule'
            <$> (x .:? "createdAt")
            <*> (x .:? "actions" .!= mempty)
            <*> (x .:? "awsIotSqlVersion")
            <*> (x .:? "errorAction")
            <*> (x .:? "ruleDisabled")
            <*> (x .:? "ruleName")
            <*> (x .:? "sql")
            <*> (x .:? "description")
      )

instance Hashable TopicRule

instance NFData TopicRule
