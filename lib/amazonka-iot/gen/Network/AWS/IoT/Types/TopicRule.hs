-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRule
  ( TopicRule (..),

    -- * Smart constructor
    mkTopicRule,

    -- * Lenses
    trCreatedAt,
    trActions,
    trAwsIotSqlVersion,
    trErrorAction,
    trRuleDisabled,
    trRuleName,
    trSql,
    trDescription,
  )
where

import Network.AWS.IoT.Types.Action
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a rule.
--
-- /See:/ 'mkTopicRule' smart constructor.
data TopicRule = TopicRule'
  { createdAt :: Lude.Maybe Lude.Timestamp,
    actions :: Lude.Maybe [Action],
    awsIotSqlVersion :: Lude.Maybe Lude.Text,
    errorAction :: Lude.Maybe Action,
    ruleDisabled :: Lude.Maybe Lude.Bool,
    ruleName :: Lude.Maybe Lude.Text,
    sql :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRule' with the minimum fields required to make a request.
--
-- * 'actions' - The actions associated with the rule.
-- * 'awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
-- * 'createdAt' - The date and time the rule was created.
-- * 'description' - The description of the rule.
-- * 'errorAction' - The action to perform when an error occurs.
-- * 'ruleDisabled' - Specifies whether the rule is disabled.
-- * 'ruleName' - The name of the rule.
-- * 'sql' - The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
mkTopicRule ::
  TopicRule
mkTopicRule =
  TopicRule'
    { createdAt = Lude.Nothing,
      actions = Lude.Nothing,
      awsIotSqlVersion = Lude.Nothing,
      errorAction = Lude.Nothing,
      ruleDisabled = Lude.Nothing,
      ruleName = Lude.Nothing,
      sql = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date and time the rule was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCreatedAt :: Lens.Lens' TopicRule (Lude.Maybe Lude.Timestamp)
trCreatedAt = Lens.lens (createdAt :: TopicRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: TopicRule)
{-# DEPRECATED trCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The actions associated with the rule.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trActions :: Lens.Lens' TopicRule (Lude.Maybe [Action])
trActions = Lens.lens (actions :: TopicRule -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: TopicRule)
{-# DEPRECATED trActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The version of the SQL rules engine to use when evaluating the rule.
--
-- /Note:/ Consider using 'awsIotSqlVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trAwsIotSqlVersion :: Lens.Lens' TopicRule (Lude.Maybe Lude.Text)
trAwsIotSqlVersion = Lens.lens (awsIotSqlVersion :: TopicRule -> Lude.Maybe Lude.Text) (\s a -> s {awsIotSqlVersion = a} :: TopicRule)
{-# DEPRECATED trAwsIotSqlVersion "Use generic-lens or generic-optics with 'awsIotSqlVersion' instead." #-}

-- | The action to perform when an error occurs.
--
-- /Note:/ Consider using 'errorAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trErrorAction :: Lens.Lens' TopicRule (Lude.Maybe Action)
trErrorAction = Lens.lens (errorAction :: TopicRule -> Lude.Maybe Action) (\s a -> s {errorAction = a} :: TopicRule)
{-# DEPRECATED trErrorAction "Use generic-lens or generic-optics with 'errorAction' instead." #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trRuleDisabled :: Lens.Lens' TopicRule (Lude.Maybe Lude.Bool)
trRuleDisabled = Lens.lens (ruleDisabled :: TopicRule -> Lude.Maybe Lude.Bool) (\s a -> s {ruleDisabled = a} :: TopicRule)
{-# DEPRECATED trRuleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trRuleName :: Lens.Lens' TopicRule (Lude.Maybe Lude.Text)
trRuleName = Lens.lens (ruleName :: TopicRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: TopicRule)
{-# DEPRECATED trRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
--
-- /Note:/ Consider using 'sql' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSql :: Lens.Lens' TopicRule (Lude.Maybe Lude.Text)
trSql = Lens.lens (sql :: TopicRule -> Lude.Maybe Lude.Text) (\s a -> s {sql = a} :: TopicRule)
{-# DEPRECATED trSql "Use generic-lens or generic-optics with 'sql' instead." #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trDescription :: Lens.Lens' TopicRule (Lude.Maybe Lude.Text)
trDescription = Lens.lens (description :: TopicRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TopicRule)
{-# DEPRECATED trDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON TopicRule where
  parseJSON =
    Lude.withObject
      "TopicRule"
      ( \x ->
          TopicRule'
            Lude.<$> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "actions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "awsIotSqlVersion")
            Lude.<*> (x Lude..:? "errorAction")
            Lude.<*> (x Lude..:? "ruleDisabled")
            Lude.<*> (x Lude..:? "ruleName")
            Lude.<*> (x Lude..:? "sql")
            Lude.<*> (x Lude..:? "description")
      )
