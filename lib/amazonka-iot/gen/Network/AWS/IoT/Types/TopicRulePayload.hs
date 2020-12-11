-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRulePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRulePayload
  ( TopicRulePayload (..),

    -- * Smart constructor
    mkTopicRulePayload,

    -- * Lenses
    trpAwsIotSqlVersion,
    trpErrorAction,
    trpRuleDisabled,
    trpDescription,
    trpSql,
    trpActions,
  )
where

import Network.AWS.IoT.Types.Action
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a rule.
--
-- /See:/ 'mkTopicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { awsIotSqlVersion ::
      Lude.Maybe Lude.Text,
    errorAction :: Lude.Maybe Action,
    ruleDisabled :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    sql :: Lude.Text,
    actions :: [Action]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRulePayload' with the minimum fields required to make a request.
--
-- * 'actions' - The actions associated with the rule.
-- * 'awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
-- * 'description' - The description of the rule.
-- * 'errorAction' - The action to take when an error occurs.
-- * 'ruleDisabled' - Specifies whether the rule is disabled.
-- * 'sql' - The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
mkTopicRulePayload ::
  -- | 'sql'
  Lude.Text ->
  TopicRulePayload
mkTopicRulePayload pSql_ =
  TopicRulePayload'
    { awsIotSqlVersion = Lude.Nothing,
      errorAction = Lude.Nothing,
      ruleDisabled = Lude.Nothing,
      description = Lude.Nothing,
      sql = pSql_,
      actions = Lude.mempty
    }

-- | The version of the SQL rules engine to use when evaluating the rule.
--
-- /Note:/ Consider using 'awsIotSqlVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpAwsIotSqlVersion :: Lens.Lens' TopicRulePayload (Lude.Maybe Lude.Text)
trpAwsIotSqlVersion = Lens.lens (awsIotSqlVersion :: TopicRulePayload -> Lude.Maybe Lude.Text) (\s a -> s {awsIotSqlVersion = a} :: TopicRulePayload)
{-# DEPRECATED trpAwsIotSqlVersion "Use generic-lens or generic-optics with 'awsIotSqlVersion' instead." #-}

-- | The action to take when an error occurs.
--
-- /Note:/ Consider using 'errorAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpErrorAction :: Lens.Lens' TopicRulePayload (Lude.Maybe Action)
trpErrorAction = Lens.lens (errorAction :: TopicRulePayload -> Lude.Maybe Action) (\s a -> s {errorAction = a} :: TopicRulePayload)
{-# DEPRECATED trpErrorAction "Use generic-lens or generic-optics with 'errorAction' instead." #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpRuleDisabled :: Lens.Lens' TopicRulePayload (Lude.Maybe Lude.Bool)
trpRuleDisabled = Lens.lens (ruleDisabled :: TopicRulePayload -> Lude.Maybe Lude.Bool) (\s a -> s {ruleDisabled = a} :: TopicRulePayload)
{-# DEPRECATED trpRuleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead." #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpDescription :: Lens.Lens' TopicRulePayload (Lude.Maybe Lude.Text)
trpDescription = Lens.lens (description :: TopicRulePayload -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TopicRulePayload)
{-# DEPRECATED trpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
--
-- /Note:/ Consider using 'sql' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpSql :: Lens.Lens' TopicRulePayload Lude.Text
trpSql = Lens.lens (sql :: TopicRulePayload -> Lude.Text) (\s a -> s {sql = a} :: TopicRulePayload)
{-# DEPRECATED trpSql "Use generic-lens or generic-optics with 'sql' instead." #-}

-- | The actions associated with the rule.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpActions :: Lens.Lens' TopicRulePayload [Action]
trpActions = Lens.lens (actions :: TopicRulePayload -> [Action]) (\s a -> s {actions = a} :: TopicRulePayload)
{-# DEPRECATED trpActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.ToJSON TopicRulePayload where
  toJSON TopicRulePayload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("awsIotSqlVersion" Lude..=) Lude.<$> awsIotSqlVersion,
            ("errorAction" Lude..=) Lude.<$> errorAction,
            ("ruleDisabled" Lude..=) Lude.<$> ruleDisabled,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("sql" Lude..= sql),
            Lude.Just ("actions" Lude..= actions)
          ]
      )
