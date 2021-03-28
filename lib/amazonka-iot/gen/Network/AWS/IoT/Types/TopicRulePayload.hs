{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRulePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TopicRulePayload
  ( TopicRulePayload (..)
  -- * Smart constructor
  , mkTopicRulePayload
  -- * Lenses
  , trpSql
  , trpActions
  , trpAwsIotSqlVersion
  , trpDescription
  , trpErrorAction
  , trpRuleDisabled
  ) where

import qualified Network.AWS.IoT.Types.Action as Types
import qualified Network.AWS.IoT.Types.AwsIotSqlVersion as Types
import qualified Network.AWS.IoT.Types.Description as Types
import qualified Network.AWS.IoT.Types.SQL as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a rule.
--
-- /See:/ 'mkTopicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { sql :: Types.SQL
    -- ^ The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
  , actions :: [Types.Action]
    -- ^ The actions associated with the rule.
  , awsIotSqlVersion :: Core.Maybe Types.AwsIotSqlVersion
    -- ^ The version of the SQL rules engine to use when evaluating the rule.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the rule.
  , errorAction :: Core.Maybe Types.Action
    -- ^ The action to take when an error occurs.
  , ruleDisabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the rule is disabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TopicRulePayload' value with any optional fields omitted.
mkTopicRulePayload
    :: Types.SQL -- ^ 'sql'
    -> TopicRulePayload
mkTopicRulePayload sql
  = TopicRulePayload'{sql, actions = Core.mempty,
                      awsIotSqlVersion = Core.Nothing, description = Core.Nothing,
                      errorAction = Core.Nothing, ruleDisabled = Core.Nothing}

-- | The SQL statement used to query the topic. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
--
-- /Note:/ Consider using 'sql' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpSql :: Lens.Lens' TopicRulePayload Types.SQL
trpSql = Lens.field @"sql"
{-# INLINEABLE trpSql #-}
{-# DEPRECATED sql "Use generic-lens or generic-optics with 'sql' instead"  #-}

-- | The actions associated with the rule.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpActions :: Lens.Lens' TopicRulePayload [Types.Action]
trpActions = Lens.field @"actions"
{-# INLINEABLE trpActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | The version of the SQL rules engine to use when evaluating the rule.
--
-- /Note:/ Consider using 'awsIotSqlVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpAwsIotSqlVersion :: Lens.Lens' TopicRulePayload (Core.Maybe Types.AwsIotSqlVersion)
trpAwsIotSqlVersion = Lens.field @"awsIotSqlVersion"
{-# INLINEABLE trpAwsIotSqlVersion #-}
{-# DEPRECATED awsIotSqlVersion "Use generic-lens or generic-optics with 'awsIotSqlVersion' instead"  #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpDescription :: Lens.Lens' TopicRulePayload (Core.Maybe Types.Description)
trpDescription = Lens.field @"description"
{-# INLINEABLE trpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The action to take when an error occurs.
--
-- /Note:/ Consider using 'errorAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpErrorAction :: Lens.Lens' TopicRulePayload (Core.Maybe Types.Action)
trpErrorAction = Lens.field @"errorAction"
{-# INLINEABLE trpErrorAction #-}
{-# DEPRECATED errorAction "Use generic-lens or generic-optics with 'errorAction' instead"  #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpRuleDisabled :: Lens.Lens' TopicRulePayload (Core.Maybe Core.Bool)
trpRuleDisabled = Lens.field @"ruleDisabled"
{-# INLINEABLE trpRuleDisabled #-}
{-# DEPRECATED ruleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead"  #-}

instance Core.FromJSON TopicRulePayload where
        toJSON TopicRulePayload{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("sql" Core..= sql),
                  Core.Just ("actions" Core..= actions),
                  ("awsIotSqlVersion" Core..=) Core.<$> awsIotSqlVersion,
                  ("description" Core..=) Core.<$> description,
                  ("errorAction" Core..=) Core.<$> errorAction,
                  ("ruleDisabled" Core..=) Core.<$> ruleDisabled])
