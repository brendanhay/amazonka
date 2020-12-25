{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the rule.
module Network.AWS.IoT.DeleteTopicRule
  ( -- * Creating a request
    DeleteTopicRule (..),
    mkDeleteTopicRule,

    -- ** Request lenses
    dRuleName,

    -- * Destructuring the response
    DeleteTopicRuleResponse (..),
    mkDeleteTopicRuleResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'mkDeleteTopicRule' smart constructor.
newtype DeleteTopicRule = DeleteTopicRule'
  { -- | The name of the rule.
    ruleName :: Types.RuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRule' value with any optional fields omitted.
mkDeleteTopicRule ::
  -- | 'ruleName'
  Types.RuleName ->
  DeleteTopicRule
mkDeleteTopicRule ruleName = DeleteTopicRule' {ruleName}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleName :: Lens.Lens' DeleteTopicRule Types.RuleName
dRuleName = Lens.field @"ruleName"
{-# DEPRECATED dRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.AWSRequest DeleteTopicRule where
  type Rs DeleteTopicRule = DeleteTopicRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/rules/" Core.<> (Core.toText ruleName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteTopicRuleResponse'

-- | /See:/ 'mkDeleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse = DeleteTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRuleResponse' value with any optional fields omitted.
mkDeleteTopicRuleResponse ::
  DeleteTopicRuleResponse
mkDeleteTopicRuleResponse = DeleteTopicRuleResponse'
