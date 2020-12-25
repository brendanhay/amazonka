{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS Config rule and all of its evaluation results.
--
-- AWS Config sets the state of a rule to @DELETING@ until the deletion is complete. You cannot update a rule while it is in this state. If you make a @PutConfigRule@ or @DeleteConfigRule@ request for the rule, you will receive a @ResourceInUseException@ .
-- You can check the state of a rule by using the @DescribeConfigRules@ request.
module Network.AWS.Config.DeleteConfigRule
  ( -- * Creating a request
    DeleteConfigRule (..),
    mkDeleteConfigRule,

    -- ** Request lenses
    dcrConfigRuleName,

    -- * Destructuring the response
    DeleteConfigRuleResponse (..),
    mkDeleteConfigRuleResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteConfigRule' smart constructor.
newtype DeleteConfigRule = DeleteConfigRule'
  { -- | The name of the AWS Config rule that you want to delete.
    configRuleName :: Types.ConfigRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigRule' value with any optional fields omitted.
mkDeleteConfigRule ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  DeleteConfigRule
mkDeleteConfigRule configRuleName =
  DeleteConfigRule' {configRuleName}

-- | The name of the AWS Config rule that you want to delete.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigRuleName :: Lens.Lens' DeleteConfigRule Types.ConfigRuleName
dcrConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED dcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Core.FromJSON DeleteConfigRule where
  toJSON DeleteConfigRule {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ConfigRuleName" Core..= configRuleName)]
      )

instance Core.AWSRequest DeleteConfigRule where
  type Rs DeleteConfigRule = DeleteConfigRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StarlingDoveService.DeleteConfigRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteConfigRuleResponse'

-- | /See:/ 'mkDeleteConfigRuleResponse' smart constructor.
data DeleteConfigRuleResponse = DeleteConfigRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigRuleResponse' value with any optional fields omitted.
mkDeleteConfigRuleResponse ::
  DeleteConfigRuleResponse
mkDeleteConfigRuleResponse = DeleteConfigRuleResponse'
