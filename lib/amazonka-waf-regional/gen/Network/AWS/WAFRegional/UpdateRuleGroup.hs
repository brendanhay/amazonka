{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @RuleGroup@ .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- You can have a maximum of ten rules per rule group.
-- To create and configure a @RuleGroup@ , perform the following steps:
--
--     * Create and update the @Rules@ that you want to include in the @RuleGroup@ . See 'CreateRule' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRuleGroup' request.
--
--
--     * Submit an @UpdateRuleGroup@ request to add @Rules@ to the @RuleGroup@ .
--
--
--     * Create and update a @WebACL@ that contains the @RuleGroup@ . See 'CreateWebACL' .
--
--
-- If you want to replace one @Rule@ with another, you delete the existing one and add the new one.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateRuleGroup
    (
    -- * Creating a request
      UpdateRuleGroup (..)
    , mkUpdateRuleGroup
    -- ** Request lenses
    , urgRuleGroupId
    , urgUpdates
    , urgChangeToken

    -- * Destructuring the response
    , UpdateRuleGroupResponse (..)
    , mkUpdateRuleGroupResponse
    -- ** Response lenses
    , urgrrsChangeToken
    , urgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { ruleGroupId :: Types.ResourceId
    -- ^ The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
  , updates :: Core.NonEmpty Types.RuleGroupUpdate
    -- ^ An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuleGroup' value with any optional fields omitted.
mkUpdateRuleGroup
    :: Types.ResourceId -- ^ 'ruleGroupId'
    -> Core.NonEmpty Types.RuleGroupUpdate -- ^ 'updates'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> UpdateRuleGroup
mkUpdateRuleGroup ruleGroupId updates changeToken
  = UpdateRuleGroup'{ruleGroupId, updates, changeToken}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgRuleGroupId :: Lens.Lens' UpdateRuleGroup Types.ResourceId
urgRuleGroupId = Lens.field @"ruleGroupId"
{-# INLINEABLE urgRuleGroupId #-}
{-# DEPRECATED ruleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead"  #-}

-- | An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgUpdates :: Lens.Lens' UpdateRuleGroup (Core.NonEmpty Types.RuleGroupUpdate)
urgUpdates = Lens.field @"updates"
{-# INLINEABLE urgUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgChangeToken :: Lens.Lens' UpdateRuleGroup Types.ChangeToken
urgChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urgChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery UpdateRuleGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRuleGroup where
        toHeaders UpdateRuleGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.UpdateRuleGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRuleGroup where
        toJSON UpdateRuleGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RuleGroupId" Core..= ruleGroupId),
                  Core.Just ("Updates" Core..= updates),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest UpdateRuleGroup where
        type Rs UpdateRuleGroup = UpdateRuleGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRuleGroupResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuleGroupResponse' value with any optional fields omitted.
mkUpdateRuleGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRuleGroupResponse
mkUpdateRuleGroupResponse responseStatus
  = UpdateRuleGroupResponse'{changeToken = Core.Nothing,
                             responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsChangeToken :: Lens.Lens' UpdateRuleGroupResponse (Core.Maybe Types.ChangeToken)
urgrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urgrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsResponseStatus :: Lens.Lens' UpdateRuleGroupResponse Core.Int
urgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
