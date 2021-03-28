{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RuleGroup@ . A rule group is a collection of predefined rules that you add to a web ACL. You use 'UpdateRuleGroup' to add rules to the rule group.
--
-- Rule groups are subject to the following limits:
--
--     * Three rule groups per account. You can request an increase to this limit by contacting customer support.
--
--
--     * One rule group per web ACL.
--
--
--     * Ten rules per rule group.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRuleGroup
    (
    -- * Creating a request
      CreateRuleGroup (..)
    , mkCreateRuleGroup
    -- ** Request lenses
    , crgName
    , crgMetricName
    , crgChangeToken
    , crgTags

    -- * Destructuring the response
    , CreateRuleGroupResponse (..)
    , mkCreateRuleGroupResponse
    -- ** Response lenses
    , crgrrsChangeToken
    , crgrrsRuleGroup
    , crgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { name :: Types.Name
    -- ^ A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
  , metricName :: Types.MetricName
    -- ^ A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRuleGroup' value with any optional fields omitted.
mkCreateRuleGroup
    :: Types.Name -- ^ 'name'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateRuleGroup
mkCreateRuleGroup name metricName changeToken
  = CreateRuleGroup'{name, metricName, changeToken,
                     tags = Core.Nothing}

-- | A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgName :: Lens.Lens' CreateRuleGroup Types.Name
crgName = Lens.field @"name"
{-# INLINEABLE crgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgMetricName :: Lens.Lens' CreateRuleGroup Types.MetricName
crgMetricName = Lens.field @"metricName"
{-# INLINEABLE crgMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgChangeToken :: Lens.Lens' CreateRuleGroup Types.ChangeToken
crgChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crgChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateRuleGroup (Core.Maybe (Core.NonEmpty Types.Tag))
crgTags = Lens.field @"tags"
{-# INLINEABLE crgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRuleGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRuleGroup where
        toHeaders CreateRuleGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateRuleGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRuleGroup where
        toJSON CreateRuleGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("MetricName" Core..= metricName),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRuleGroup where
        type Rs CreateRuleGroup = CreateRuleGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRuleGroupResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "RuleGroup" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , ruleGroup :: Core.Maybe Types.RuleGroup
    -- ^ An empty 'RuleGroup' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRuleGroupResponse' value with any optional fields omitted.
mkCreateRuleGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRuleGroupResponse
mkCreateRuleGroupResponse responseStatus
  = CreateRuleGroupResponse'{changeToken = Core.Nothing,
                             ruleGroup = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsChangeToken :: Lens.Lens' CreateRuleGroupResponse (Core.Maybe Types.ChangeToken)
crgrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crgrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An empty 'RuleGroup' .
--
-- /Note:/ Consider using 'ruleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsRuleGroup :: Lens.Lens' CreateRuleGroupResponse (Core.Maybe Types.RuleGroup)
crgrrsRuleGroup = Lens.field @"ruleGroup"
{-# INLINEABLE crgrrsRuleGroup #-}
{-# DEPRECATED ruleGroup "Use generic-lens or generic-optics with 'ruleGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsResponseStatus :: Lens.Lens' CreateRuleGroupResponse Core.Int
crgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
