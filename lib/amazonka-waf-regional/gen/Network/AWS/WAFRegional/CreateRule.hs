{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Rule@ , which contains the @IPSet@ objects, @ByteMatchSet@ objects, and other predicates that identify the requests that you want to block. If you add more than one predicate to a @Rule@ , a request must match all of the specifications to be allowed or blocked. For example, suppose that you add the following to a @Rule@ :
--
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@ 
--
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to blocks requests that satisfy the @Rule@ . For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ .
-- To create and configure a @Rule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the @Rule@ . For more information, see 'CreateByteMatchSet' , 'CreateIPSet' , and 'CreateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRule@ request.
--
--
--     * Submit a @CreateRule@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--
--     * Submit an @UpdateRule@ request to specify the predicates that you want to include in the @Rule@ .
--
--
--     * Create and update a @WebACL@ that contains the @Rule@ . For more information, see 'CreateWebACL' .
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRule
    (
    -- * Creating a request
      CreateRule (..)
    , mkCreateRule
    -- ** Request lenses
    , crName
    , crMetricName
    , crChangeToken
    , crTags

    -- * Destructuring the response
    , CreateRuleResponse (..)
    , mkCreateRuleResponse
    -- ** Response lenses
    , crrrsChangeToken
    , crrrsRule
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateRule' smart constructor.
data CreateRule = CreateRule'
  { name :: Types.ResourceName
    -- ^ A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
  , metricName :: Types.MetricName
    -- ^ A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @Rule@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRule' value with any optional fields omitted.
mkCreateRule
    :: Types.ResourceName -- ^ 'name'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateRule
mkCreateRule name metricName changeToken
  = CreateRule'{name, metricName, changeToken, tags = Core.Nothing}

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CreateRule Types.ResourceName
crName = Lens.field @"name"
{-# INLINEABLE crName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @Rule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMetricName :: Lens.Lens' CreateRule Types.MetricName
crMetricName = Lens.field @"metricName"
{-# INLINEABLE crMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crChangeToken :: Lens.Lens' CreateRule Types.ChangeToken
crChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRule (Core.Maybe (Core.NonEmpty Types.Tag))
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRule where
        toHeaders CreateRule{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRule where
        toJSON CreateRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("MetricName" Core..= metricName),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRule where
        type Rs CreateRule = CreateRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRuleResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "Rule" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , rule :: Core.Maybe Types.Rule
    -- ^ The 'Rule' returned in the @CreateRule@ response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRuleResponse' value with any optional fields omitted.
mkCreateRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRuleResponse
mkCreateRuleResponse responseStatus
  = CreateRuleResponse'{changeToken = Core.Nothing,
                        rule = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsChangeToken :: Lens.Lens' CreateRuleResponse (Core.Maybe Types.ChangeToken)
crrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The 'Rule' returned in the @CreateRule@ response.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRule :: Lens.Lens' CreateRuleResponse (Core.Maybe Types.Rule)
crrrsRule = Lens.field @"rule"
{-# INLINEABLE crrrsRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRuleResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
