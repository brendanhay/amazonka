{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @WebACL@ , which contains the @Rules@ that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates @Rules@ in order based on the value of @Priority@ for each @Rule@ .
--
-- You also specify a default action, either @ALLOW@ or @BLOCK@ . If a web request doesn't match any of the @Rules@ in a @WebACL@ , AWS WAF responds to the request with the default action. 
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the @ByteMatchSet@ objects and other predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateWebACL@ request.
--
--
--     * Submit a @CreateWebACL@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--
--     * Submit an 'UpdateWebACL' request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
--
--
-- For more information about how to use the AWS WAF API, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateWebACL
    (
    -- * Creating a request
      CreateWebACL (..)
    , mkCreateWebACL
    -- ** Request lenses
    , cwaclName
    , cwaclMetricName
    , cwaclDefaultAction
    , cwaclChangeToken
    , cwaclTags

    -- * Destructuring the response
    , CreateWebACLResponse (..)
    , mkCreateWebACLResponse
    -- ** Response lenses
    , cwaclrrsChangeToken
    , cwaclrrsWebACL
    , cwaclrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateWebACL' smart constructor.
data CreateWebACL = CreateWebACL'
  { name :: Types.ResourceName
    -- ^ A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
  , metricName :: Types.MetricName
    -- ^ A friendly name or description for the metrics for this @WebACL@ .The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
  , defaultAction :: Types.WafAction
    -- ^ The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWebACL' value with any optional fields omitted.
mkCreateWebACL
    :: Types.ResourceName -- ^ 'name'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.WafAction -- ^ 'defaultAction'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateWebACL
mkCreateWebACL name metricName defaultAction changeToken
  = CreateWebACL'{name, metricName, defaultAction, changeToken,
                  tags = Core.Nothing}

-- | A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclName :: Lens.Lens' CreateWebACL Types.ResourceName
cwaclName = Lens.field @"name"
{-# INLINEABLE cwaclName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A friendly name or description for the metrics for this @WebACL@ .The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclMetricName :: Lens.Lens' CreateWebACL Types.MetricName
cwaclMetricName = Lens.field @"metricName"
{-# INLINEABLE cwaclMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclDefaultAction :: Lens.Lens' CreateWebACL Types.WafAction
cwaclDefaultAction = Lens.field @"defaultAction"
{-# INLINEABLE cwaclDefaultAction #-}
{-# DEPRECATED defaultAction "Use generic-lens or generic-optics with 'defaultAction' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclChangeToken :: Lens.Lens' CreateWebACL Types.ChangeToken
cwaclChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cwaclChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclTags :: Lens.Lens' CreateWebACL (Core.Maybe (Core.NonEmpty Types.Tag))
cwaclTags = Lens.field @"tags"
{-# INLINEABLE cwaclTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateWebACL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateWebACL where
        toHeaders CreateWebACL{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.CreateWebACL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateWebACL where
        toJSON CreateWebACL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("MetricName" Core..= metricName),
                  Core.Just ("DefaultAction" Core..= defaultAction),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateWebACL where
        type Rs CreateWebACL = CreateWebACLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateWebACLResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "WebACL" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateWebACLResponse' smart constructor.
data CreateWebACLResponse = CreateWebACLResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , webACL :: Core.Maybe Types.WebACL
    -- ^ The 'WebACL' returned in the @CreateWebACL@ response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWebACLResponse' value with any optional fields omitted.
mkCreateWebACLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateWebACLResponse
mkCreateWebACLResponse responseStatus
  = CreateWebACLResponse'{changeToken = Core.Nothing,
                          webACL = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclrrsChangeToken :: Lens.Lens' CreateWebACLResponse (Core.Maybe Types.ChangeToken)
cwaclrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cwaclrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The 'WebACL' returned in the @CreateWebACL@ response.
--
-- /Note:/ Consider using 'webACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclrrsWebACL :: Lens.Lens' CreateWebACLResponse (Core.Maybe Types.WebACL)
cwaclrrsWebACL = Lens.field @"webACL"
{-# INLINEABLE cwaclrrsWebACL #-}
{-# DEPRECATED webACL "Use generic-lens or generic-optics with 'webACL' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclrrsResponseStatus :: Lens.Lens' CreateWebACLResponse Core.Int
cwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cwaclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
