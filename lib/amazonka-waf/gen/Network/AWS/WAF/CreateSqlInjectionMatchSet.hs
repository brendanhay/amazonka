{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'SqlInjectionMatchSet' , which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.
--
-- To create and configure a @SqlInjectionMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSqlInjectionMatchSet@ request.
--
--
--     * Submit a @CreateSqlInjectionMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateSqlInjectionMatchSet' request.
--
--
--     * Submit an 'UpdateSqlInjectionMatchSet' request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateSqlInjectionMatchSet
    (
    -- * Creating a request
      CreateSqlInjectionMatchSet (..)
    , mkCreateSqlInjectionMatchSet
    -- ** Request lenses
    , csimsName
    , csimsChangeToken

    -- * Destructuring the response
    , CreateSqlInjectionMatchSetResponse (..)
    , mkCreateSqlInjectionMatchSetResponse
    -- ** Response lenses
    , csimsrrsChangeToken
    , csimsrrsSqlInjectionMatchSet
    , csimsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to create a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkCreateSqlInjectionMatchSet' smart constructor.
data CreateSqlInjectionMatchSet = CreateSqlInjectionMatchSet'
  { name :: Types.Name
    -- ^ A friendly name or description for the 'SqlInjectionMatchSet' that you're creating. You can't change @Name@ after you create the @SqlInjectionMatchSet@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSqlInjectionMatchSet' value with any optional fields omitted.
mkCreateSqlInjectionMatchSet
    :: Types.Name -- ^ 'name'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateSqlInjectionMatchSet
mkCreateSqlInjectionMatchSet name changeToken
  = CreateSqlInjectionMatchSet'{name, changeToken}

-- | A friendly name or description for the 'SqlInjectionMatchSet' that you're creating. You can't change @Name@ after you create the @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsName :: Lens.Lens' CreateSqlInjectionMatchSet Types.Name
csimsName = Lens.field @"name"
{-# INLINEABLE csimsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsChangeToken :: Lens.Lens' CreateSqlInjectionMatchSet Types.ChangeToken
csimsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE csimsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery CreateSqlInjectionMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSqlInjectionMatchSet where
        toHeaders CreateSqlInjectionMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.CreateSqlInjectionMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSqlInjectionMatchSet where
        toJSON CreateSqlInjectionMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest CreateSqlInjectionMatchSet where
        type Rs CreateSqlInjectionMatchSet =
             CreateSqlInjectionMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSqlInjectionMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*>
                     x Core..:? "SqlInjectionMatchSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a @CreateSqlInjectionMatchSet@ request.
--
-- /See:/ 'mkCreateSqlInjectionMatchSetResponse' smart constructor.
data CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , sqlInjectionMatchSet :: Core.Maybe Types.SqlInjectionMatchSet
    -- ^ A 'SqlInjectionMatchSet' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkCreateSqlInjectionMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSqlInjectionMatchSetResponse
mkCreateSqlInjectionMatchSetResponse responseStatus
  = CreateSqlInjectionMatchSetResponse'{changeToken = Core.Nothing,
                                        sqlInjectionMatchSet = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrrsChangeToken :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Core.Maybe Types.ChangeToken)
csimsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE csimsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | A 'SqlInjectionMatchSet' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrrsSqlInjectionMatchSet :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Core.Maybe Types.SqlInjectionMatchSet)
csimsrrsSqlInjectionMatchSet = Lens.field @"sqlInjectionMatchSet"
{-# INLINEABLE csimsrrsSqlInjectionMatchSet #-}
{-# DEPRECATED sqlInjectionMatchSet "Use generic-lens or generic-optics with 'sqlInjectionMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrrsResponseStatus :: Lens.Lens' CreateSqlInjectionMatchSetResponse Core.Int
csimsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csimsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
