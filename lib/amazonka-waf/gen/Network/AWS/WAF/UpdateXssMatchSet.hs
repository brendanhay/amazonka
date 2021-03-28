{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateXssMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'XssMatchTuple' objects (filters) in an 'XssMatchSet' . For each @XssMatchTuple@ object, you specify the following values:
--
--
--     * @Action@ : Whether to insert the object into or delete the object from the array. To change an @XssMatchTuple@ , you delete the existing object and add a new one.
--
--
--     * @FieldToMatch@ : The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header or custom query parameter, the name of the header or parameter.
--
--
--     * @TextTransformation@ : Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.
-- You can only specify a single type of TextTransformation.
--
--
-- You use @XssMatchSet@ objects to specify which CloudFront requests that you want to allow, block, or count. For example, if you're receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an @XssMatchSet@ with the applicable settings, and then configure AWS WAF to block the requests. 
-- To create and configure an @XssMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateXssMatchSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateXssMatchSet@ request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateXssMatchSet
    (
    -- * Creating a request
      UpdateXssMatchSet (..)
    , mkUpdateXssMatchSet
    -- ** Request lenses
    , uxmsXssMatchSetId
    , uxmsChangeToken
    , uxmsUpdates

    -- * Destructuring the response
    , UpdateXssMatchSetResponse (..)
    , mkUpdateXssMatchSetResponse
    -- ** Response lenses
    , uxmsrrsChangeToken
    , uxmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to update an 'XssMatchSet' .
--
-- /See:/ 'mkUpdateXssMatchSet' smart constructor.
data UpdateXssMatchSet = UpdateXssMatchSet'
  { xssMatchSetId :: Types.ResourceId
    -- ^ The @XssMatchSetId@ of the @XssMatchSet@ that you want to update. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: Core.NonEmpty Types.XssMatchSetUpdate
    -- ^ An array of @XssMatchSetUpdate@ objects that you want to insert into or delete from an 'XssMatchSet' . For more information, see the applicable data types:
--
--
--     * 'XssMatchSetUpdate' : Contains @Action@ and @XssMatchTuple@ 
--
--
--     * 'XssMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateXssMatchSet' value with any optional fields omitted.
mkUpdateXssMatchSet
    :: Types.ResourceId -- ^ 'xssMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> Core.NonEmpty Types.XssMatchSetUpdate -- ^ 'updates'
    -> UpdateXssMatchSet
mkUpdateXssMatchSet xssMatchSetId changeToken updates
  = UpdateXssMatchSet'{xssMatchSetId, changeToken, updates}

-- | The @XssMatchSetId@ of the @XssMatchSet@ that you want to update. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsXssMatchSetId :: Lens.Lens' UpdateXssMatchSet Types.ResourceId
uxmsXssMatchSetId = Lens.field @"xssMatchSetId"
{-# INLINEABLE uxmsXssMatchSetId #-}
{-# DEPRECATED xssMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsChangeToken :: Lens.Lens' UpdateXssMatchSet Types.ChangeToken
uxmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uxmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @XssMatchSetUpdate@ objects that you want to insert into or delete from an 'XssMatchSet' . For more information, see the applicable data types:
--
--
--     * 'XssMatchSetUpdate' : Contains @Action@ and @XssMatchTuple@ 
--
--
--     * 'XssMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsUpdates :: Lens.Lens' UpdateXssMatchSet (Core.NonEmpty Types.XssMatchSetUpdate)
uxmsUpdates = Lens.field @"updates"
{-# INLINEABLE uxmsUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

instance Core.ToQuery UpdateXssMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateXssMatchSet where
        toHeaders UpdateXssMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateXssMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateXssMatchSet where
        toJSON UpdateXssMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("XssMatchSetId" Core..= xssMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates)])

instance Core.AWSRequest UpdateXssMatchSet where
        type Rs UpdateXssMatchSet = UpdateXssMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateXssMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to an 'UpdateXssMatchSets' request.
--
-- /See:/ 'mkUpdateXssMatchSetResponse' smart constructor.
data UpdateXssMatchSetResponse = UpdateXssMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateXssMatchSetResponse' value with any optional fields omitted.
mkUpdateXssMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateXssMatchSetResponse
mkUpdateXssMatchSetResponse responseStatus
  = UpdateXssMatchSetResponse'{changeToken = Core.Nothing,
                               responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsrrsChangeToken :: Lens.Lens' UpdateXssMatchSetResponse (Core.Maybe Types.ChangeToken)
uxmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uxmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsrrsResponseStatus :: Lens.Lens' UpdateXssMatchSetResponse Core.Int
uxmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uxmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
