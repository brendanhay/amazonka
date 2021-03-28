{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ByteMatchTuple' objects (filters) in a 'ByteMatchSet' . For each @ByteMatchTuple@ object, you specify the following values: 
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @ByteMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header. 
--
--
--     * The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see @TargetString@ in the 'ByteMatchTuple' data type. 
--
--
--     * Where to look, such as at the beginning or the end of a query string.
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
-- For example, you can add a @ByteMatchSetUpdate@ object that matches web requests in which @User-Agent@ headers contain the string @BadBot@ . You can then configure AWS WAF to block those requests.
-- To create and configure a @ByteMatchSet@ , perform the following steps:
--
--     * Create a @ByteMatchSet.@ For more information, see 'CreateByteMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
--
--     * Submit an @UpdateByteMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateByteMatchSet
    (
    -- * Creating a request
      UpdateByteMatchSet (..)
    , mkUpdateByteMatchSet
    -- ** Request lenses
    , ubmsByteMatchSetId
    , ubmsChangeToken
    , ubmsUpdates

    -- * Destructuring the response
    , UpdateByteMatchSetResponse (..)
    , mkUpdateByteMatchSetResponse
    -- ** Response lenses
    , ubmsrrsChangeToken
    , ubmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkUpdateByteMatchSet' smart constructor.
data UpdateByteMatchSet = UpdateByteMatchSet'
  { byteMatchSetId :: Types.ResourceId
    -- ^ The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: Core.NonEmpty Types.ByteMatchSetUpdate
    -- ^ An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:
--
--
--     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@ 
--
--
--     * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateByteMatchSet' value with any optional fields omitted.
mkUpdateByteMatchSet
    :: Types.ResourceId -- ^ 'byteMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> Core.NonEmpty Types.ByteMatchSetUpdate -- ^ 'updates'
    -> UpdateByteMatchSet
mkUpdateByteMatchSet byteMatchSetId changeToken updates
  = UpdateByteMatchSet'{byteMatchSetId, changeToken, updates}

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsByteMatchSetId :: Lens.Lens' UpdateByteMatchSet Types.ResourceId
ubmsByteMatchSetId = Lens.field @"byteMatchSetId"
{-# INLINEABLE ubmsByteMatchSetId #-}
{-# DEPRECATED byteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsChangeToken :: Lens.Lens' UpdateByteMatchSet Types.ChangeToken
ubmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE ubmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:
--
--
--     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@ 
--
--
--     * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsUpdates :: Lens.Lens' UpdateByteMatchSet (Core.NonEmpty Types.ByteMatchSetUpdate)
ubmsUpdates = Lens.field @"updates"
{-# INLINEABLE ubmsUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

instance Core.ToQuery UpdateByteMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateByteMatchSet where
        toHeaders UpdateByteMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.UpdateByteMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateByteMatchSet where
        toJSON UpdateByteMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ByteMatchSetId" Core..= byteMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates)])

instance Core.AWSRequest UpdateByteMatchSet where
        type Rs UpdateByteMatchSet = UpdateByteMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateByteMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateByteMatchSetResponse' smart constructor.
data UpdateByteMatchSetResponse = UpdateByteMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateByteMatchSetResponse' value with any optional fields omitted.
mkUpdateByteMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateByteMatchSetResponse
mkUpdateByteMatchSetResponse responseStatus
  = UpdateByteMatchSetResponse'{changeToken = Core.Nothing,
                                responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsrrsChangeToken :: Lens.Lens' UpdateByteMatchSetResponse (Core.Maybe Types.ChangeToken)
ubmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE ubmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsrrsResponseStatus :: Lens.Lens' UpdateByteMatchSetResponse Core.Int
ubmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
