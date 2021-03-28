{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'SizeConstraint' objects (filters) in a 'SizeConstraintSet' . For each @SizeConstraint@ object, you specify the following values: 
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @SizeConstraintSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the @User-Agent@ header.
--
--
--     * Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first @8192@ bytes of your request to AWS WAF.
-- You can only specify a single type of TextTransformation.
--
--
--     * A @ComparisonOperator@ used for evaluating the selected part of the request against the specified @Size@ , such as equals, greater than, less than, and so on.
--
--
--     * The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.
--
--
-- For example, you can add a @SizeConstraintSetUpdate@ object that matches web requests in which the length of the @User-Agent@ header is greater than 100 bytes. You can then configure AWS WAF to block those requests.
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Create a @SizeConstraintSet.@ For more information, see 'CreateSizeConstraintSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--
--     * Submit an @UpdateSizeConstraintSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateSizeConstraintSet
    (
    -- * Creating a request
      UpdateSizeConstraintSet (..)
    , mkUpdateSizeConstraintSet
    -- ** Request lenses
    , uscsSizeConstraintSetId
    , uscsChangeToken
    , uscsUpdates

    -- * Destructuring the response
    , UpdateSizeConstraintSetResponse (..)
    , mkUpdateSizeConstraintSetResponse
    -- ** Response lenses
    , uscsrrsChangeToken
    , uscsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkUpdateSizeConstraintSet' smart constructor.
data UpdateSizeConstraintSet = UpdateSizeConstraintSet'
  { sizeConstraintSetId :: Types.ResourceId
    -- ^ The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to update. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: Core.NonEmpty Types.SizeConstraintSetUpdate
    -- ^ An array of @SizeConstraintSetUpdate@ objects that you want to insert into or delete from a 'SizeConstraintSet' . For more information, see the applicable data types:
--
--
--     * 'SizeConstraintSetUpdate' : Contains @Action@ and @SizeConstraint@ 
--
--
--     * 'SizeConstraint' : Contains @FieldToMatch@ , @TextTransformation@ , @ComparisonOperator@ , and @Size@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSizeConstraintSet' value with any optional fields omitted.
mkUpdateSizeConstraintSet
    :: Types.ResourceId -- ^ 'sizeConstraintSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> Core.NonEmpty Types.SizeConstraintSetUpdate -- ^ 'updates'
    -> UpdateSizeConstraintSet
mkUpdateSizeConstraintSet sizeConstraintSetId changeToken updates
  = UpdateSizeConstraintSet'{sizeConstraintSetId, changeToken,
                             updates}

-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to update. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsSizeConstraintSetId :: Lens.Lens' UpdateSizeConstraintSet Types.ResourceId
uscsSizeConstraintSetId = Lens.field @"sizeConstraintSetId"
{-# INLINEABLE uscsSizeConstraintSetId #-}
{-# DEPRECATED sizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsChangeToken :: Lens.Lens' UpdateSizeConstraintSet Types.ChangeToken
uscsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uscsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @SizeConstraintSetUpdate@ objects that you want to insert into or delete from a 'SizeConstraintSet' . For more information, see the applicable data types:
--
--
--     * 'SizeConstraintSetUpdate' : Contains @Action@ and @SizeConstraint@ 
--
--
--     * 'SizeConstraint' : Contains @FieldToMatch@ , @TextTransformation@ , @ComparisonOperator@ , and @Size@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsUpdates :: Lens.Lens' UpdateSizeConstraintSet (Core.NonEmpty Types.SizeConstraintSetUpdate)
uscsUpdates = Lens.field @"updates"
{-# INLINEABLE uscsUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

instance Core.ToQuery UpdateSizeConstraintSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSizeConstraintSet where
        toHeaders UpdateSizeConstraintSet{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSWAF_Regional_20161128.UpdateSizeConstraintSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSizeConstraintSet where
        toJSON UpdateSizeConstraintSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SizeConstraintSetId" Core..= sizeConstraintSetId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates)])

instance Core.AWSRequest UpdateSizeConstraintSet where
        type Rs UpdateSizeConstraintSet = UpdateSizeConstraintSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSizeConstraintSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSizeConstraintSetResponse' smart constructor.
data UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSizeConstraintSetResponse' value with any optional fields omitted.
mkUpdateSizeConstraintSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSizeConstraintSetResponse
mkUpdateSizeConstraintSetResponse responseStatus
  = UpdateSizeConstraintSetResponse'{changeToken = Core.Nothing,
                                     responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsrrsChangeToken :: Lens.Lens' UpdateSizeConstraintSetResponse (Core.Maybe Types.ChangeToken)
uscsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uscsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsrrsResponseStatus :: Lens.Lens' UpdateSizeConstraintSetResponse Core.Int
uscsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uscsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
