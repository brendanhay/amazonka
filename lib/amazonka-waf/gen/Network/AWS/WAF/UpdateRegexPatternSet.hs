{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes @RegexPatternString@ objects in a 'RegexPatternSet' . For each @RegexPatternString@ object, you specify the following values: 
--
--
--     * Whether to insert or delete the @RegexPatternString@ .
--
--
--     * The regular expression pattern that you want to insert or delete. For more information, see 'RegexPatternSet' . 
--
--
-- For example, you can create a @RegexPatternString@ such as @B[a@]dB[o0]t@ . AWS WAF will match this @RegexPatternString@ to:
--
--     * BadBot
--
--
--     * BadB0t
--
--
--     * B@dBot
--
--
--     * B@dB0t
--
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Create a @RegexPatternSet.@ For more information, see 'CreateRegexPatternSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--
--     * Submit an @UpdateRegexPatternSet@ request to specify the regular expression pattern that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateRegexPatternSet
    (
    -- * Creating a request
      UpdateRegexPatternSet (..)
    , mkUpdateRegexPatternSet
    -- ** Request lenses
    , urpsRegexPatternSetId
    , urpsUpdates
    , urpsChangeToken

    -- * Destructuring the response
    , UpdateRegexPatternSetResponse (..)
    , mkUpdateRegexPatternSetResponse
    -- ** Response lenses
    , urpsrrsChangeToken
    , urpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { regexPatternSetId :: Types.ResourceId
    -- ^ The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
  , updates :: Core.NonEmpty Types.RegexPatternSetUpdate
    -- ^ An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegexPatternSet' value with any optional fields omitted.
mkUpdateRegexPatternSet
    :: Types.ResourceId -- ^ 'regexPatternSetId'
    -> Core.NonEmpty Types.RegexPatternSetUpdate -- ^ 'updates'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> UpdateRegexPatternSet
mkUpdateRegexPatternSet regexPatternSetId updates changeToken
  = UpdateRegexPatternSet'{regexPatternSetId, updates, changeToken}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsRegexPatternSetId :: Lens.Lens' UpdateRegexPatternSet Types.ResourceId
urpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# INLINEABLE urpsRegexPatternSetId #-}
{-# DEPRECATED regexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead"  #-}

-- | An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsUpdates :: Lens.Lens' UpdateRegexPatternSet (Core.NonEmpty Types.RegexPatternSetUpdate)
urpsUpdates = Lens.field @"updates"
{-# INLINEABLE urpsUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsChangeToken :: Lens.Lens' UpdateRegexPatternSet Types.ChangeToken
urpsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urpsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery UpdateRegexPatternSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRegexPatternSet where
        toHeaders UpdateRegexPatternSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.UpdateRegexPatternSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRegexPatternSet where
        toJSON UpdateRegexPatternSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegexPatternSetId" Core..= regexPatternSetId),
                  Core.Just ("Updates" Core..= updates),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest UpdateRegexPatternSet where
        type Rs UpdateRegexPatternSet = UpdateRegexPatternSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRegexPatternSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegexPatternSetResponse' value with any optional fields omitted.
mkUpdateRegexPatternSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRegexPatternSetResponse
mkUpdateRegexPatternSetResponse responseStatus
  = UpdateRegexPatternSetResponse'{changeToken = Core.Nothing,
                                   responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsrrsChangeToken :: Lens.Lens' UpdateRegexPatternSetResponse (Core.Maybe Types.ChangeToken)
urpsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urpsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsrrsResponseStatus :: Lens.Lens' UpdateRegexPatternSetResponse Core.Int
urpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
