{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListPublicKeys
    (
    -- * Creating a request
      ListPublicKeys (..)
    , mkListPublicKeys
    -- ** Request lenses
    , lpkEndTime
    , lpkNextToken
    , lpkStartTime

    -- * Destructuring the response
    , ListPublicKeysResponse (..)
    , mkListPublicKeysResponse
    -- ** Response lenses
    , lpkrrsNextToken
    , lpkrrsPublicKeyList
    , lpkrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests the public keys for a specified time range.
--
-- /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Reserved for future use.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPublicKeys' value with any optional fields omitted.
mkListPublicKeys
    :: ListPublicKeys
mkListPublicKeys
  = ListPublicKeys'{endTime = Core.Nothing, nextToken = Core.Nothing,
                    startTime = Core.Nothing}

-- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkEndTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.NominalDiffTime)
lpkEndTime = Lens.field @"endTime"
{-# INLINEABLE lpkEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkNextToken :: Lens.Lens' ListPublicKeys (Core.Maybe Core.Text)
lpkNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpkNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkStartTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.NominalDiffTime)
lpkStartTime = Lens.field @"startTime"
{-# INLINEABLE lpkStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery ListPublicKeys where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPublicKeys where
        toHeaders ListPublicKeys{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPublicKeys where
        toJSON ListPublicKeys{..}
          = Core.object
              (Core.catMaybes
                 [("EndTime" Core..=) Core.<$> endTime,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("StartTime" Core..=) Core.<$> startTime])

instance Core.AWSRequest ListPublicKeys where
        type Rs ListPublicKeys = ListPublicKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPublicKeysResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PublicKeyList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPublicKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"publicKeyList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Reserved for future use.
  , publicKeyList :: Core.Maybe [Types.PublicKey]
    -- ^ Contains an array of PublicKey objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPublicKeysResponse' value with any optional fields omitted.
mkListPublicKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPublicKeysResponse
mkListPublicKeysResponse responseStatus
  = ListPublicKeysResponse'{nextToken = Core.Nothing,
                            publicKeyList = Core.Nothing, responseStatus}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsNextToken :: Lens.Lens' ListPublicKeysResponse (Core.Maybe Core.Text)
lpkrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpkrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Contains an array of PublicKey objects.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Core.Maybe [Types.PublicKey])
lpkrrsPublicKeyList = Lens.field @"publicKeyList"
{-# INLINEABLE lpkrrsPublicKeyList #-}
{-# DEPRECATED publicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Core.Int
lpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
