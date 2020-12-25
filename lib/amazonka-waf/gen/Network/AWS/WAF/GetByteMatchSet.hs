{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'ByteMatchSet' specified by @ByteMatchSetId@ .
module Network.AWS.WAF.GetByteMatchSet
  ( -- * Creating a request
    GetByteMatchSet (..),
    mkGetByteMatchSet,

    -- ** Request lenses
    gbmsByteMatchSetId,

    -- * Destructuring the response
    GetByteMatchSetResponse (..),
    mkGetByteMatchSetResponse,

    -- ** Response lenses
    gbmsrrsByteMatchSet,
    gbmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetByteMatchSet' smart constructor.
newtype GetByteMatchSet = GetByteMatchSet'
  { -- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
    byteMatchSetId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetByteMatchSet' value with any optional fields omitted.
mkGetByteMatchSet ::
  -- | 'byteMatchSetId'
  Types.ResourceId ->
  GetByteMatchSet
mkGetByteMatchSet byteMatchSetId = GetByteMatchSet' {byteMatchSetId}

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsByteMatchSetId :: Lens.Lens' GetByteMatchSet Types.ResourceId
gbmsByteMatchSetId = Lens.field @"byteMatchSetId"
{-# DEPRECATED gbmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

instance Core.FromJSON GetByteMatchSet where
  toJSON GetByteMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ByteMatchSetId" Core..= byteMatchSetId)]
      )

instance Core.AWSRequest GetByteMatchSet where
  type Rs GetByteMatchSet = GetByteMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetByteMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetByteMatchSetResponse'
            Core.<$> (x Core..:? "ByteMatchSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetByteMatchSetResponse' smart constructor.
data GetByteMatchSetResponse = GetByteMatchSetResponse'
  { -- | Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:
    --
    --
    --     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@
    --
    --
    --     * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    byteMatchSet :: Core.Maybe Types.ByteMatchSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetByteMatchSetResponse' value with any optional fields omitted.
mkGetByteMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetByteMatchSetResponse
mkGetByteMatchSetResponse responseStatus =
  GetByteMatchSetResponse'
    { byteMatchSet = Core.Nothing,
      responseStatus
    }

-- | Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:
--
--
--     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@
--
--
--     * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'byteMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsrrsByteMatchSet :: Lens.Lens' GetByteMatchSetResponse (Core.Maybe Types.ByteMatchSet)
gbmsrrsByteMatchSet = Lens.field @"byteMatchSet"
{-# DEPRECATED gbmsrrsByteMatchSet "Use generic-lens or generic-optics with 'byteMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsrrsResponseStatus :: Lens.Lens' GetByteMatchSetResponse Core.Int
gbmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
