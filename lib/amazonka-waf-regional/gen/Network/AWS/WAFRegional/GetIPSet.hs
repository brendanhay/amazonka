{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'IPSet' that is specified by @IPSetId@ .
module Network.AWS.WAFRegional.GetIPSet
  ( -- * Creating a request
    GetIPSet (..),
    mkGetIPSet,

    -- ** Request lenses
    gipsIPSetId,

    -- * Destructuring the response
    GetIPSetResponse (..),
    mkGetIPSetResponse,

    -- ** Response lenses
    gipsrrsIPSet,
    gipsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetIPSet' smart constructor.
newtype GetIPSet = GetIPSet'
  { -- | The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    iPSetId :: Types.IPSetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIPSet' value with any optional fields omitted.
mkGetIPSet ::
  -- | 'iPSetId'
  Types.IPSetId ->
  GetIPSet
mkGetIPSet iPSetId = GetIPSet' {iPSetId}

-- | The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'iPSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsIPSetId :: Lens.Lens' GetIPSet Types.IPSetId
gipsIPSetId = Lens.field @"iPSetId"
{-# DEPRECATED gipsIPSetId "Use generic-lens or generic-optics with 'iPSetId' instead." #-}

instance Core.FromJSON GetIPSet where
  toJSON GetIPSet {..} =
    Core.object
      (Core.catMaybes [Core.Just ("IPSetId" Core..= iPSetId)])

instance Core.AWSRequest GetIPSet where
  type Rs GetIPSet = GetIPSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.GetIPSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Core.<$> (x Core..:? "IPSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { -- | Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:
    --
    --
    --     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@
    --
    --
    --     * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
    iPSet :: Core.Maybe Types.IPSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIPSetResponse' value with any optional fields omitted.
mkGetIPSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIPSetResponse
mkGetIPSetResponse responseStatus =
  GetIPSetResponse' {iPSet = Core.Nothing, responseStatus}

-- | Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:
--
--
--     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@
--
--
--     * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
--
--
--
-- /Note:/ Consider using 'iPSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsIPSet :: Lens.Lens' GetIPSetResponse (Core.Maybe Types.IPSet)
gipsrrsIPSet = Lens.field @"iPSet"
{-# DEPRECATED gipsrrsIPSet "Use generic-lens or generic-optics with 'iPSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsResponseStatus :: Lens.Lens' GetIPSetResponse Core.Int
gipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
