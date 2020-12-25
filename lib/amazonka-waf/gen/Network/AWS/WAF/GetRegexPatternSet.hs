{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexPatternSet' specified by @RegexPatternSetId@ .
module Network.AWS.WAF.GetRegexPatternSet
  ( -- * Creating a request
    GetRegexPatternSet (..),
    mkGetRegexPatternSet,

    -- ** Request lenses
    grpsRegexPatternSetId,

    -- * Destructuring the response
    GetRegexPatternSetResponse (..),
    mkGetRegexPatternSetResponse,

    -- ** Response lenses
    grpsrrsRegexPatternSet,
    grpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRegexPatternSet' smart constructor.
newtype GetRegexPatternSet = GetRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
    regexPatternSetId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexPatternSet' value with any optional fields omitted.
mkGetRegexPatternSet ::
  -- | 'regexPatternSetId'
  Types.ResourceId ->
  GetRegexPatternSet
mkGetRegexPatternSet regexPatternSetId =
  GetRegexPatternSet' {regexPatternSetId}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsRegexPatternSetId :: Lens.Lens' GetRegexPatternSet Types.ResourceId
grpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# DEPRECATED grpsRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

instance Core.FromJSON GetRegexPatternSet where
  toJSON GetRegexPatternSet {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RegexPatternSetId" Core..= regexPatternSetId)]
      )

instance Core.AWSRequest GetRegexPatternSet where
  type Rs GetRegexPatternSet = GetRegexPatternSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetRegexPatternSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexPatternSetResponse'
            Core.<$> (x Core..:? "RegexPatternSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { -- | Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for.
    regexPatternSet :: Core.Maybe Types.RegexPatternSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexPatternSetResponse' value with any optional fields omitted.
mkGetRegexPatternSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRegexPatternSetResponse
mkGetRegexPatternSetResponse responseStatus =
  GetRegexPatternSetResponse'
    { regexPatternSet = Core.Nothing,
      responseStatus
    }

-- | Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for.
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrrsRegexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Core.Maybe Types.RegexPatternSet)
grpsrrsRegexPatternSet = Lens.field @"regexPatternSet"
{-# DEPRECATED grpsrrsRegexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrrsResponseStatus :: Lens.Lens' GetRegexPatternSetResponse Core.Int
grpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
