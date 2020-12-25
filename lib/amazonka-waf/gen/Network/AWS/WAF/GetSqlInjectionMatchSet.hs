{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'SqlInjectionMatchSet' that is specified by @SqlInjectionMatchSetId@ .
module Network.AWS.WAF.GetSqlInjectionMatchSet
  ( -- * Creating a request
    GetSqlInjectionMatchSet (..),
    mkGetSqlInjectionMatchSet,

    -- ** Request lenses
    gsimsSqlInjectionMatchSetId,

    -- * Destructuring the response
    GetSqlInjectionMatchSetResponse (..),
    mkGetSqlInjectionMatchSetResponse,

    -- ** Response lenses
    gsimsrrsSqlInjectionMatchSet,
    gsimsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to get a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkGetSqlInjectionMatchSet' smart constructor.
newtype GetSqlInjectionMatchSet = GetSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Types.SqlInjectionMatchSetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSqlInjectionMatchSet' value with any optional fields omitted.
mkGetSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Types.SqlInjectionMatchSetId ->
  GetSqlInjectionMatchSet
mkGetSqlInjectionMatchSet sqlInjectionMatchSetId =
  GetSqlInjectionMatchSet' {sqlInjectionMatchSetId}

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsSqlInjectionMatchSetId :: Lens.Lens' GetSqlInjectionMatchSet Types.SqlInjectionMatchSetId
gsimsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# DEPRECATED gsimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

instance Core.FromJSON GetSqlInjectionMatchSet where
  toJSON GetSqlInjectionMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SqlInjectionMatchSetId" Core..= sqlInjectionMatchSetId)
          ]
      )

instance Core.AWSRequest GetSqlInjectionMatchSet where
  type Rs GetSqlInjectionMatchSet = GetSqlInjectionMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.GetSqlInjectionMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSqlInjectionMatchSetResponse'
            Core.<$> (x Core..:? "SqlInjectionMatchSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to a 'GetSqlInjectionMatchSet' request.
--
-- /See:/ 'mkGetSqlInjectionMatchSetResponse' smart constructor.
data GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse'
  { -- | Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
    --
    --
    --     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
    --
    --
    --     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    sqlInjectionMatchSet :: Core.Maybe Types.SqlInjectionMatchSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkGetSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSqlInjectionMatchSetResponse
mkGetSqlInjectionMatchSetResponse responseStatus =
  GetSqlInjectionMatchSetResponse'
    { sqlInjectionMatchSet =
        Core.Nothing,
      responseStatus
    }

-- | Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
--
--
--     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
--
--
--     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'sqlInjectionMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrrsSqlInjectionMatchSet :: Lens.Lens' GetSqlInjectionMatchSetResponse (Core.Maybe Types.SqlInjectionMatchSet)
gsimsrrsSqlInjectionMatchSet = Lens.field @"sqlInjectionMatchSet"
{-# DEPRECATED gsimsrrsSqlInjectionMatchSet "Use generic-lens or generic-optics with 'sqlInjectionMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrrsResponseStatus :: Lens.Lens' GetSqlInjectionMatchSetResponse Core.Int
gsimsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsimsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
