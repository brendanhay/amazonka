{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of upgrade compatible Elastisearch versions. You can optionally pass a @'DomainName' @ to get all upgrade compatible Elasticsearch versions for that specific domain.
module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
  ( -- * Creating a request
    GetCompatibleElasticsearchVersions (..),
    mkGetCompatibleElasticsearchVersions,

    -- ** Request lenses
    gcevDomainName,

    -- * Destructuring the response
    GetCompatibleElasticsearchVersionsResponse (..),
    mkGetCompatibleElasticsearchVersionsResponse,

    -- ** Response lenses
    gcevrrsCompatibleElasticsearchVersions,
    gcevrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /See:/ 'mkGetCompatibleElasticsearchVersions' smart constructor.
newtype GetCompatibleElasticsearchVersions = GetCompatibleElasticsearchVersions'
  { domainName :: Core.Maybe Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCompatibleElasticsearchVersions' value with any optional fields omitted.
mkGetCompatibleElasticsearchVersions ::
  GetCompatibleElasticsearchVersions
mkGetCompatibleElasticsearchVersions =
  GetCompatibleElasticsearchVersions' {domainName = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevDomainName :: Lens.Lens' GetCompatibleElasticsearchVersions (Core.Maybe Types.DomainName)
gcevDomainName = Lens.field @"domainName"
{-# DEPRECATED gcevDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest GetCompatibleElasticsearchVersions where
  type
    Rs GetCompatibleElasticsearchVersions =
      GetCompatibleElasticsearchVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2015-01-01/es/compatibleVersions",
        Core._rqQuery = Core.toQueryValue "domainName" Core.<$> domainName,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCompatibleElasticsearchVersionsResponse'
            Core.<$> (x Core..:? "CompatibleElasticsearchVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /See:/ 'mkGetCompatibleElasticsearchVersionsResponse' smart constructor.
data GetCompatibleElasticsearchVersionsResponse = GetCompatibleElasticsearchVersionsResponse'
  { -- | A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
    compatibleElasticsearchVersions :: Core.Maybe [Types.CompatibleVersionsMap],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCompatibleElasticsearchVersionsResponse' value with any optional fields omitted.
mkGetCompatibleElasticsearchVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCompatibleElasticsearchVersionsResponse
mkGetCompatibleElasticsearchVersionsResponse responseStatus =
  GetCompatibleElasticsearchVersionsResponse'
    { compatibleElasticsearchVersions =
        Core.Nothing,
      responseStatus
    }

-- | A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /Note:/ Consider using 'compatibleElasticsearchVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevrrsCompatibleElasticsearchVersions :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse (Core.Maybe [Types.CompatibleVersionsMap])
gcevrrsCompatibleElasticsearchVersions = Lens.field @"compatibleElasticsearchVersions"
{-# DEPRECATED gcevrrsCompatibleElasticsearchVersions "Use generic-lens or generic-optics with 'compatibleElasticsearchVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevrrsResponseStatus :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse Core.Int
gcevrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcevrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
