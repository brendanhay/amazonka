{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListDomainNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name of all Elasticsearch domains owned by the current user's account.
module Network.AWS.ElasticSearch.ListDomainNames
  ( -- * Creating a request
    ListDomainNames (..),
    mkListDomainNames,

    -- * Destructuring the response
    ListDomainNamesResponse (..),
    mkListDomainNamesResponse,

    -- ** Response lenses
    ldnrrsDomainNames,
    ldnrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainNames' value with any optional fields omitted.
mkListDomainNames ::
  ListDomainNames
mkListDomainNames = ListDomainNames'

instance Core.AWSRequest ListDomainNames where
  type Rs ListDomainNames = ListDomainNamesResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2015-01-01/domain",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainNamesResponse'
            Core.<$> (x Core..:? "DomainNames") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @ListDomainNames@ operation. Contains the names of all Elasticsearch domains owned by this account.
--
-- /See:/ 'mkListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { -- | List of Elasticsearch domain names.
    domainNames :: Core.Maybe [Types.DomainInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainNamesResponse' value with any optional fields omitted.
mkListDomainNamesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDomainNamesResponse
mkListDomainNamesResponse responseStatus =
  ListDomainNamesResponse'
    { domainNames = Core.Nothing,
      responseStatus
    }

-- | List of Elasticsearch domain names.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrrsDomainNames :: Lens.Lens' ListDomainNamesResponse (Core.Maybe [Types.DomainInfo])
ldnrrsDomainNames = Lens.field @"domainNames"
{-# DEPRECATED ldnrrsDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrrsResponseStatus :: Lens.Lens' ListDomainNamesResponse Core.Int
ldnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
