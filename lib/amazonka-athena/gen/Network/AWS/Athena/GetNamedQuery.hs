{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query. Requires that you have access to the workgroup in which the query was saved.
module Network.AWS.Athena.GetNamedQuery
  ( -- * Creating a request
    GetNamedQuery (..),
    mkGetNamedQuery,

    -- ** Request lenses
    gnqNamedQueryId,

    -- * Destructuring the response
    GetNamedQueryResponse (..),
    mkGetNamedQueryResponse,

    -- ** Response lenses
    gnqrrsNamedQuery,
    gnqrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetNamedQuery' smart constructor.
newtype GetNamedQuery = GetNamedQuery'
  { -- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
    namedQueryId :: Types.NamedQueryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetNamedQuery' value with any optional fields omitted.
mkGetNamedQuery ::
  -- | 'namedQueryId'
  Types.NamedQueryId ->
  GetNamedQuery
mkGetNamedQuery namedQueryId = GetNamedQuery' {namedQueryId}

-- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqNamedQueryId :: Lens.Lens' GetNamedQuery Types.NamedQueryId
gnqNamedQueryId = Lens.field @"namedQueryId"
{-# DEPRECATED gnqNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

instance Core.FromJSON GetNamedQuery where
  toJSON GetNamedQuery {..} =
    Core.object
      (Core.catMaybes [Core.Just ("NamedQueryId" Core..= namedQueryId)])

instance Core.AWSRequest GetNamedQuery where
  type Rs GetNamedQuery = GetNamedQueryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.GetNamedQuery")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamedQueryResponse'
            Core.<$> (x Core..:? "NamedQuery") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { -- | Information about the query.
    namedQuery :: Core.Maybe Types.NamedQuery,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNamedQueryResponse' value with any optional fields omitted.
mkGetNamedQueryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetNamedQueryResponse
mkGetNamedQueryResponse responseStatus =
  GetNamedQueryResponse' {namedQuery = Core.Nothing, responseStatus}

-- | Information about the query.
--
-- /Note:/ Consider using 'namedQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrrsNamedQuery :: Lens.Lens' GetNamedQueryResponse (Core.Maybe Types.NamedQuery)
gnqrrsNamedQuery = Lens.field @"namedQuery"
{-# DEPRECATED gnqrrsNamedQuery "Use generic-lens or generic-optics with 'namedQuery' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrrsResponseStatus :: Lens.Lens' GetNamedQueryResponse Core.Int
gnqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gnqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
