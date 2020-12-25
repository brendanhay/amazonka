{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.ListServerNeighbors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of servers that are one network hop away from a specified server.
module Network.AWS.Discovery.ListServerNeighbors
  ( -- * Creating a request
    ListServerNeighbors (..),
    mkListServerNeighbors,

    -- ** Request lenses
    lsnConfigurationId,
    lsnMaxResults,
    lsnNeighborConfigurationIds,
    lsnNextToken,
    lsnPortInformationNeeded,

    -- * Destructuring the response
    ListServerNeighborsResponse (..),
    mkListServerNeighborsResponse,

    -- ** Response lenses
    lsnrrsNeighbors,
    lsnrrsKnownDependencyCount,
    lsnrrsNextToken,
    lsnrrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListServerNeighbors' smart constructor.
data ListServerNeighbors = ListServerNeighbors'
  { -- | Configuration ID of the server for which neighbors are being listed.
    configurationId :: Types.ConfigurationId,
    -- | Maximum number of results to return in a single page of output.
    maxResults :: Core.Maybe Core.Int,
    -- | List of configuration IDs to test for one-hop-away.
    neighborConfigurationIds :: Core.Maybe [Types.ConfigurationId],
    -- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
    nextToken :: Core.Maybe Types.String,
    -- | Flag to indicate if port and protocol information is needed as part of the response.
    portInformationNeeded :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServerNeighbors' value with any optional fields omitted.
mkListServerNeighbors ::
  -- | 'configurationId'
  Types.ConfigurationId ->
  ListServerNeighbors
mkListServerNeighbors configurationId =
  ListServerNeighbors'
    { configurationId,
      maxResults = Core.Nothing,
      neighborConfigurationIds = Core.Nothing,
      nextToken = Core.Nothing,
      portInformationNeeded = Core.Nothing
    }

-- | Configuration ID of the server for which neighbors are being listed.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnConfigurationId :: Lens.Lens' ListServerNeighbors Types.ConfigurationId
lsnConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED lsnConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | Maximum number of results to return in a single page of output.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnMaxResults :: Lens.Lens' ListServerNeighbors (Core.Maybe Core.Int)
lsnMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | List of configuration IDs to test for one-hop-away.
--
-- /Note:/ Consider using 'neighborConfigurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnNeighborConfigurationIds :: Lens.Lens' ListServerNeighbors (Core.Maybe [Types.ConfigurationId])
lsnNeighborConfigurationIds = Lens.field @"neighborConfigurationIds"
{-# DEPRECATED lsnNeighborConfigurationIds "Use generic-lens or generic-optics with 'neighborConfigurationIds' instead." #-}

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnNextToken :: Lens.Lens' ListServerNeighbors (Core.Maybe Types.String)
lsnNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Flag to indicate if port and protocol information is needed as part of the response.
--
-- /Note:/ Consider using 'portInformationNeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnPortInformationNeeded :: Lens.Lens' ListServerNeighbors (Core.Maybe Core.Bool)
lsnPortInformationNeeded = Lens.field @"portInformationNeeded"
{-# DEPRECATED lsnPortInformationNeeded "Use generic-lens or generic-optics with 'portInformationNeeded' instead." #-}

instance Core.FromJSON ListServerNeighbors where
  toJSON ListServerNeighbors {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("configurationId" Core..= configurationId),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("neighborConfigurationIds" Core..=)
              Core.<$> neighborConfigurationIds,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("portInformationNeeded" Core..=) Core.<$> portInformationNeeded
          ]
      )

instance Core.AWSRequest ListServerNeighbors where
  type Rs ListServerNeighbors = ListServerNeighborsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.ListServerNeighbors"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServerNeighborsResponse'
            Core.<$> (x Core..:? "neighbors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "knownDependencyCount")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListServerNeighborsResponse' smart constructor.
data ListServerNeighborsResponse = ListServerNeighborsResponse'
  { -- | List of distinct servers that are one hop away from the given server.
    neighbors :: [Types.NeighborConnectionDetail],
    -- | Count of distinct servers that are one hop away from the given server.
    knownDependencyCount :: Core.Maybe Core.Integer,
    -- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServerNeighborsResponse' value with any optional fields omitted.
mkListServerNeighborsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListServerNeighborsResponse
mkListServerNeighborsResponse responseStatus =
  ListServerNeighborsResponse'
    { neighbors = Core.mempty,
      knownDependencyCount = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of distinct servers that are one hop away from the given server.
--
-- /Note:/ Consider using 'neighbors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrrsNeighbors :: Lens.Lens' ListServerNeighborsResponse [Types.NeighborConnectionDetail]
lsnrrsNeighbors = Lens.field @"neighbors"
{-# DEPRECATED lsnrrsNeighbors "Use generic-lens or generic-optics with 'neighbors' instead." #-}

-- | Count of distinct servers that are one hop away from the given server.
--
-- /Note:/ Consider using 'knownDependencyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrrsKnownDependencyCount :: Lens.Lens' ListServerNeighborsResponse (Core.Maybe Core.Integer)
lsnrrsKnownDependencyCount = Lens.field @"knownDependencyCount"
{-# DEPRECATED lsnrrsKnownDependencyCount "Use generic-lens or generic-optics with 'knownDependencyCount' instead." #-}

-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrrsNextToken :: Lens.Lens' ListServerNeighborsResponse (Core.Maybe Types.NextToken)
lsnrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsnrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrrsResponseStatus :: Lens.Lens' ListServerNeighborsResponse Core.Int
lsnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
