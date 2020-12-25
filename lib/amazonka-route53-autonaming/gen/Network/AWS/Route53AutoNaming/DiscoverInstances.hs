{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DiscoverInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discovers registered instances for a specified namespace and service. You can use @DiscoverInstances@ to discover instances for any type of namespace. For public and private DNS namespaces, you can also use DNS queries to discover instances.
module Network.AWS.Route53AutoNaming.DiscoverInstances
  ( -- * Creating a request
    DiscoverInstances (..),
    mkDiscoverInstances,

    -- ** Request lenses
    diNamespaceName,
    diServiceName,
    diHealthStatus,
    diMaxResults,
    diOptionalParameters,
    diQueryParameters,

    -- * Destructuring the response
    DiscoverInstancesResponse (..),
    mkDiscoverInstancesResponse,

    -- ** Response lenses
    dirrsInstances,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkDiscoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { -- | The name of the namespace that you specified when you registered the instance.
    namespaceName :: Types.NamespaceName,
    -- | The name of the service that you specified when you registered the instance.
    serviceName :: Types.ServiceName,
    -- | The health status of the instances that you want to discover.
    healthStatus :: Core.Maybe Types.HealthStatusFilter,
    -- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
    maxResults :: Core.Maybe Core.Natural,
    -- | Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
    optionalParameters :: Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue),
    -- | Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
    queryParameters :: Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiscoverInstances' value with any optional fields omitted.
mkDiscoverInstances ::
  -- | 'namespaceName'
  Types.NamespaceName ->
  -- | 'serviceName'
  Types.ServiceName ->
  DiscoverInstances
mkDiscoverInstances namespaceName serviceName =
  DiscoverInstances'
    { namespaceName,
      serviceName,
      healthStatus = Core.Nothing,
      maxResults = Core.Nothing,
      optionalParameters = Core.Nothing,
      queryParameters = Core.Nothing
    }

-- | The name of the namespace that you specified when you registered the instance.
--
-- /Note:/ Consider using 'namespaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNamespaceName :: Lens.Lens' DiscoverInstances Types.NamespaceName
diNamespaceName = Lens.field @"namespaceName"
{-# DEPRECATED diNamespaceName "Use generic-lens or generic-optics with 'namespaceName' instead." #-}

-- | The name of the service that you specified when you registered the instance.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diServiceName :: Lens.Lens' DiscoverInstances Types.ServiceName
diServiceName = Lens.field @"serviceName"
{-# DEPRECATED diServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The health status of the instances that you want to discover.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diHealthStatus :: Lens.Lens' DiscoverInstances (Core.Maybe Types.HealthStatusFilter)
diHealthStatus = Lens.field @"healthStatus"
{-# DEPRECATED diHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DiscoverInstances (Core.Maybe Core.Natural)
diMaxResults = Lens.field @"maxResults"
{-# DEPRECATED diMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
--
-- /Note:/ Consider using 'optionalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diOptionalParameters :: Lens.Lens' DiscoverInstances (Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue))
diOptionalParameters = Lens.field @"optionalParameters"
{-# DEPRECATED diOptionalParameters "Use generic-lens or generic-optics with 'optionalParameters' instead." #-}

-- | Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
--
-- /Note:/ Consider using 'queryParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diQueryParameters :: Lens.Lens' DiscoverInstances (Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue))
diQueryParameters = Lens.field @"queryParameters"
{-# DEPRECATED diQueryParameters "Use generic-lens or generic-optics with 'queryParameters' instead." #-}

instance Core.FromJSON DiscoverInstances where
  toJSON DiscoverInstances {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NamespaceName" Core..= namespaceName),
            Core.Just ("ServiceName" Core..= serviceName),
            ("HealthStatus" Core..=) Core.<$> healthStatus,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("OptionalParameters" Core..=) Core.<$> optionalParameters,
            ("QueryParameters" Core..=) Core.<$> queryParameters
          ]
      )

instance Core.AWSRequest DiscoverInstances where
  type Rs DiscoverInstances = DiscoverInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.DiscoverInstances")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverInstancesResponse'
            Core.<$> (x Core..:? "Instances") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDiscoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { -- | A complex type that contains one @HttpInstanceSummary@ for each registered instance.
    instances :: Core.Maybe [Types.HttpInstanceSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiscoverInstancesResponse' value with any optional fields omitted.
mkDiscoverInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DiscoverInstancesResponse
mkDiscoverInstancesResponse responseStatus =
  DiscoverInstancesResponse'
    { instances = Core.Nothing,
      responseStatus
    }

-- | A complex type that contains one @HttpInstanceSummary@ for each registered instance.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInstances :: Lens.Lens' DiscoverInstancesResponse (Core.Maybe [Types.HttpInstanceSummary])
dirrsInstances = Lens.field @"instances"
{-# DEPRECATED dirrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DiscoverInstancesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
