{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    diQueryParameters,
    diOptionalParameters,
    diHealthStatus,
    diMaxResults,
    diNamespaceName,
    diServiceName,

    -- * Destructuring the response
    DiscoverInstancesResponse (..),
    mkDiscoverInstancesResponse,

    -- ** Response lenses
    dirsInstances,
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkDiscoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { queryParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    optionalParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    healthStatus :: Lude.Maybe HealthStatusFilter,
    maxResults :: Lude.Maybe Lude.Natural,
    namespaceName :: Lude.Text,
    serviceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverInstances' with the minimum fields required to make a request.
--
-- * 'healthStatus' - The health status of the instances that you want to discover.
-- * 'maxResults' - The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
-- * 'namespaceName' - The name of the namespace that you specified when you registered the instance.
-- * 'optionalParameters' - Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
-- * 'queryParameters' - Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
-- * 'serviceName' - The name of the service that you specified when you registered the instance.
mkDiscoverInstances ::
  -- | 'namespaceName'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  DiscoverInstances
mkDiscoverInstances pNamespaceName_ pServiceName_ =
  DiscoverInstances'
    { queryParameters = Lude.Nothing,
      optionalParameters = Lude.Nothing,
      healthStatus = Lude.Nothing,
      maxResults = Lude.Nothing,
      namespaceName = pNamespaceName_,
      serviceName = pServiceName_
    }

-- | Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
--
-- /Note:/ Consider using 'queryParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diQueryParameters :: Lens.Lens' DiscoverInstances (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diQueryParameters = Lens.lens (queryParameters :: DiscoverInstances -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {queryParameters = a} :: DiscoverInstances)
{-# DEPRECATED diQueryParameters "Use generic-lens or generic-optics with 'queryParameters' instead." #-}

-- | Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
--
-- /Note:/ Consider using 'optionalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diOptionalParameters :: Lens.Lens' DiscoverInstances (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diOptionalParameters = Lens.lens (optionalParameters :: DiscoverInstances -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {optionalParameters = a} :: DiscoverInstances)
{-# DEPRECATED diOptionalParameters "Use generic-lens or generic-optics with 'optionalParameters' instead." #-}

-- | The health status of the instances that you want to discover.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diHealthStatus :: Lens.Lens' DiscoverInstances (Lude.Maybe HealthStatusFilter)
diHealthStatus = Lens.lens (healthStatus :: DiscoverInstances -> Lude.Maybe HealthStatusFilter) (\s a -> s {healthStatus = a} :: DiscoverInstances)
{-# DEPRECATED diHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DiscoverInstances (Lude.Maybe Lude.Natural)
diMaxResults = Lens.lens (maxResults :: DiscoverInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DiscoverInstances)
{-# DEPRECATED diMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the namespace that you specified when you registered the instance.
--
-- /Note:/ Consider using 'namespaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNamespaceName :: Lens.Lens' DiscoverInstances Lude.Text
diNamespaceName = Lens.lens (namespaceName :: DiscoverInstances -> Lude.Text) (\s a -> s {namespaceName = a} :: DiscoverInstances)
{-# DEPRECATED diNamespaceName "Use generic-lens or generic-optics with 'namespaceName' instead." #-}

-- | The name of the service that you specified when you registered the instance.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diServiceName :: Lens.Lens' DiscoverInstances Lude.Text
diServiceName = Lens.lens (serviceName :: DiscoverInstances -> Lude.Text) (\s a -> s {serviceName = a} :: DiscoverInstances)
{-# DEPRECATED diServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest DiscoverInstances where
  type Rs DiscoverInstances = DiscoverInstancesResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DiscoverInstancesResponse'
            Lude.<$> (x Lude..?> "Instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DiscoverInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.DiscoverInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DiscoverInstances where
  toJSON DiscoverInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QueryParameters" Lude..=) Lude.<$> queryParameters,
            ("OptionalParameters" Lude..=) Lude.<$> optionalParameters,
            ("HealthStatus" Lude..=) Lude.<$> healthStatus,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("NamespaceName" Lude..= namespaceName),
            Lude.Just ("ServiceName" Lude..= serviceName)
          ]
      )

instance Lude.ToPath DiscoverInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DiscoverInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDiscoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { instances ::
      Lude.Maybe [HTTPInstanceSummary],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instances' - A complex type that contains one @HttpInstanceSummary@ for each registered instance.
-- * 'responseStatus' - The response status code.
mkDiscoverInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DiscoverInstancesResponse
mkDiscoverInstancesResponse pResponseStatus_ =
  DiscoverInstancesResponse'
    { instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains one @HttpInstanceSummary@ for each registered instance.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsInstances :: Lens.Lens' DiscoverInstancesResponse (Lude.Maybe [HTTPInstanceSummary])
dirsInstances = Lens.lens (instances :: DiscoverInstancesResponse -> Lude.Maybe [HTTPInstanceSummary]) (\s a -> s {instances = a} :: DiscoverInstancesResponse)
{-# DEPRECATED dirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DiscoverInstancesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DiscoverInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DiscoverInstancesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
