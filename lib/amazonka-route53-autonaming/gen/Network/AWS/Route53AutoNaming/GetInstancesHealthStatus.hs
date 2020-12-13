{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current health status (@Healthy@ , @Unhealthy@ , or @Unknown@ ) of one or more instances that are associated with a specified service.
module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
  ( -- * Creating a request
    GetInstancesHealthStatus (..),
    mkGetInstancesHealthStatus,

    -- ** Request lenses
    gihsNextToken,
    gihsInstances,
    gihsServiceId,
    gihsMaxResults,

    -- * Destructuring the response
    GetInstancesHealthStatusResponse (..),
    mkGetInstancesHealthStatusResponse,

    -- ** Response lenses
    gihsrsStatus,
    gihsrsNextToken,
    gihsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkGetInstancesHealthStatus' smart constructor.
data GetInstancesHealthStatus = GetInstancesHealthStatus'
  { -- | For the first @GetInstancesHealthStatus@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array that contains the IDs of all the instances that you want to get the health status for.
    --
    -- If you omit @Instances@ , AWS Cloud Map returns the health status for all the instances that are associated with the specified service.
    instances :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The ID of the service that the instance is associated with.
    serviceId :: Lude.Text,
    -- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstancesHealthStatus' with the minimum fields required to make a request.
--
-- * 'nextToken' - For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'instances' - An array that contains the IDs of all the instances that you want to get the health status for.
--
-- If you omit @Instances@ , AWS Cloud Map returns the health status for all the instances that are associated with the specified service.
-- * 'serviceId' - The ID of the service that the instance is associated with.
-- * 'maxResults' - The maximum number of instances that you want AWS Cloud Map to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
mkGetInstancesHealthStatus ::
  -- | 'serviceId'
  Lude.Text ->
  GetInstancesHealthStatus
mkGetInstancesHealthStatus pServiceId_ =
  GetInstancesHealthStatus'
    { nextToken = Lude.Nothing,
      instances = Lude.Nothing,
      serviceId = pServiceId_,
      maxResults = Lude.Nothing
    }

-- | For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsNextToken :: Lens.Lens' GetInstancesHealthStatus (Lude.Maybe Lude.Text)
gihsNextToken = Lens.lens (nextToken :: GetInstancesHealthStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInstancesHealthStatus)
{-# DEPRECATED gihsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array that contains the IDs of all the instances that you want to get the health status for.
--
-- If you omit @Instances@ , AWS Cloud Map returns the health status for all the instances that are associated with the specified service.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsInstances :: Lens.Lens' GetInstancesHealthStatus (Lude.Maybe (Lude.NonEmpty Lude.Text))
gihsInstances = Lens.lens (instances :: GetInstancesHealthStatus -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {instances = a} :: GetInstancesHealthStatus)
{-# DEPRECATED gihsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsServiceId :: Lens.Lens' GetInstancesHealthStatus Lude.Text
gihsServiceId = Lens.lens (serviceId :: GetInstancesHealthStatus -> Lude.Text) (\s a -> s {serviceId = a} :: GetInstancesHealthStatus)
{-# DEPRECATED gihsServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsMaxResults :: Lens.Lens' GetInstancesHealthStatus (Lude.Maybe Lude.Natural)
gihsMaxResults = Lens.lens (maxResults :: GetInstancesHealthStatus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetInstancesHealthStatus)
{-# DEPRECATED gihsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetInstancesHealthStatus where
  type Rs GetInstancesHealthStatus = GetInstancesHealthStatusResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstancesHealthStatusResponse'
            Lude.<$> (x Lude..?> "Status" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstancesHealthStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.GetInstancesHealthStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstancesHealthStatus where
  toJSON GetInstancesHealthStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Instances" Lude..=) Lude.<$> instances,
            Lude.Just ("ServiceId" Lude..= serviceId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetInstancesHealthStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstancesHealthStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstancesHealthStatusResponse' smart constructor.
data GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse'
  { -- | A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
    status :: Lude.Maybe (Lude.HashMap Lude.Text (HealthStatus)),
    -- | If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstancesHealthStatusResponse' with the minimum fields required to make a request.
--
-- * 'status' - A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
-- * 'nextToken' - If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'responseStatus' - The response status code.
mkGetInstancesHealthStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstancesHealthStatusResponse
mkGetInstancesHealthStatusResponse pResponseStatus_ =
  GetInstancesHealthStatusResponse'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrsStatus :: Lens.Lens' GetInstancesHealthStatusResponse (Lude.Maybe (Lude.HashMap Lude.Text (HealthStatus)))
gihsrsStatus = Lens.lens (status :: GetInstancesHealthStatusResponse -> Lude.Maybe (Lude.HashMap Lude.Text (HealthStatus))) (\s a -> s {status = a} :: GetInstancesHealthStatusResponse)
{-# DEPRECATED gihsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrsNextToken :: Lens.Lens' GetInstancesHealthStatusResponse (Lude.Maybe Lude.Text)
gihsrsNextToken = Lens.lens (nextToken :: GetInstancesHealthStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInstancesHealthStatusResponse)
{-# DEPRECATED gihsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrsResponseStatus :: Lens.Lens' GetInstancesHealthStatusResponse Lude.Int
gihsrsResponseStatus = Lens.lens (responseStatus :: GetInstancesHealthStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstancesHealthStatusResponse)
{-# DEPRECATED gihsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
