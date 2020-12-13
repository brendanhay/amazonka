{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the instances that you registered by using a specified service.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListInstances
  ( -- * Creating a request
    ListInstances (..),
    mkListInstances,

    -- ** Request lenses
    liNextToken,
    liServiceId,
    liMaxResults,

    -- * Destructuring the response
    ListInstancesResponse (..),
    mkListInstancesResponse,

    -- ** Response lenses
    lirsNextToken,
    lirsInstances,
    lirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | For the first @ListInstances@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the service that you want to list instances for.
    serviceId :: Lude.Text,
    -- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- * 'nextToken' - For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'serviceId' - The ID of the service that you want to list instances for.
-- * 'maxResults' - The maximum number of instances that you want AWS Cloud Map to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
mkListInstances ::
  -- | 'serviceId'
  Lude.Text ->
  ListInstances
mkListInstances pServiceId_ =
  ListInstances'
    { nextToken = Lude.Nothing,
      serviceId = pServiceId_,
      maxResults = Lude.Nothing
    }

-- | For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInstances (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstances)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the service that you want to list instances for.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liServiceId :: Lens.Lens' ListInstances Lude.Text
liServiceId = Lens.lens (serviceId :: ListInstances -> Lude.Text) (\s a -> s {serviceId = a} :: ListInstances)
{-# DEPRECATED liServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInstances (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInstances)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInstances where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListInstances where
  type Rs ListInstances = ListInstancesResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.ListInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ServiceId" Lude..= serviceId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Summary information about the instances that are associated with the specified service.
    instances :: Lude.Maybe [InstanceSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'instances' - Summary information about the instances that are associated with the specified service.
-- * 'responseStatus' - The response status code.
mkListInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstancesResponse
mkListInstancesResponse pResponseStatus_ =
  ListInstancesResponse'
    { nextToken = Lude.Nothing,
      instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListInstancesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstancesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about the instances that are associated with the specified service.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInstances :: Lens.Lens' ListInstancesResponse (Lude.Maybe [InstanceSummary])
lirsInstances = Lens.lens (instances :: ListInstancesResponse -> Lude.Maybe [InstanceSummary]) (\s a -> s {instances = a} :: ListInstancesResponse)
{-# DEPRECATED lirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInstancesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstancesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
