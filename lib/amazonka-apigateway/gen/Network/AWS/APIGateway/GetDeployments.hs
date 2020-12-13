{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Deployments' collection.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDeployments
  ( -- * Creating a request
    GetDeployments (..),
    mkGetDeployments,

    -- ** Request lenses
    gdLimit,
    gdRestAPIId,
    gdPosition,

    -- * Destructuring the response
    GetDeploymentsResponse (..),
    mkGetDeploymentsResponse,

    -- ** Response lenses
    gdrsItems,
    gdrsPosition,
    gdrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to get information about a 'Deployments' collection.
--
-- /See:/ 'mkGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeployments' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'position' - The current pagination position in the paged result set.
mkGetDeployments ::
  -- | 'restAPIId'
  Lude.Text ->
  GetDeployments
mkGetDeployments pRestAPIId_ =
  GetDeployments'
    { limit = Lude.Nothing,
      restAPIId = pRestAPIId_,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdLimit :: Lens.Lens' GetDeployments (Lude.Maybe Lude.Int)
gdLimit = Lens.lens (limit :: GetDeployments -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetDeployments)
{-# DEPRECATED gdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdRestAPIId :: Lens.Lens' GetDeployments Lude.Text
gdRestAPIId = Lens.lens (restAPIId :: GetDeployments -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDeployments)
{-# DEPRECATED gdRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPosition :: Lens.Lens' GetDeployments (Lude.Maybe Lude.Text)
gdPosition = Lens.lens (position :: GetDeployments -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDeployments)
{-# DEPRECATED gdPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetDeployments where
  page rq rs
    | Page.stop (rs Lens.^. gdrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gdrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdPosition Lens..~ rs Lens.^. gdrsPosition

instance Lude.AWSRequest GetDeployments where
  type Rs GetDeployments = GetDeploymentsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDeployments where
  toPath GetDeployments' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/deployments"]

instance Lude.ToQuery GetDeployments where
  toQuery GetDeployments' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection resource that contains zero or more references to your existing deployments, and links that guide you on how to interact with your collection. The collection offers a paginated view of the contained deployments.
--
-- To create a new deployment of a 'RestApi' , make a @POST@ request against this resource. To view, update, or delete an existing deployment, make a @GET@ , @PATCH@ , or @DELETE@ request, respectively, on a specified 'Deployment' resource.<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploying an API> , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'mkGetDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [Deployment],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentsResponse
mkGetDeploymentsResponse pResponseStatus_ =
  GetDeploymentsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsItems :: Lens.Lens' GetDeploymentsResponse (Lude.Maybe [Deployment])
gdrsItems = Lens.lens (items :: GetDeploymentsResponse -> Lude.Maybe [Deployment]) (\s a -> s {items = a} :: GetDeploymentsResponse)
{-# DEPRECATED gdrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsPosition :: Lens.Lens' GetDeploymentsResponse (Lude.Maybe Lude.Text)
gdrsPosition = Lens.lens (position :: GetDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDeploymentsResponse)
{-# DEPRECATED gdrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDeploymentsResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentsResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
