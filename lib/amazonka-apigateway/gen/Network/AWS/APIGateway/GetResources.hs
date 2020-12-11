{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a collection of 'Resource' resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetResources
  ( -- * Creating a request
    GetResources (..),
    mkGetResources,

    -- ** Request lenses
    grsEmbed,
    grsLimit,
    grsPosition,
    grsRestAPIId,

    -- * Destructuring the response
    GetResourcesResponse (..),
    mkGetResourcesResponse,

    -- ** Response lenses
    grrsItems,
    grrsPosition,
    grrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to list information about a collection of resources.
--
-- /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { embed :: Lude.Maybe [Lude.Text],
    limit :: Lude.Maybe Lude.Int,
    position :: Lude.Maybe Lude.Text,
    restAPIId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- * 'embed' - A query parameter used to retrieve the specified resources embedded in the returned 'Resources' resource in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources?embed=methods@ .
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetResources ::
  -- | 'restAPIId'
  Lude.Text ->
  GetResources
mkGetResources pRestAPIId_ =
  GetResources'
    { embed = Lude.Nothing,
      limit = Lude.Nothing,
      position = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | A query parameter used to retrieve the specified resources embedded in the returned 'Resources' resource in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources?embed=methods@ .
--
-- /Note:/ Consider using 'embed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsEmbed :: Lens.Lens' GetResources (Lude.Maybe [Lude.Text])
grsEmbed = Lens.lens (embed :: GetResources -> Lude.Maybe [Lude.Text]) (\s a -> s {embed = a} :: GetResources)
{-# DEPRECATED grsEmbed "Use generic-lens or generic-optics with 'embed' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLimit :: Lens.Lens' GetResources (Lude.Maybe Lude.Int)
grsLimit = Lens.lens (limit :: GetResources -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetResources)
{-# DEPRECATED grsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPosition :: Lens.Lens' GetResources (Lude.Maybe Lude.Text)
grsPosition = Lens.lens (position :: GetResources -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetResources)
{-# DEPRECATED grsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsRestAPIId :: Lens.Lens' GetResources Lude.Text
grsRestAPIId = Lens.lens (restAPIId :: GetResources -> Lude.Text) (\s a -> s {restAPIId = a} :: GetResources)
{-# DEPRECATED grsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Page.AWSPager GetResources where
  page rq rs
    | Page.stop (rs Lens.^. grrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. grrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grsPosition Lens..~ rs Lens.^. grrsPosition

instance Lude.AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetResources where
  toPath GetResources' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/resources"]

instance Lude.ToQuery GetResources where
  toQuery GetResources' {..} =
    Lude.mconcat
      [ "embed"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> embed),
        "limit" Lude.=: limit,
        "position" Lude.=: position
      ]

-- | Represents a collection of 'Resource' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { items ::
      Lude.Maybe [Resource],
    position :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcesResponse
mkGetResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsItems :: Lens.Lens' GetResourcesResponse (Lude.Maybe [Resource])
grrsItems = Lens.lens (items :: GetResourcesResponse -> Lude.Maybe [Resource]) (\s a -> s {items = a} :: GetResourcesResponse)
{-# DEPRECATED grrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsPosition :: Lens.Lens' GetResourcesResponse (Lude.Maybe Lude.Text)
grrsPosition = Lens.lens (position :: GetResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetResourcesResponse)
{-# DEPRECATED grrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetResourcesResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcesResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
