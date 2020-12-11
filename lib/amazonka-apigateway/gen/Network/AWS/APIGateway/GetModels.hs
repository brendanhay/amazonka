{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes existing 'Models' defined for a 'RestApi' resource.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetModels
  ( -- * Creating a request
    GetModels (..),
    mkGetModels,

    -- ** Request lenses
    gmsLimit,
    gmsPosition,
    gmsRestAPIId,

    -- * Destructuring the response
    GetModelsResponse (..),
    mkGetModelsResponse,

    -- ** Response lenses
    gmrsItems,
    gmrsPosition,
    gmrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to list existing 'Models' defined for a 'RestApi' resource.
--
-- /See:/ 'mkGetModels' smart constructor.
data GetModels = GetModels'
  { limit :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'GetModels' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetModels ::
  -- | 'restAPIId'
  Lude.Text ->
  GetModels
mkGetModels pRestAPIId_ =
  GetModels'
    { limit = Lude.Nothing,
      position = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsLimit :: Lens.Lens' GetModels (Lude.Maybe Lude.Int)
gmsLimit = Lens.lens (limit :: GetModels -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetModels)
{-# DEPRECATED gmsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsPosition :: Lens.Lens' GetModels (Lude.Maybe Lude.Text)
gmsPosition = Lens.lens (position :: GetModels -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetModels)
{-# DEPRECATED gmsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsRestAPIId :: Lens.Lens' GetModels Lude.Text
gmsRestAPIId = Lens.lens (restAPIId :: GetModels -> Lude.Text) (\s a -> s {restAPIId = a} :: GetModels)
{-# DEPRECATED gmsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Page.AWSPager GetModels where
  page rq rs
    | Page.stop (rs Lens.^. gmrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gmrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gmsPosition Lens..~ rs Lens.^. gmrsPosition

instance Lude.AWSRequest GetModels where
  type Rs GetModels = GetModelsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetModelsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetModels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetModels where
  toPath GetModels' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/models"]

instance Lude.ToQuery GetModels where
  toQuery GetModels' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection of 'Model' resources.
--
-- 'Method' , 'MethodResponse' , <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Models and Mappings>
--
-- /See:/ 'mkGetModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { items ::
      Lude.Maybe [Model],
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

-- | Creates a value of 'GetModelsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetModelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetModelsResponse
mkGetModelsResponse pResponseStatus_ =
  GetModelsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsItems :: Lens.Lens' GetModelsResponse (Lude.Maybe [Model])
gmrsItems = Lens.lens (items :: GetModelsResponse -> Lude.Maybe [Model]) (\s a -> s {items = a} :: GetModelsResponse)
{-# DEPRECATED gmrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsPosition :: Lens.Lens' GetModelsResponse (Lude.Maybe Lude.Text)
gmrsPosition = Lens.lens (position :: GetModelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetModelsResponse)
{-# DEPRECATED gmrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsResponseStatus :: Lens.Lens' GetModelsResponse Lude.Int
gmrsResponseStatus = Lens.lens (responseStatus :: GetModelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetModelsResponse)
{-# DEPRECATED gmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
