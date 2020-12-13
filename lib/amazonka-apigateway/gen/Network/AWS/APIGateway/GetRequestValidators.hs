{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetRequestValidators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'RequestValidators' collection of a given 'RestApi' .
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetRequestValidators
  ( -- * Creating a request
    GetRequestValidators (..),
    mkGetRequestValidators,

    -- ** Request lenses
    grvsLimit,
    grvsRestAPIId,
    grvsPosition,

    -- * Destructuring the response
    GetRequestValidatorsResponse (..),
    mkGetRequestValidatorsResponse,

    -- ** Response lenses
    grvrsItems,
    grvrsPosition,
    grvrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the 'RequestValidators' collection of a given 'RestApi' .
--
-- /See:/ 'mkGetRequestValidators' smart constructor.
data GetRequestValidators = GetRequestValidators'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRequestValidators' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'position' - The current pagination position in the paged result set.
mkGetRequestValidators ::
  -- | 'restAPIId'
  Lude.Text ->
  GetRequestValidators
mkGetRequestValidators pRestAPIId_ =
  GetRequestValidators'
    { limit = Lude.Nothing,
      restAPIId = pRestAPIId_,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvsLimit :: Lens.Lens' GetRequestValidators (Lude.Maybe Lude.Int)
grvsLimit = Lens.lens (limit :: GetRequestValidators -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetRequestValidators)
{-# DEPRECATED grvsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvsRestAPIId :: Lens.Lens' GetRequestValidators Lude.Text
grvsRestAPIId = Lens.lens (restAPIId :: GetRequestValidators -> Lude.Text) (\s a -> s {restAPIId = a} :: GetRequestValidators)
{-# DEPRECATED grvsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvsPosition :: Lens.Lens' GetRequestValidators (Lude.Maybe Lude.Text)
grvsPosition = Lens.lens (position :: GetRequestValidators -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetRequestValidators)
{-# DEPRECATED grvsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetRequestValidators where
  page rq rs
    | Page.stop (rs Lens.^. grvrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. grvrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grvsPosition Lens..~ rs Lens.^. grvrsPosition

instance Lude.AWSRequest GetRequestValidators where
  type Rs GetRequestValidators = GetRequestValidatorsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRequestValidatorsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRequestValidators where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetRequestValidators where
  toPath GetRequestValidators' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/requestvalidators"]

instance Lude.ToQuery GetRequestValidators where
  toQuery GetRequestValidators' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | A collection of 'RequestValidator' resources of a given 'RestApi' .
--
-- In OpenAPI, the 'RequestValidators' of an API is defined by the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html x-amazon-apigateway-request-validators> extension.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'mkGetRequestValidatorsResponse' smart constructor.
data GetRequestValidatorsResponse = GetRequestValidatorsResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [RequestValidator],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRequestValidatorsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetRequestValidatorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRequestValidatorsResponse
mkGetRequestValidatorsResponse pResponseStatus_ =
  GetRequestValidatorsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrsItems :: Lens.Lens' GetRequestValidatorsResponse (Lude.Maybe [RequestValidator])
grvrsItems = Lens.lens (items :: GetRequestValidatorsResponse -> Lude.Maybe [RequestValidator]) (\s a -> s {items = a} :: GetRequestValidatorsResponse)
{-# DEPRECATED grvrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrsPosition :: Lens.Lens' GetRequestValidatorsResponse (Lude.Maybe Lude.Text)
grvrsPosition = Lens.lens (position :: GetRequestValidatorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetRequestValidatorsResponse)
{-# DEPRECATED grvrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrsResponseStatus :: Lens.Lens' GetRequestValidatorsResponse Lude.Int
grvrsResponseStatus = Lens.lens (responseStatus :: GetRequestValidatorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRequestValidatorsResponse)
{-# DEPRECATED grvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
