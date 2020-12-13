{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetAuthorizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizers' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizers.html AWS CLI>
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetAuthorizers
  ( -- * Creating a request
    GetAuthorizers (..),
    mkGetAuthorizers,

    -- ** Request lenses
    gaLimit,
    gaRestAPIId,
    gaPosition,

    -- * Destructuring the response
    GetAuthorizersResponse (..),
    mkGetAuthorizersResponse,

    -- ** Response lenses
    garsItems,
    garsPosition,
    garsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe an existing 'Authorizers' resource.
--
-- /See:/ 'mkGetAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAuthorizers' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'position' - The current pagination position in the paged result set.
mkGetAuthorizers ::
  -- | 'restAPIId'
  Lude.Text ->
  GetAuthorizers
mkGetAuthorizers pRestAPIId_ =
  GetAuthorizers'
    { limit = Lude.Nothing,
      restAPIId = pRestAPIId_,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaLimit :: Lens.Lens' GetAuthorizers (Lude.Maybe Lude.Int)
gaLimit = Lens.lens (limit :: GetAuthorizers -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetAuthorizers)
{-# DEPRECATED gaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaRestAPIId :: Lens.Lens' GetAuthorizers Lude.Text
gaRestAPIId = Lens.lens (restAPIId :: GetAuthorizers -> Lude.Text) (\s a -> s {restAPIId = a} :: GetAuthorizers)
{-# DEPRECATED gaRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPosition :: Lens.Lens' GetAuthorizers (Lude.Maybe Lude.Text)
gaPosition = Lens.lens (position :: GetAuthorizers -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetAuthorizers)
{-# DEPRECATED gaPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetAuthorizers where
  page rq rs
    | Page.stop (rs Lens.^. garsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. garsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gaPosition Lens..~ rs Lens.^. garsPosition

instance Lude.AWSRequest GetAuthorizers where
  type Rs GetAuthorizers = GetAuthorizersResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAuthorizersResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAuthorizers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetAuthorizers where
  toPath GetAuthorizers' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/authorizers"]

instance Lude.ToQuery GetAuthorizers where
  toQuery GetAuthorizers' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection of 'Authorizer' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
--
-- /See:/ 'mkGetAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [Authorizer],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAuthorizersResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetAuthorizersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAuthorizersResponse
mkGetAuthorizersResponse pResponseStatus_ =
  GetAuthorizersResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsItems :: Lens.Lens' GetAuthorizersResponse (Lude.Maybe [Authorizer])
garsItems = Lens.lens (items :: GetAuthorizersResponse -> Lude.Maybe [Authorizer]) (\s a -> s {items = a} :: GetAuthorizersResponse)
{-# DEPRECATED garsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsPosition :: Lens.Lens' GetAuthorizersResponse (Lude.Maybe Lude.Text)
garsPosition = Lens.lens (position :: GetAuthorizersResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetAuthorizersResponse)
{-# DEPRECATED garsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAuthorizersResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAuthorizersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAuthorizersResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
