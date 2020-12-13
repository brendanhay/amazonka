{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDocumentationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDocumentationVersions
  ( -- * Creating a request
    GetDocumentationVersions (..),
    mkGetDocumentationVersions,

    -- ** Request lenses
    gdvLimit,
    gdvRestAPIId,
    gdvPosition,

    -- * Destructuring the response
    GetDocumentationVersionsResponse (..),
    mkGetDocumentationVersionsResponse,

    -- ** Response lenses
    gdvrsItems,
    gdvrsPosition,
    gdvrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the documentation versions of an API.
--
-- /See:/ 'mkGetDocumentationVersions' smart constructor.
data GetDocumentationVersions = GetDocumentationVersions'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentationVersions' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'position' - The current pagination position in the paged result set.
mkGetDocumentationVersions ::
  -- | 'restAPIId'
  Lude.Text ->
  GetDocumentationVersions
mkGetDocumentationVersions pRestAPIId_ =
  GetDocumentationVersions'
    { limit = Lude.Nothing,
      restAPIId = pRestAPIId_,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvLimit :: Lens.Lens' GetDocumentationVersions (Lude.Maybe Lude.Int)
gdvLimit = Lens.lens (limit :: GetDocumentationVersions -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetDocumentationVersions)
{-# DEPRECATED gdvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvRestAPIId :: Lens.Lens' GetDocumentationVersions Lude.Text
gdvRestAPIId = Lens.lens (restAPIId :: GetDocumentationVersions -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDocumentationVersions)
{-# DEPRECATED gdvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvPosition :: Lens.Lens' GetDocumentationVersions (Lude.Maybe Lude.Text)
gdvPosition = Lens.lens (position :: GetDocumentationVersions -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDocumentationVersions)
{-# DEPRECATED gdvPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetDocumentationVersions where
  page rq rs
    | Page.stop (rs Lens.^. gdvrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gdvrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdvPosition Lens..~ rs Lens.^. gdvrsPosition

instance Lude.AWSRequest GetDocumentationVersions where
  type Rs GetDocumentationVersions = GetDocumentationVersionsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentationVersionsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocumentationVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDocumentationVersions where
  toPath GetDocumentationVersions' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/documentation/versions"]

instance Lude.ToQuery GetDocumentationVersions where
  toQuery GetDocumentationVersions' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | The collection of documentation snapshots of an API.
--
-- Use the 'DocumentationVersions' to manage documentation snapshots associated with various API stages.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersion'
--
-- /See:/ 'mkGetDocumentationVersionsResponse' smart constructor.
data GetDocumentationVersionsResponse = GetDocumentationVersionsResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [DocumentationVersion],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentationVersionsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetDocumentationVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentationVersionsResponse
mkGetDocumentationVersionsResponse pResponseStatus_ =
  GetDocumentationVersionsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsItems :: Lens.Lens' GetDocumentationVersionsResponse (Lude.Maybe [DocumentationVersion])
gdvrsItems = Lens.lens (items :: GetDocumentationVersionsResponse -> Lude.Maybe [DocumentationVersion]) (\s a -> s {items = a} :: GetDocumentationVersionsResponse)
{-# DEPRECATED gdvrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsPosition :: Lens.Lens' GetDocumentationVersionsResponse (Lude.Maybe Lude.Text)
gdvrsPosition = Lens.lens (position :: GetDocumentationVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDocumentationVersionsResponse)
{-# DEPRECATED gdvrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsResponseStatus :: Lens.Lens' GetDocumentationVersionsResponse Lude.Int
gdvrsResponseStatus = Lens.lens (responseStatus :: GetDocumentationVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentationVersionsResponse)
{-# DEPRECATED gdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
