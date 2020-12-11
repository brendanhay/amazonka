{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDocumentationParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDocumentationParts
  ( -- * Creating a request
    GetDocumentationParts (..),
    mkGetDocumentationParts,

    -- ** Request lenses
    gdpPath,
    gdpLocationStatus,
    gdpNameQuery,
    gdpLimit,
    gdpType,
    gdpPosition,
    gdpRestAPIId,

    -- * Destructuring the response
    GetDocumentationPartsResponse (..),
    mkGetDocumentationPartsResponse,

    -- ** Response lenses
    gdprsItems,
    gdprsPosition,
    gdprsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the documentation parts of an API. The result may be filtered by the type, name, or path of API entities (targets).
--
-- /See:/ 'mkGetDocumentationParts' smart constructor.
data GetDocumentationParts = GetDocumentationParts'
  { path ::
      Lude.Maybe Lude.Text,
    locationStatus :: Lude.Maybe LocationStatusType,
    nameQuery :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe DocumentationPartType,
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

-- | Creates a value of 'GetDocumentationParts' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'locationStatus' - The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
-- * 'nameQuery' - The name of API entities of the to-be-retrieved documentation parts.
-- * 'path' - The path of API entities of the to-be-retrieved documentation parts.
-- * 'position' - The current pagination position in the paged result set.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'type'' - The type of API entities of the to-be-retrieved documentation parts.
mkGetDocumentationParts ::
  -- | 'restAPIId'
  Lude.Text ->
  GetDocumentationParts
mkGetDocumentationParts pRestAPIId_ =
  GetDocumentationParts'
    { path = Lude.Nothing,
      locationStatus = Lude.Nothing,
      nameQuery = Lude.Nothing,
      limit = Lude.Nothing,
      type' = Lude.Nothing,
      position = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | The path of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpPath :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpPath = Lens.lens (path :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: GetDocumentationParts)
{-# DEPRECATED gdpPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
--
-- /Note:/ Consider using 'locationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLocationStatus :: Lens.Lens' GetDocumentationParts (Lude.Maybe LocationStatusType)
gdpLocationStatus = Lens.lens (locationStatus :: GetDocumentationParts -> Lude.Maybe LocationStatusType) (\s a -> s {locationStatus = a} :: GetDocumentationParts)
{-# DEPRECATED gdpLocationStatus "Use generic-lens or generic-optics with 'locationStatus' instead." #-}

-- | The name of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpNameQuery :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpNameQuery = Lens.lens (nameQuery :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {nameQuery = a} :: GetDocumentationParts)
{-# DEPRECATED gdpNameQuery "Use generic-lens or generic-optics with 'nameQuery' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLimit :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Int)
gdpLimit = Lens.lens (limit :: GetDocumentationParts -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetDocumentationParts)
{-# DEPRECATED gdpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The type of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpType :: Lens.Lens' GetDocumentationParts (Lude.Maybe DocumentationPartType)
gdpType = Lens.lens (type' :: GetDocumentationParts -> Lude.Maybe DocumentationPartType) (\s a -> s {type' = a} :: GetDocumentationParts)
{-# DEPRECATED gdpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpPosition :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpPosition = Lens.lens (position :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDocumentationParts)
{-# DEPRECATED gdpPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpRestAPIId :: Lens.Lens' GetDocumentationParts Lude.Text
gdpRestAPIId = Lens.lens (restAPIId :: GetDocumentationParts -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDocumentationParts)
{-# DEPRECATED gdpRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Page.AWSPager GetDocumentationParts where
  page rq rs
    | Page.stop (rs Lens.^. gdprsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gdprsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdpPosition Lens..~ rs Lens.^. gdprsPosition

instance Lude.AWSRequest GetDocumentationParts where
  type Rs GetDocumentationParts = GetDocumentationPartsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentationPartsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocumentationParts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDocumentationParts where
  toPath GetDocumentationParts' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/documentation/parts"]

instance Lude.ToQuery GetDocumentationParts where
  toQuery GetDocumentationParts' {..} =
    Lude.mconcat
      [ "path" Lude.=: path,
        "locationStatus" Lude.=: locationStatus,
        "name" Lude.=: nameQuery,
        "limit" Lude.=: limit,
        "type" Lude.=: type',
        "position" Lude.=: position
      ]

-- | The collection of documentation parts of an API.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart'
--
-- /See:/ 'mkGetDocumentationPartsResponse' smart constructor.
data GetDocumentationPartsResponse = GetDocumentationPartsResponse'
  { items ::
      Lude.Maybe [DocumentationPart],
    position ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetDocumentationPartsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetDocumentationPartsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentationPartsResponse
mkGetDocumentationPartsResponse pResponseStatus_ =
  GetDocumentationPartsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsItems :: Lens.Lens' GetDocumentationPartsResponse (Lude.Maybe [DocumentationPart])
gdprsItems = Lens.lens (items :: GetDocumentationPartsResponse -> Lude.Maybe [DocumentationPart]) (\s a -> s {items = a} :: GetDocumentationPartsResponse)
{-# DEPRECATED gdprsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsPosition :: Lens.Lens' GetDocumentationPartsResponse (Lude.Maybe Lude.Text)
gdprsPosition = Lens.lens (position :: GetDocumentationPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDocumentationPartsResponse)
{-# DEPRECATED gdprsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsResponseStatus :: Lens.Lens' GetDocumentationPartsResponse Lude.Int
gdprsResponseStatus = Lens.lens (responseStatus :: GetDocumentationPartsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentationPartsResponse)
{-# DEPRECATED gdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
