{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gdpsPath,
    gdpsLocationStatus,
    gdpsNameQuery,
    gdpsLimit,
    gdpsRestAPIId,
    gdpsType,
    gdpsPosition,

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
  { -- | The path of API entities of the to-be-retrieved documentation parts.
    path :: Lude.Maybe Lude.Text,
    -- | The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
    locationStatus :: Lude.Maybe LocationStatusType,
    -- | The name of API entities of the to-be-retrieved documentation parts.
    nameQuery :: Lude.Maybe Lude.Text,
    -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The type of API entities of the to-be-retrieved documentation parts.
    type' :: Lude.Maybe DocumentationPartType,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentationParts' with the minimum fields required to make a request.
--
-- * 'path' - The path of API entities of the to-be-retrieved documentation parts.
-- * 'locationStatus' - The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
-- * 'nameQuery' - The name of API entities of the to-be-retrieved documentation parts.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'type'' - The type of API entities of the to-be-retrieved documentation parts.
-- * 'position' - The current pagination position in the paged result set.
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
      restAPIId = pRestAPIId_,
      type' = Lude.Nothing,
      position = Lude.Nothing
    }

-- | The path of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsPath :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpsPath = Lens.lens (path :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
--
-- /Note:/ Consider using 'locationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsLocationStatus :: Lens.Lens' GetDocumentationParts (Lude.Maybe LocationStatusType)
gdpsLocationStatus = Lens.lens (locationStatus :: GetDocumentationParts -> Lude.Maybe LocationStatusType) (\s a -> s {locationStatus = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsLocationStatus "Use generic-lens or generic-optics with 'locationStatus' instead." #-}

-- | The name of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsNameQuery :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpsNameQuery = Lens.lens (nameQuery :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {nameQuery = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsNameQuery "Use generic-lens or generic-optics with 'nameQuery' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsLimit :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Int)
gdpsLimit = Lens.lens (limit :: GetDocumentationParts -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsRestAPIId :: Lens.Lens' GetDocumentationParts Lude.Text
gdpsRestAPIId = Lens.lens (restAPIId :: GetDocumentationParts -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The type of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsType :: Lens.Lens' GetDocumentationParts (Lude.Maybe DocumentationPartType)
gdpsType = Lens.lens (type' :: GetDocumentationParts -> Lude.Maybe DocumentationPartType) (\s a -> s {type' = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsPosition :: Lens.Lens' GetDocumentationParts (Lude.Maybe Lude.Text)
gdpsPosition = Lens.lens (position :: GetDocumentationParts -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDocumentationParts)
{-# DEPRECATED gdpsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetDocumentationParts where
  page rq rs
    | Page.stop (rs Lens.^. gdprsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gdprsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdpsPosition Lens..~ rs Lens.^. gdprsPosition

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
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [DocumentationPart],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentationPartsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
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
