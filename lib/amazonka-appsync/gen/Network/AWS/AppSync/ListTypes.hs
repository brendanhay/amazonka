{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the types for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListTypes
  ( -- * Creating a request
    ListTypes (..),
    mkListTypes,

    -- ** Request lenses
    ltNextToken,
    ltMaxResults,
    ltApiId,
    ltFormat,

    -- * Destructuring the response
    ListTypesResponse (..),
    mkListTypesResponse,

    -- ** Response lenses
    ltrsTypes,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypes' smart constructor.
data ListTypes = ListTypes'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    apiId :: Lude.Text,
    format :: TypeDefinitionFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypes' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'format' - The type format: SDL or JSON.
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListTypes ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  ListTypes
mkListTypes pApiId_ pFormat_ =
  ListTypes'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTypes (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypes)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTypes (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypes)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltApiId :: Lens.Lens' ListTypes Lude.Text
ltApiId = Lens.lens (apiId :: ListTypes -> Lude.Text) (\s a -> s {apiId = a} :: ListTypes)
{-# DEPRECATED ltApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltFormat :: Lens.Lens' ListTypes TypeDefinitionFormat
ltFormat = Lens.lens (format :: ListTypes -> TypeDefinitionFormat) (\s a -> s {format = a} :: ListTypes)
{-# DEPRECATED ltFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Page.AWSPager ListTypes where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTypes where
  type Rs ListTypes = ListTypesResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTypesResponse'
            Lude.<$> (x Lude..?> "types" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListTypes where
  toPath ListTypes' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/types"]

instance Lude.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults,
        "format" Lude.=: format
      ]

-- | /See:/ 'mkListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { types ::
      Lude.Maybe [Type],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTypesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'types' - The @Type@ objects.
mkListTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypesResponse
mkListTypesResponse pResponseStatus_ =
  ListTypesResponse'
    { types = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Type@ objects.
--
-- /Note:/ Consider using 'types' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTypes :: Lens.Lens' ListTypesResponse (Lude.Maybe [Type])
ltrsTypes = Lens.lens (types :: ListTypesResponse -> Lude.Maybe [Type]) (\s a -> s {types = a} :: ListTypesResponse)
{-# DEPRECATED ltrsTypes "Use generic-lens or generic-optics with 'types' instead." #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTypesResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypesResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTypesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
