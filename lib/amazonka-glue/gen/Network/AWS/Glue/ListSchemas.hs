{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schemas with minimal details. Schemas in Deleting status will not be included in the results. Empty results will be returned if there are no schemas available.
--
-- When the @RegistryId@ is not provided, all the schemas across registries will be part of the API response.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemas
  ( -- * Creating a request
    ListSchemas (..),
    mkListSchemas,

    -- ** Request lenses
    lsRegistryId,
    lsNextToken,
    lsMaxResults,

    -- * Destructuring the response
    ListSchemasResponse (..),
    mkListSchemasResponse,

    -- ** Response lenses
    lsrsSchemas,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
    registryId :: Lude.Maybe RegistryId,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSchemas' with the minimum fields required to make a request.
--
-- * 'registryId' - A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'maxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
mkListSchemas ::
  ListSchemas
mkListSchemas =
  ListSchemas'
    { registryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsRegistryId :: Lens.Lens' ListSchemas (Lude.Maybe RegistryId)
lsRegistryId = Lens.lens (registryId :: ListSchemas -> Lude.Maybe RegistryId) (\s a -> s {registryId = a} :: ListSchemas)
{-# DEPRECATED lsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSchemas (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListSchemas -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemas)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListSchemas (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListSchemas -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSchemas)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSchemas where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsSchemas) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListSchemas where
  type Rs ListSchemas = ListSchemasResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Lude.<$> (x Lude..?> "Schemas" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSchemas where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.ListSchemas" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegistryId" Lude..=) Lude.<$> registryId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSchemas where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSchemas where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | An array of @SchemaListItem@ objects containing details of each schema.
    schemas :: Lude.Maybe [SchemaListItem],
    -- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSchemasResponse' with the minimum fields required to make a request.
--
-- * 'schemas' - An array of @SchemaListItem@ objects containing details of each schema.
-- * 'nextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
-- * 'responseStatus' - The response status code.
mkListSchemasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSchemasResponse
mkListSchemasResponse pResponseStatus_ =
  ListSchemasResponse'
    { schemas = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @SchemaListItem@ objects containing details of each schema.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsSchemas :: Lens.Lens' ListSchemasResponse (Lude.Maybe [SchemaListItem])
lsrsSchemas = Lens.lens (schemas :: ListSchemasResponse -> Lude.Maybe [SchemaListItem]) (\s a -> s {schemas = a} :: ListSchemasResponse)
{-# DEPRECATED lsrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListSchemasResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListSchemasResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemasResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListSchemasResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListSchemasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSchemasResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
