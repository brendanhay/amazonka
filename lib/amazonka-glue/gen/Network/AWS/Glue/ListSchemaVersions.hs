{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schema versions that you have created, with minimal information. Schema versions in Deleted status will not be included in the results. Empty results will be returned if there are no schema versions available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemaVersions
  ( -- * Creating a request
    ListSchemaVersions (..),
    mkListSchemaVersions,

    -- ** Request lenses
    lsvNextToken,
    lsvMaxResults,
    lsvSchemaId,

    -- * Destructuring the response
    ListSchemaVersionsResponse (..),
    mkListSchemaVersionsResponse,

    -- ** Response lenses
    lsvrsSchemas,
    lsvrsNextToken,
    lsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSchemaVersions' smart constructor.
data ListSchemaVersions = ListSchemaVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    schemaId :: SchemaId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSchemaVersions' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
mkListSchemaVersions ::
  -- | 'schemaId'
  SchemaId ->
  ListSchemaVersions
mkListSchemaVersions pSchemaId_ =
  ListSchemaVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      schemaId = pSchemaId_
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvNextToken :: Lens.Lens' ListSchemaVersions (Lude.Maybe Lude.Text)
lsvNextToken = Lens.lens (nextToken :: ListSchemaVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemaVersions)
{-# DEPRECATED lsvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvMaxResults :: Lens.Lens' ListSchemaVersions (Lude.Maybe Lude.Natural)
lsvMaxResults = Lens.lens (maxResults :: ListSchemaVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSchemaVersions)
{-# DEPRECATED lsvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvSchemaId :: Lens.Lens' ListSchemaVersions SchemaId
lsvSchemaId = Lens.lens (schemaId :: ListSchemaVersions -> SchemaId) (\s a -> s {schemaId = a} :: ListSchemaVersions)
{-# DEPRECATED lsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

instance Page.AWSPager ListSchemaVersions where
  page rq rs
    | Page.stop (rs Lens.^. lsvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsvrsSchemas) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsvNextToken Lens..~ rs Lens.^. lsvrsNextToken

instance Lude.AWSRequest ListSchemaVersions where
  type Rs ListSchemaVersions = ListSchemaVersionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSchemaVersionsResponse'
            Lude.<$> (x Lude..?> "Schemas" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSchemaVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListSchemaVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSchemaVersions where
  toJSON ListSchemaVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("SchemaId" Lude..= schemaId)
          ]
      )

instance Lude.ToPath ListSchemaVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSchemaVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSchemaVersionsResponse' smart constructor.
data ListSchemaVersionsResponse = ListSchemaVersionsResponse'
  { schemas ::
      Lude.Maybe [SchemaVersionListItem],
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

-- | Creates a value of 'ListSchemaVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
-- * 'responseStatus' - The response status code.
-- * 'schemas' - An array of @SchemaVersionList@ objects containing details of each schema version.
mkListSchemaVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSchemaVersionsResponse
mkListSchemaVersionsResponse pResponseStatus_ =
  ListSchemaVersionsResponse'
    { schemas = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @SchemaVersionList@ objects containing details of each schema version.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrsSchemas :: Lens.Lens' ListSchemaVersionsResponse (Lude.Maybe [SchemaVersionListItem])
lsvrsSchemas = Lens.lens (schemas :: ListSchemaVersionsResponse -> Lude.Maybe [SchemaVersionListItem]) (\s a -> s {schemas = a} :: ListSchemaVersionsResponse)
{-# DEPRECATED lsvrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrsNextToken :: Lens.Lens' ListSchemaVersionsResponse (Lude.Maybe Lude.Text)
lsvrsNextToken = Lens.lens (nextToken :: ListSchemaVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemaVersionsResponse)
{-# DEPRECATED lsvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrsResponseStatus :: Lens.Lens' ListSchemaVersionsResponse Lude.Int
lsvrsResponseStatus = Lens.lens (responseStatus :: ListSchemaVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSchemaVersionsResponse)
{-# DEPRECATED lsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
