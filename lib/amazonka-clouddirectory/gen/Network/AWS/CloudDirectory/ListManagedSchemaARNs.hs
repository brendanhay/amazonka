{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListManagedSchemaARNs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each managed schema. If a major version ARN is provided as SchemaArn, the minor version revisions in that family are listed instead.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListManagedSchemaARNs
  ( -- * Creating a request
    ListManagedSchemaARNs (..),
    mkListManagedSchemaARNs,

    -- ** Request lenses
    lmsaNextToken,
    lmsaSchemaARN,
    lmsaMaxResults,

    -- * Destructuring the response
    ListManagedSchemaARNsResponse (..),
    mkListManagedSchemaARNsResponse,

    -- ** Response lenses
    lmsarsSchemaARNs,
    lmsarsNextToken,
    lmsarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListManagedSchemaARNs' smart constructor.
data ListManagedSchemaARNs = ListManagedSchemaARNs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    schemaARN :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListManagedSchemaARNs' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
mkListManagedSchemaARNs ::
  ListManagedSchemaARNs
mkListManagedSchemaARNs =
  ListManagedSchemaARNs'
    { nextToken = Lude.Nothing,
      schemaARN = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaNextToken :: Lens.Lens' ListManagedSchemaARNs (Lude.Maybe Lude.Text)
lmsaNextToken = Lens.lens (nextToken :: ListManagedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListManagedSchemaARNs)
{-# DEPRECATED lmsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaSchemaARN :: Lens.Lens' ListManagedSchemaARNs (Lude.Maybe Lude.Text)
lmsaSchemaARN = Lens.lens (schemaARN :: ListManagedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: ListManagedSchemaARNs)
{-# DEPRECATED lmsaSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaMaxResults :: Lens.Lens' ListManagedSchemaARNs (Lude.Maybe Lude.Natural)
lmsaMaxResults = Lens.lens (maxResults :: ListManagedSchemaARNs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListManagedSchemaARNs)
{-# DEPRECATED lmsaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListManagedSchemaARNs where
  page rq rs
    | Page.stop (rs Lens.^. lmsarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmsarsSchemaARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmsaNextToken Lens..~ rs Lens.^. lmsarsNextToken

instance Lude.AWSRequest ListManagedSchemaARNs where
  type Rs ListManagedSchemaARNs = ListManagedSchemaARNsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListManagedSchemaARNsResponse'
            Lude.<$> (x Lude..?> "SchemaArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListManagedSchemaARNs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListManagedSchemaARNs where
  toJSON ListManagedSchemaARNs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SchemaArn" Lude..=) Lude.<$> schemaARN,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListManagedSchemaARNs where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/managed"

instance Lude.ToQuery ListManagedSchemaARNs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListManagedSchemaARNsResponse' smart constructor.
data ListManagedSchemaARNsResponse = ListManagedSchemaARNsResponse'
  { schemaARNs ::
      Lude.Maybe [Lude.Text],
    nextToken ::
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

-- | Creates a value of 'ListManagedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
-- * 'schemaARNs' - The ARNs for all AWS managed schemas.
mkListManagedSchemaARNsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListManagedSchemaARNsResponse
mkListManagedSchemaARNsResponse pResponseStatus_ =
  ListManagedSchemaARNsResponse'
    { schemaARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs for all AWS managed schemas.
--
-- /Note:/ Consider using 'schemaARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarsSchemaARNs :: Lens.Lens' ListManagedSchemaARNsResponse (Lude.Maybe [Lude.Text])
lmsarsSchemaARNs = Lens.lens (schemaARNs :: ListManagedSchemaARNsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {schemaARNs = a} :: ListManagedSchemaARNsResponse)
{-# DEPRECATED lmsarsSchemaARNs "Use generic-lens or generic-optics with 'schemaARNs' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarsNextToken :: Lens.Lens' ListManagedSchemaARNsResponse (Lude.Maybe Lude.Text)
lmsarsNextToken = Lens.lens (nextToken :: ListManagedSchemaARNsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListManagedSchemaARNsResponse)
{-# DEPRECATED lmsarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarsResponseStatus :: Lens.Lens' ListManagedSchemaARNsResponse Lude.Int
lmsarsResponseStatus = Lens.lens (responseStatus :: ListManagedSchemaARNsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListManagedSchemaARNsResponse)
{-# DEPRECATED lmsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
