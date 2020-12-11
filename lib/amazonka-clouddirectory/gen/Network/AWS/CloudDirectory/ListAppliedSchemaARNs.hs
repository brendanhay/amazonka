{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListAppliedSchemaARNs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists schema major versions applied to a directory. If @SchemaArn@ is provided, lists the minor version.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAppliedSchemaARNs
  ( -- * Creating a request
    ListAppliedSchemaARNs (..),
    mkListAppliedSchemaARNs,

    -- ** Request lenses
    lasaNextToken,
    lasaSchemaARN,
    lasaMaxResults,
    lasaDirectoryARN,

    -- * Destructuring the response
    ListAppliedSchemaARNsResponse (..),
    mkListAppliedSchemaARNsResponse,

    -- ** Response lenses
    lasarsSchemaARNs,
    lasarsNextToken,
    lasarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAppliedSchemaARNs' smart constructor.
data ListAppliedSchemaARNs = ListAppliedSchemaARNs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    schemaARN :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    directoryARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppliedSchemaARNs' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory you are listing.
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The response for @ListAppliedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
mkListAppliedSchemaARNs ::
  -- | 'directoryARN'
  Lude.Text ->
  ListAppliedSchemaARNs
mkListAppliedSchemaARNs pDirectoryARN_ =
  ListAppliedSchemaARNs'
    { nextToken = Lude.Nothing,
      schemaARN = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaNextToken :: Lens.Lens' ListAppliedSchemaARNs (Lude.Maybe Lude.Text)
lasaNextToken = Lens.lens (nextToken :: ListAppliedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppliedSchemaARNs)
{-# DEPRECATED lasaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response for @ListAppliedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaSchemaARN :: Lens.Lens' ListAppliedSchemaARNs (Lude.Maybe Lude.Text)
lasaSchemaARN = Lens.lens (schemaARN :: ListAppliedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: ListAppliedSchemaARNs)
{-# DEPRECATED lasaSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaMaxResults :: Lens.Lens' ListAppliedSchemaARNs (Lude.Maybe Lude.Natural)
lasaMaxResults = Lens.lens (maxResults :: ListAppliedSchemaARNs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAppliedSchemaARNs)
{-# DEPRECATED lasaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the directory you are listing.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaDirectoryARN :: Lens.Lens' ListAppliedSchemaARNs Lude.Text
lasaDirectoryARN = Lens.lens (directoryARN :: ListAppliedSchemaARNs -> Lude.Text) (\s a -> s {directoryARN = a} :: ListAppliedSchemaARNs)
{-# DEPRECATED lasaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

instance Page.AWSPager ListAppliedSchemaARNs where
  page rq rs
    | Page.stop (rs Lens.^. lasarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasarsSchemaARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasaNextToken Lens..~ rs Lens.^. lasarsNextToken

instance Lude.AWSRequest ListAppliedSchemaARNs where
  type Rs ListAppliedSchemaARNs = ListAppliedSchemaARNsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAppliedSchemaARNsResponse'
            Lude.<$> (x Lude..?> "SchemaArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAppliedSchemaARNs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListAppliedSchemaARNs where
  toJSON ListAppliedSchemaARNs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SchemaArn" Lude..=) Lude.<$> schemaARN,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("DirectoryArn" Lude..= directoryARN)
          ]
      )

instance Lude.ToPath ListAppliedSchemaARNs where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/applied"

instance Lude.ToQuery ListAppliedSchemaARNs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAppliedSchemaARNsResponse' smart constructor.
data ListAppliedSchemaARNsResponse = ListAppliedSchemaARNsResponse'
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

-- | Creates a value of 'ListAppliedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
-- * 'schemaARNs' - The ARNs of schemas that are applied to the directory.
mkListAppliedSchemaARNsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAppliedSchemaARNsResponse
mkListAppliedSchemaARNsResponse pResponseStatus_ =
  ListAppliedSchemaARNsResponse'
    { schemaARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs of schemas that are applied to the directory.
--
-- /Note:/ Consider using 'schemaARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarsSchemaARNs :: Lens.Lens' ListAppliedSchemaARNsResponse (Lude.Maybe [Lude.Text])
lasarsSchemaARNs = Lens.lens (schemaARNs :: ListAppliedSchemaARNsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {schemaARNs = a} :: ListAppliedSchemaARNsResponse)
{-# DEPRECATED lasarsSchemaARNs "Use generic-lens or generic-optics with 'schemaARNs' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarsNextToken :: Lens.Lens' ListAppliedSchemaARNsResponse (Lude.Maybe Lude.Text)
lasarsNextToken = Lens.lens (nextToken :: ListAppliedSchemaARNsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppliedSchemaARNsResponse)
{-# DEPRECATED lasarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarsResponseStatus :: Lens.Lens' ListAppliedSchemaARNsResponse Lude.Int
lasarsResponseStatus = Lens.lens (responseStatus :: ListAppliedSchemaARNsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAppliedSchemaARNsResponse)
{-# DEPRECATED lasarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
