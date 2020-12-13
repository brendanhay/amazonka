{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListResourceDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a resource definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitionVersions
  ( -- * Creating a request
    ListResourceDefinitionVersions (..),
    mkListResourceDefinitionVersions,

    -- ** Request lenses
    lrdvResourceDefinitionId,
    lrdvNextToken,
    lrdvMaxResults,

    -- * Destructuring the response
    ListResourceDefinitionVersionsResponse (..),
    mkListResourceDefinitionVersionsResponse,

    -- ** Response lenses
    lrdvrsVersions,
    lrdvrsNextToken,
    lrdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResourceDefinitionVersions' smart constructor.
data ListResourceDefinitionVersions = ListResourceDefinitionVersions'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionId' - The ID of the resource definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListResourceDefinitionVersions ::
  -- | 'resourceDefinitionId'
  Lude.Text ->
  ListResourceDefinitionVersions
mkListResourceDefinitionVersions pResourceDefinitionId_ =
  ListResourceDefinitionVersions'
    { resourceDefinitionId =
        pResourceDefinitionId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvResourceDefinitionId :: Lens.Lens' ListResourceDefinitionVersions Lude.Text
lrdvResourceDefinitionId = Lens.lens (resourceDefinitionId :: ListResourceDefinitionVersions -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: ListResourceDefinitionVersions)
{-# DEPRECATED lrdvResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvNextToken :: Lens.Lens' ListResourceDefinitionVersions (Lude.Maybe Lude.Text)
lrdvNextToken = Lens.lens (nextToken :: ListResourceDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDefinitionVersions)
{-# DEPRECATED lrdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvMaxResults :: Lens.Lens' ListResourceDefinitionVersions (Lude.Maybe Lude.Text)
lrdvMaxResults = Lens.lens (maxResults :: ListResourceDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListResourceDefinitionVersions)
{-# DEPRECATED lrdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListResourceDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lrdvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrdvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrdvNextToken Lens..~ rs Lens.^. lrdvrsNextToken

instance Lude.AWSRequest ListResourceDefinitionVersions where
  type
    Rs ListResourceDefinitionVersions =
      ListResourceDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListResourceDefinitionVersions where
  toPath ListResourceDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListResourceDefinitionVersions where
  toQuery ListResourceDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListResourceDefinitionVersionsResponse' smart constructor.
data ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Lude.Maybe [VersionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - Information about a version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListResourceDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceDefinitionVersionsResponse
mkListResourceDefinitionVersionsResponse pResponseStatus_ =
  ListResourceDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrsVersions :: Lens.Lens' ListResourceDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lrdvrsVersions = Lens.lens (versions :: ListResourceDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListResourceDefinitionVersionsResponse)
{-# DEPRECATED lrdvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrsNextToken :: Lens.Lens' ListResourceDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lrdvrsNextToken = Lens.lens (nextToken :: ListResourceDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDefinitionVersionsResponse)
{-# DEPRECATED lrdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrsResponseStatus :: Lens.Lens' ListResourceDefinitionVersionsResponse Lude.Int
lrdvrsResponseStatus = Lens.lens (responseStatus :: ListResourceDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceDefinitionVersionsResponse)
{-# DEPRECATED lrdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
