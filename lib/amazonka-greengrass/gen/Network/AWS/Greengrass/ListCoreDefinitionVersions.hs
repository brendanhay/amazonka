{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a core definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitionVersions
  ( -- * Creating a request
    ListCoreDefinitionVersions (..),
    mkListCoreDefinitionVersions,

    -- ** Request lenses
    lcdvsNextToken,
    lcdvsMaxResults,
    lcdvsCoreDefinitionId,

    -- * Destructuring the response
    ListCoreDefinitionVersionsResponse (..),
    mkListCoreDefinitionVersionsResponse,

    -- ** Response lenses
    lcdvrsVersions,
    lcdvrsNextToken,
    lcdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text,
    coreDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCoreDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'coreDefinitionId' - The ID of the core definition.
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListCoreDefinitionVersions ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  ListCoreDefinitionVersions
mkListCoreDefinitionVersions pCoreDefinitionId_ =
  ListCoreDefinitionVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsNextToken :: Lens.Lens' ListCoreDefinitionVersions (Lude.Maybe Lude.Text)
lcdvsNextToken = Lens.lens (nextToken :: ListCoreDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCoreDefinitionVersions)
{-# DEPRECATED lcdvsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsMaxResults :: Lens.Lens' ListCoreDefinitionVersions (Lude.Maybe Lude.Text)
lcdvsMaxResults = Lens.lens (maxResults :: ListCoreDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListCoreDefinitionVersions)
{-# DEPRECATED lcdvsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsCoreDefinitionId :: Lens.Lens' ListCoreDefinitionVersions Lude.Text
lcdvsCoreDefinitionId = Lens.lens (coreDefinitionId :: ListCoreDefinitionVersions -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: ListCoreDefinitionVersions)
{-# DEPRECATED lcdvsCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Page.AWSPager ListCoreDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lcdvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcdvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcdvsNextToken Lens..~ rs Lens.^. lcdvrsNextToken

instance Lude.AWSRequest ListCoreDefinitionVersions where
  type
    Rs ListCoreDefinitionVersions =
      ListCoreDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCoreDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCoreDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListCoreDefinitionVersions where
  toPath ListCoreDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/cores/",
        Lude.toBS coreDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListCoreDefinitionVersions where
  toQuery ListCoreDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
  { versions ::
      Lude.Maybe
        [VersionInformation],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCoreDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'versions' - Information about a version.
mkListCoreDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCoreDefinitionVersionsResponse
mkListCoreDefinitionVersionsResponse pResponseStatus_ =
  ListCoreDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrsVersions :: Lens.Lens' ListCoreDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lcdvrsVersions = Lens.lens (versions :: ListCoreDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListCoreDefinitionVersionsResponse)
{-# DEPRECATED lcdvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrsNextToken :: Lens.Lens' ListCoreDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lcdvrsNextToken = Lens.lens (nextToken :: ListCoreDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCoreDefinitionVersionsResponse)
{-# DEPRECATED lcdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrsResponseStatus :: Lens.Lens' ListCoreDefinitionVersionsResponse Lude.Int
lcdvrsResponseStatus = Lens.lens (responseStatus :: ListCoreDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCoreDefinitionVersionsResponse)
{-# DEPRECATED lcdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
