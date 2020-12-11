{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a connector definition, which are containers for connectors. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitionVersions
  ( -- * Creating a request
    ListConnectorDefinitionVersions (..),
    mkListConnectorDefinitionVersions,

    -- ** Request lenses
    lcdvNextToken,
    lcdvMaxResults,
    lcdvConnectorDefinitionId,

    -- * Destructuring the response
    ListConnectorDefinitionVersionsResponse (..),
    mkListConnectorDefinitionVersionsResponse,

    -- ** Response lenses
    lcdvsrsVersions,
    lcdvsrsNextToken,
    lcdvsrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConnectorDefinitionVersions' smart constructor.
data ListConnectorDefinitionVersions = ListConnectorDefinitionVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Text,
    connectorDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConnectorDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'connectorDefinitionId' - The ID of the connector definition.
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListConnectorDefinitionVersions ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  ListConnectorDefinitionVersions
mkListConnectorDefinitionVersions pConnectorDefinitionId_ =
  ListConnectorDefinitionVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      connectorDefinitionId = pConnectorDefinitionId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvNextToken :: Lens.Lens' ListConnectorDefinitionVersions (Lude.Maybe Lude.Text)
lcdvNextToken = Lens.lens (nextToken :: ListConnectorDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConnectorDefinitionVersions)
{-# DEPRECATED lcdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvMaxResults :: Lens.Lens' ListConnectorDefinitionVersions (Lude.Maybe Lude.Text)
lcdvMaxResults = Lens.lens (maxResults :: ListConnectorDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListConnectorDefinitionVersions)
{-# DEPRECATED lcdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvConnectorDefinitionId :: Lens.Lens' ListConnectorDefinitionVersions Lude.Text
lcdvConnectorDefinitionId = Lens.lens (connectorDefinitionId :: ListConnectorDefinitionVersions -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: ListConnectorDefinitionVersions)
{-# DEPRECATED lcdvConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Page.AWSPager ListConnectorDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lcdvsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcdvsrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcdvNextToken Lens..~ rs Lens.^. lcdvsrsNextToken

instance Lude.AWSRequest ListConnectorDefinitionVersions where
  type
    Rs ListConnectorDefinitionVersions =
      ListConnectorDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConnectorDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListConnectorDefinitionVersions where
  toPath ListConnectorDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListConnectorDefinitionVersions where
  toQuery ListConnectorDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListConnectorDefinitionVersionsResponse' smart constructor.
data ListConnectorDefinitionVersionsResponse = ListConnectorDefinitionVersionsResponse'
  { versions ::
      Lude.Maybe
        [VersionInformation],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListConnectorDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'versions' - Information about a version.
mkListConnectorDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConnectorDefinitionVersionsResponse
mkListConnectorDefinitionVersionsResponse pResponseStatus_ =
  ListConnectorDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsrsVersions :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lcdvsrsVersions = Lens.lens (versions :: ListConnectorDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListConnectorDefinitionVersionsResponse)
{-# DEPRECATED lcdvsrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsrsNextToken :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lcdvsrsNextToken = Lens.lens (nextToken :: ListConnectorDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConnectorDefinitionVersionsResponse)
{-# DEPRECATED lcdvsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsrsResponseStatus :: Lens.Lens' ListConnectorDefinitionVersionsResponse Lude.Int
lcdvsrsResponseStatus = Lens.lens (responseStatus :: ListConnectorDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConnectorDefinitionVersionsResponse)
{-# DEPRECATED lcdvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
