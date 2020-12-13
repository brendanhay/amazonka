{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a logger definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitionVersions
  ( -- * Creating a request
    ListLoggerDefinitionVersions (..),
    mkListLoggerDefinitionVersions,

    -- ** Request lenses
    lldvLoggerDefinitionId,
    lldvNextToken,
    lldvMaxResults,

    -- * Destructuring the response
    ListLoggerDefinitionVersionsResponse (..),
    mkListLoggerDefinitionVersionsResponse,

    -- ** Response lenses
    lldvrsVersions,
    lldvrsNextToken,
    lldvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLoggerDefinitionVersions' smart constructor.
data ListLoggerDefinitionVersions = ListLoggerDefinitionVersions'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLoggerDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'loggerDefinitionId' - The ID of the logger definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListLoggerDefinitionVersions ::
  -- | 'loggerDefinitionId'
  Lude.Text ->
  ListLoggerDefinitionVersions
mkListLoggerDefinitionVersions pLoggerDefinitionId_ =
  ListLoggerDefinitionVersions'
    { loggerDefinitionId =
        pLoggerDefinitionId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvLoggerDefinitionId :: Lens.Lens' ListLoggerDefinitionVersions Lude.Text
lldvLoggerDefinitionId = Lens.lens (loggerDefinitionId :: ListLoggerDefinitionVersions -> Lude.Text) (\s a -> s {loggerDefinitionId = a} :: ListLoggerDefinitionVersions)
{-# DEPRECATED lldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvNextToken :: Lens.Lens' ListLoggerDefinitionVersions (Lude.Maybe Lude.Text)
lldvNextToken = Lens.lens (nextToken :: ListLoggerDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLoggerDefinitionVersions)
{-# DEPRECATED lldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvMaxResults :: Lens.Lens' ListLoggerDefinitionVersions (Lude.Maybe Lude.Text)
lldvMaxResults = Lens.lens (maxResults :: ListLoggerDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListLoggerDefinitionVersions)
{-# DEPRECATED lldvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListLoggerDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lldvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lldvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lldvNextToken Lens..~ rs Lens.^. lldvrsNextToken

instance Lude.AWSRequest ListLoggerDefinitionVersions where
  type
    Rs ListLoggerDefinitionVersions =
      ListLoggerDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLoggerDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListLoggerDefinitionVersions where
  toPath ListLoggerDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/loggers/",
        Lude.toBS loggerDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListLoggerDefinitionVersions where
  toQuery ListLoggerDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListLoggerDefinitionVersionsResponse' smart constructor.
data ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Lude.Maybe [VersionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLoggerDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - Information about a version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListLoggerDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLoggerDefinitionVersionsResponse
mkListLoggerDefinitionVersionsResponse pResponseStatus_ =
  ListLoggerDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrsVersions :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lldvrsVersions = Lens.lens (versions :: ListLoggerDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListLoggerDefinitionVersionsResponse)
{-# DEPRECATED lldvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrsNextToken :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lldvrsNextToken = Lens.lens (nextToken :: ListLoggerDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLoggerDefinitionVersionsResponse)
{-# DEPRECATED lldvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrsResponseStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Lude.Int
lldvrsResponseStatus = Lens.lens (responseStatus :: ListLoggerDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLoggerDefinitionVersionsResponse)
{-# DEPRECATED lldvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
