{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitionVersions
  ( -- * Creating a request
    ListFunctionDefinitionVersions (..),
    mkListFunctionDefinitionVersions,

    -- ** Request lenses
    lfdvNextToken,
    lfdvFunctionDefinitionId,
    lfdvMaxResults,

    -- * Destructuring the response
    ListFunctionDefinitionVersionsResponse (..),
    mkListFunctionDefinitionVersionsResponse,

    -- ** Response lenses
    lfdvrsVersions,
    lfdvrsNextToken,
    lfdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListFunctionDefinitionVersions ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  ListFunctionDefinitionVersions
mkListFunctionDefinitionVersions pFunctionDefinitionId_ =
  ListFunctionDefinitionVersions'
    { nextToken = Lude.Nothing,
      functionDefinitionId = pFunctionDefinitionId_,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvNextToken :: Lens.Lens' ListFunctionDefinitionVersions (Lude.Maybe Lude.Text)
lfdvNextToken = Lens.lens (nextToken :: ListFunctionDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctionDefinitionVersions)
{-# DEPRECATED lfdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvFunctionDefinitionId :: Lens.Lens' ListFunctionDefinitionVersions Lude.Text
lfdvFunctionDefinitionId = Lens.lens (functionDefinitionId :: ListFunctionDefinitionVersions -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: ListFunctionDefinitionVersions)
{-# DEPRECATED lfdvFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvMaxResults :: Lens.Lens' ListFunctionDefinitionVersions (Lude.Maybe Lude.Text)
lfdvMaxResults = Lens.lens (maxResults :: ListFunctionDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListFunctionDefinitionVersions)
{-# DEPRECATED lfdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFunctionDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lfdvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfdvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfdvNextToken Lens..~ rs Lens.^. lfdvrsNextToken

instance Lude.AWSRequest ListFunctionDefinitionVersions where
  type
    Rs ListFunctionDefinitionVersions =
      ListFunctionDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctionDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListFunctionDefinitionVersions where
  toPath ListFunctionDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListFunctionDefinitionVersions where
  toQuery ListFunctionDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Lude.Maybe [VersionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - Information about a version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListFunctionDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionDefinitionVersionsResponse
mkListFunctionDefinitionVersionsResponse pResponseStatus_ =
  ListFunctionDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrsVersions :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lfdvrsVersions = Lens.lens (versions :: ListFunctionDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListFunctionDefinitionVersionsResponse)
{-# DEPRECATED lfdvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrsNextToken :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lfdvrsNextToken = Lens.lens (nextToken :: ListFunctionDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctionDefinitionVersionsResponse)
{-# DEPRECATED lfdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrsResponseStatus :: Lens.Lens' ListFunctionDefinitionVersionsResponse Lude.Int
lfdvrsResponseStatus = Lens.lens (responseStatus :: ListFunctionDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionDefinitionVersionsResponse)
{-# DEPRECATED lfdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
