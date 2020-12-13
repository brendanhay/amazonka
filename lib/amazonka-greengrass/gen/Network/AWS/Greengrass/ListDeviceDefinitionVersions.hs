{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitionVersions
  ( -- * Creating a request
    ListDeviceDefinitionVersions (..),
    mkListDeviceDefinitionVersions,

    -- ** Request lenses
    lddvNextToken,
    lddvDeviceDefinitionId,
    lddvMaxResults,

    -- * Destructuring the response
    ListDeviceDefinitionVersionsResponse (..),
    mkListDeviceDefinitionVersionsResponse,

    -- ** Response lenses
    lddvrsVersions,
    lddvrsNextToken,
    lddvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'deviceDefinitionId' - The ID of the device definition.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListDeviceDefinitionVersions ::
  -- | 'deviceDefinitionId'
  Lude.Text ->
  ListDeviceDefinitionVersions
mkListDeviceDefinitionVersions pDeviceDefinitionId_ =
  ListDeviceDefinitionVersions'
    { nextToken = Lude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvNextToken :: Lens.Lens' ListDeviceDefinitionVersions (Lude.Maybe Lude.Text)
lddvNextToken = Lens.lens (nextToken :: ListDeviceDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceDefinitionVersions)
{-# DEPRECATED lddvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvDeviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Lude.Text
lddvDeviceDefinitionId = Lens.lens (deviceDefinitionId :: ListDeviceDefinitionVersions -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: ListDeviceDefinitionVersions)
{-# DEPRECATED lddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvMaxResults :: Lens.Lens' ListDeviceDefinitionVersions (Lude.Maybe Lude.Text)
lddvMaxResults = Lens.lens (maxResults :: ListDeviceDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListDeviceDefinitionVersions)
{-# DEPRECATED lddvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDeviceDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lddvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lddvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lddvNextToken Lens..~ rs Lens.^. lddvrsNextToken

instance Lude.AWSRequest ListDeviceDefinitionVersions where
  type
    Rs ListDeviceDefinitionVersions =
      ListDeviceDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeviceDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDeviceDefinitionVersions where
  toPath ListDeviceDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/devices/",
        Lude.toBS deviceDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListDeviceDefinitionVersions where
  toQuery ListDeviceDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Lude.Maybe [VersionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - Information about a version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListDeviceDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeviceDefinitionVersionsResponse
mkListDeviceDefinitionVersionsResponse pResponseStatus_ =
  ListDeviceDefinitionVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrsVersions :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lddvrsVersions = Lens.lens (versions :: ListDeviceDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListDeviceDefinitionVersionsResponse)
{-# DEPRECATED lddvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrsNextToken :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lddvrsNextToken = Lens.lens (nextToken :: ListDeviceDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceDefinitionVersionsResponse)
{-# DEPRECATED lddvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrsResponseStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Lude.Int
lddvrsResponseStatus = Lens.lens (responseStatus :: ListDeviceDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeviceDefinitionVersionsResponse)
{-# DEPRECATED lddvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
