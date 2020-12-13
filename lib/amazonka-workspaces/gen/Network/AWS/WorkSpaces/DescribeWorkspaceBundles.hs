{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the available WorkSpace bundles.
--
-- You can filter the results using either bundle ID or owner, but not both.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
  ( -- * Creating a request
    DescribeWorkspaceBundles (..),
    mkDescribeWorkspaceBundles,

    -- ** Request lenses
    dwbBundleIds,
    dwbOwner,
    dwbNextToken,

    -- * Destructuring the response
    DescribeWorkspaceBundlesResponse (..),
    mkDescribeWorkspaceBundlesResponse,

    -- ** Response lenses
    dwbrsBundles,
    dwbrsNextToken,
    dwbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaceBundles' smart constructor.
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
  { -- | The identifiers of the bundles. You cannot combine this parameter with any other filter.
    bundleIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The owner of the bundles. You cannot combine this parameter with any other filter.
    --
    -- Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
    owner :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceBundles' with the minimum fields required to make a request.
--
-- * 'bundleIds' - The identifiers of the bundles. You cannot combine this parameter with any other filter.
-- * 'owner' - The owner of the bundles. You cannot combine this parameter with any other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
-- * 'nextToken' - The token for the next set of results. (You received this token from a previous call.)
mkDescribeWorkspaceBundles ::
  DescribeWorkspaceBundles
mkDescribeWorkspaceBundles =
  DescribeWorkspaceBundles'
    { bundleIds = Lude.Nothing,
      owner = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The identifiers of the bundles. You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'bundleIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbBundleIds :: Lens.Lens' DescribeWorkspaceBundles (Lude.Maybe (Lude.NonEmpty Lude.Text))
dwbBundleIds = Lens.lens (bundleIds :: DescribeWorkspaceBundles -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {bundleIds = a} :: DescribeWorkspaceBundles)
{-# DEPRECATED dwbBundleIds "Use generic-lens or generic-optics with 'bundleIds' instead." #-}

-- | The owner of the bundles. You cannot combine this parameter with any other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbOwner :: Lens.Lens' DescribeWorkspaceBundles (Lude.Maybe Lude.Text)
dwbOwner = Lens.lens (owner :: DescribeWorkspaceBundles -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: DescribeWorkspaceBundles)
{-# DEPRECATED dwbOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The token for the next set of results. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbNextToken :: Lens.Lens' DescribeWorkspaceBundles (Lude.Maybe Lude.Text)
dwbNextToken = Lens.lens (nextToken :: DescribeWorkspaceBundles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceBundles)
{-# DEPRECATED dwbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeWorkspaceBundles where
  page rq rs
    | Page.stop (rs Lens.^. dwbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dwbrsBundles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dwbNextToken Lens..~ rs Lens.^. dwbrsNextToken

instance Lude.AWSRequest DescribeWorkspaceBundles where
  type Rs DescribeWorkspaceBundles = DescribeWorkspaceBundlesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspaceBundlesResponse'
            Lude.<$> (x Lude..?> "Bundles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaceBundles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeWorkspaceBundles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaceBundles where
  toJSON DescribeWorkspaceBundles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BundleIds" Lude..=) Lude.<$> bundleIds,
            ("Owner" Lude..=) Lude.<$> owner,
            ("NextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath DescribeWorkspaceBundles where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaceBundles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspaceBundlesResponse' smart constructor.
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
  { -- | Information about the bundles.
    bundles :: Lude.Maybe [WorkspaceBundle],
    -- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceBundlesResponse' with the minimum fields required to make a request.
--
-- * 'bundles' - Information about the bundles.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspaceBundlesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspaceBundlesResponse
mkDescribeWorkspaceBundlesResponse pResponseStatus_ =
  DescribeWorkspaceBundlesResponse'
    { bundles = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the bundles.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrsBundles :: Lens.Lens' DescribeWorkspaceBundlesResponse (Lude.Maybe [WorkspaceBundle])
dwbrsBundles = Lens.lens (bundles :: DescribeWorkspaceBundlesResponse -> Lude.Maybe [WorkspaceBundle]) (\s a -> s {bundles = a} :: DescribeWorkspaceBundlesResponse)
{-# DEPRECATED dwbrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrsNextToken :: Lens.Lens' DescribeWorkspaceBundlesResponse (Lude.Maybe Lude.Text)
dwbrsNextToken = Lens.lens (nextToken :: DescribeWorkspaceBundlesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceBundlesResponse)
{-# DEPRECATED dwbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrsResponseStatus :: Lens.Lens' DescribeWorkspaceBundlesResponse Lude.Int
dwbrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspaceBundlesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspaceBundlesResponse)
{-# DEPRECATED dwbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
