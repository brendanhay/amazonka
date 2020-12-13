{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles you have created. You can use filters to list only those security profiles associated with a thing group or only those associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfiles
  ( -- * Creating a request
    ListSecurityProfiles (..),
    mkListSecurityProfiles,

    -- ** Request lenses
    lspNextToken,
    lspDimensionName,
    lspMaxResults,

    -- * Destructuring the response
    ListSecurityProfilesResponse (..),
    mkListSecurityProfilesResponse,

    -- ** Response lenses
    lsprsNextToken,
    lsprsSecurityProfileIdentifiers,
    lsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A filter to limit results to the security profiles that use the defined dimension.
    dimensionName :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityProfiles' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'dimensionName' - A filter to limit results to the security profiles that use the defined dimension.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListSecurityProfiles ::
  ListSecurityProfiles
mkListSecurityProfiles =
  ListSecurityProfiles'
    { nextToken = Lude.Nothing,
      dimensionName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListSecurityProfiles (Lude.Maybe Lude.Text)
lspNextToken = Lens.lens (nextToken :: ListSecurityProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfiles)
{-# DEPRECATED lspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit results to the security profiles that use the defined dimension.
--
-- /Note:/ Consider using 'dimensionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspDimensionName :: Lens.Lens' ListSecurityProfiles (Lude.Maybe Lude.Text)
lspDimensionName = Lens.lens (dimensionName :: ListSecurityProfiles -> Lude.Maybe Lude.Text) (\s a -> s {dimensionName = a} :: ListSecurityProfiles)
{-# DEPRECATED lspDimensionName "Use generic-lens or generic-optics with 'dimensionName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListSecurityProfiles (Lude.Maybe Lude.Natural)
lspMaxResults = Lens.lens (maxResults :: ListSecurityProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecurityProfiles)
{-# DEPRECATED lspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSecurityProfiles where
  page rq rs
    | Page.stop (rs Lens.^. lsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsprsSecurityProfileIdentifiers) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspNextToken Lens..~ rs Lens.^. lsprsNextToken

instance Lude.AWSRequest ListSecurityProfiles where
  type Rs ListSecurityProfiles = ListSecurityProfilesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "securityProfileIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecurityProfiles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSecurityProfiles where
  toPath = Lude.const "/security-profiles"

instance Lude.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "dimensionName" Lude.=: dimensionName,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of security profile identifiers (names and ARNs).
    securityProfileIdentifiers :: Lude.Maybe [SecurityProfileIdentifier],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'securityProfileIdentifiers' - A list of security profile identifiers (names and ARNs).
-- * 'responseStatus' - The response status code.
mkListSecurityProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecurityProfilesResponse
mkListSecurityProfilesResponse pResponseStatus_ =
  ListSecurityProfilesResponse'
    { nextToken = Lude.Nothing,
      securityProfileIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsNextToken :: Lens.Lens' ListSecurityProfilesResponse (Lude.Maybe Lude.Text)
lsprsNextToken = Lens.lens (nextToken :: ListSecurityProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of security profile identifiers (names and ARNs).
--
-- /Note:/ Consider using 'securityProfileIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsSecurityProfileIdentifiers :: Lens.Lens' ListSecurityProfilesResponse (Lude.Maybe [SecurityProfileIdentifier])
lsprsSecurityProfileIdentifiers = Lens.lens (securityProfileIdentifiers :: ListSecurityProfilesResponse -> Lude.Maybe [SecurityProfileIdentifier]) (\s a -> s {securityProfileIdentifiers = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsSecurityProfileIdentifiers "Use generic-lens or generic-optics with 'securityProfileIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsResponseStatus :: Lens.Lens' ListSecurityProfilesResponse Lude.Int
lsprsResponseStatus = Lens.lens (responseStatus :: ListSecurityProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
