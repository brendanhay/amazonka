{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the security profiles for the specified Amazon Connect instance.
--
-- For more information about security profiles, see <https://docs.aws.amazon.com/connect/latest/adminguide/connect-security-profiles.html Security Profiles> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityProfiles
  ( -- * Creating a request
    ListSecurityProfiles (..),
    mkListSecurityProfiles,

    -- ** Request lenses
    lspNextToken,
    lspMaxResults,
    lspInstanceId,

    -- * Destructuring the response
    ListSecurityProfilesResponse (..),
    mkListSecurityProfilesResponse,

    -- ** Response lenses
    lsprsNextToken,
    lsprsSecurityProfileSummaryList,
    lsprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityProfiles' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListSecurityProfiles ::
  -- | 'instanceId'
  Lude.Text ->
  ListSecurityProfiles
mkListSecurityProfiles pInstanceId_ =
  ListSecurityProfiles'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListSecurityProfiles (Lude.Maybe Lude.Text)
lspNextToken = Lens.lens (nextToken :: ListSecurityProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfiles)
{-# DEPRECATED lspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListSecurityProfiles (Lude.Maybe Lude.Natural)
lspMaxResults = Lens.lens (maxResults :: ListSecurityProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecurityProfiles)
{-# DEPRECATED lspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspInstanceId :: Lens.Lens' ListSecurityProfiles Lude.Text
lspInstanceId = Lens.lens (instanceId :: ListSecurityProfiles -> Lude.Text) (\s a -> s {instanceId = a} :: ListSecurityProfiles)
{-# DEPRECATED lspInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListSecurityProfiles where
  page rq rs
    | Page.stop (rs Lens.^. lsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsprsSecurityProfileSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspNextToken Lens..~ rs Lens.^. lsprsNextToken

instance Lude.AWSRequest ListSecurityProfiles where
  type Rs ListSecurityProfiles = ListSecurityProfilesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SecurityProfileSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecurityProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListSecurityProfiles where
  toPath ListSecurityProfiles' {..} =
    Lude.mconcat
      ["/security-profiles-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    securityProfileSummaryList ::
      Lude.Maybe
        [SecurityProfileSummary],
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

-- | Creates a value of 'ListSecurityProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'securityProfileSummaryList' - Information about the security profiles.
mkListSecurityProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecurityProfilesResponse
mkListSecurityProfilesResponse pResponseStatus_ =
  ListSecurityProfilesResponse'
    { nextToken = Lude.Nothing,
      securityProfileSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsNextToken :: Lens.Lens' ListSecurityProfilesResponse (Lude.Maybe Lude.Text)
lsprsNextToken = Lens.lens (nextToken :: ListSecurityProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the security profiles.
--
-- /Note:/ Consider using 'securityProfileSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsSecurityProfileSummaryList :: Lens.Lens' ListSecurityProfilesResponse (Lude.Maybe [SecurityProfileSummary])
lsprsSecurityProfileSummaryList = Lens.lens (securityProfileSummaryList :: ListSecurityProfilesResponse -> Lude.Maybe [SecurityProfileSummary]) (\s a -> s {securityProfileSummaryList = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsSecurityProfileSummaryList "Use generic-lens or generic-optics with 'securityProfileSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsResponseStatus :: Lens.Lens' ListSecurityProfilesResponse Lude.Int
lsprsResponseStatus = Lens.lens (responseStatus :: ListSecurityProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecurityProfilesResponse)
{-# DEPRECATED lsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
