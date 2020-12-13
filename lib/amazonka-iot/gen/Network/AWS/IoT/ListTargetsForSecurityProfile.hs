{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTargetsForSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets (thing groups) associated with a given Device Defender security profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForSecurityProfile
  ( -- * Creating a request
    ListTargetsForSecurityProfile (..),
    mkListTargetsForSecurityProfile,

    -- ** Request lenses
    ltfspNextToken,
    ltfspSecurityProfileName,
    ltfspMaxResults,

    -- * Destructuring the response
    ListTargetsForSecurityProfileResponse (..),
    mkListTargetsForSecurityProfileResponse,

    -- ** Response lenses
    ltfsprsSecurityProfileTargets,
    ltfsprsNextToken,
    ltfsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTargetsForSecurityProfile' smart constructor.
data ListTargetsForSecurityProfile = ListTargetsForSecurityProfile'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The security profile.
    securityProfileName :: Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsForSecurityProfile' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'securityProfileName' - The security profile.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListTargetsForSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  ListTargetsForSecurityProfile
mkListTargetsForSecurityProfile pSecurityProfileName_ =
  ListTargetsForSecurityProfile'
    { nextToken = Lude.Nothing,
      securityProfileName = pSecurityProfileName_,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspNextToken :: Lens.Lens' ListTargetsForSecurityProfile (Lude.Maybe Lude.Text)
ltfspNextToken = Lens.lens (nextToken :: ListTargetsForSecurityProfile -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsForSecurityProfile)
{-# DEPRECATED ltfspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspSecurityProfileName :: Lens.Lens' ListTargetsForSecurityProfile Lude.Text
ltfspSecurityProfileName = Lens.lens (securityProfileName :: ListTargetsForSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: ListTargetsForSecurityProfile)
{-# DEPRECATED ltfspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspMaxResults :: Lens.Lens' ListTargetsForSecurityProfile (Lude.Maybe Lude.Natural)
ltfspMaxResults = Lens.lens (maxResults :: ListTargetsForSecurityProfile -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTargetsForSecurityProfile)
{-# DEPRECATED ltfspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTargetsForSecurityProfile where
  page rq rs
    | Page.stop (rs Lens.^. ltfsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfsprsSecurityProfileTargets) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfspNextToken Lens..~ rs Lens.^. ltfsprsNextToken

instance Lude.AWSRequest ListTargetsForSecurityProfile where
  type
    Rs ListTargetsForSecurityProfile =
      ListTargetsForSecurityProfileResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTargetsForSecurityProfileResponse'
            Lude.<$> (x Lude..?> "securityProfileTargets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTargetsForSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTargetsForSecurityProfile where
  toPath ListTargetsForSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName, "/targets"]

instance Lude.ToQuery ListTargetsForSecurityProfile where
  toQuery ListTargetsForSecurityProfile' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListTargetsForSecurityProfileResponse' smart constructor.
data ListTargetsForSecurityProfileResponse = ListTargetsForSecurityProfileResponse'
  { -- | The thing groups to which the security profile is attached.
    securityProfileTargets :: Lude.Maybe [SecurityProfileTarget],
    -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsForSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'securityProfileTargets' - The thing groups to which the security profile is attached.
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListTargetsForSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTargetsForSecurityProfileResponse
mkListTargetsForSecurityProfileResponse pResponseStatus_ =
  ListTargetsForSecurityProfileResponse'
    { securityProfileTargets =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing groups to which the security profile is attached.
--
-- /Note:/ Consider using 'securityProfileTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprsSecurityProfileTargets :: Lens.Lens' ListTargetsForSecurityProfileResponse (Lude.Maybe [SecurityProfileTarget])
ltfsprsSecurityProfileTargets = Lens.lens (securityProfileTargets :: ListTargetsForSecurityProfileResponse -> Lude.Maybe [SecurityProfileTarget]) (\s a -> s {securityProfileTargets = a} :: ListTargetsForSecurityProfileResponse)
{-# DEPRECATED ltfsprsSecurityProfileTargets "Use generic-lens or generic-optics with 'securityProfileTargets' instead." #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprsNextToken :: Lens.Lens' ListTargetsForSecurityProfileResponse (Lude.Maybe Lude.Text)
ltfsprsNextToken = Lens.lens (nextToken :: ListTargetsForSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsForSecurityProfileResponse)
{-# DEPRECATED ltfsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprsResponseStatus :: Lens.Lens' ListTargetsForSecurityProfileResponse Lude.Int
ltfsprsResponseStatus = Lens.lens (responseStatus :: ListTargetsForSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTargetsForSecurityProfileResponse)
{-# DEPRECATED ltfsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
