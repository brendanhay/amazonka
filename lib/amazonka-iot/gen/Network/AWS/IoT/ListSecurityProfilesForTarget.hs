{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListSecurityProfilesForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles attached to a target (thing group).
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfilesForTarget
  ( -- * Creating a request
    ListSecurityProfilesForTarget (..),
    mkListSecurityProfilesForTarget,

    -- ** Request lenses
    lspftNextToken,
    lspftRecursive,
    lspftMaxResults,
    lspftSecurityProfileTargetARN,

    -- * Destructuring the response
    ListSecurityProfilesForTargetResponse (..),
    mkListSecurityProfilesForTargetResponse,

    -- ** Response lenses
    lspftrsNextToken,
    lspftrsSecurityProfileTargetMappings,
    lspftrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSecurityProfilesForTarget' smart constructor.
data ListSecurityProfilesForTarget = ListSecurityProfilesForTarget'
  { nextToken ::
      Lude.Maybe Lude.Text,
    recursive ::
      Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural,
    securityProfileTargetARN ::
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

-- | Creates a value of 'ListSecurityProfilesForTarget' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - The token for the next set of results.
-- * 'recursive' - If true, return child groups too.
-- * 'securityProfileTargetARN' - The ARN of the target (thing group) whose attached security profiles you want to get.
mkListSecurityProfilesForTarget ::
  -- | 'securityProfileTargetARN'
  Lude.Text ->
  ListSecurityProfilesForTarget
mkListSecurityProfilesForTarget pSecurityProfileTargetARN_ =
  ListSecurityProfilesForTarget'
    { nextToken = Lude.Nothing,
      recursive = Lude.Nothing,
      maxResults = Lude.Nothing,
      securityProfileTargetARN = pSecurityProfileTargetARN_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftNextToken :: Lens.Lens' ListSecurityProfilesForTarget (Lude.Maybe Lude.Text)
lspftNextToken = Lens.lens (nextToken :: ListSecurityProfilesForTarget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfilesForTarget)
{-# DEPRECATED lspftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If true, return child groups too.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftRecursive :: Lens.Lens' ListSecurityProfilesForTarget (Lude.Maybe Lude.Bool)
lspftRecursive = Lens.lens (recursive :: ListSecurityProfilesForTarget -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: ListSecurityProfilesForTarget)
{-# DEPRECATED lspftRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftMaxResults :: Lens.Lens' ListSecurityProfilesForTarget (Lude.Maybe Lude.Natural)
lspftMaxResults = Lens.lens (maxResults :: ListSecurityProfilesForTarget -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecurityProfilesForTarget)
{-# DEPRECATED lspftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the target (thing group) whose attached security profiles you want to get.
--
-- /Note:/ Consider using 'securityProfileTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftSecurityProfileTargetARN :: Lens.Lens' ListSecurityProfilesForTarget Lude.Text
lspftSecurityProfileTargetARN = Lens.lens (securityProfileTargetARN :: ListSecurityProfilesForTarget -> Lude.Text) (\s a -> s {securityProfileTargetARN = a} :: ListSecurityProfilesForTarget)
{-# DEPRECATED lspftSecurityProfileTargetARN "Use generic-lens or generic-optics with 'securityProfileTargetARN' instead." #-}

instance Page.AWSPager ListSecurityProfilesForTarget where
  page rq rs
    | Page.stop (rs Lens.^. lspftrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lspftrsSecurityProfileTargetMappings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspftNextToken Lens..~ rs Lens.^. lspftrsNextToken

instance Lude.AWSRequest ListSecurityProfilesForTarget where
  type
    Rs ListSecurityProfilesForTarget =
      ListSecurityProfilesForTargetResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecurityProfilesForTargetResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "securityProfileTargetMappings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecurityProfilesForTarget where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSecurityProfilesForTarget where
  toPath = Lude.const "/security-profiles-for-target"

instance Lude.ToQuery ListSecurityProfilesForTarget where
  toQuery ListSecurityProfilesForTarget' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "recursive" Lude.=: recursive,
        "maxResults" Lude.=: maxResults,
        "securityProfileTargetArn" Lude.=: securityProfileTargetARN
      ]

-- | /See:/ 'mkListSecurityProfilesForTargetResponse' smart constructor.
data ListSecurityProfilesForTargetResponse = ListSecurityProfilesForTargetResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    securityProfileTargetMappings ::
      Lude.Maybe
        [SecurityProfileTargetMapping],
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

-- | Creates a value of 'ListSecurityProfilesForTargetResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'securityProfileTargetMappings' - A list of security profiles and their associated targets.
mkListSecurityProfilesForTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecurityProfilesForTargetResponse
mkListSecurityProfilesForTargetResponse pResponseStatus_ =
  ListSecurityProfilesForTargetResponse'
    { nextToken = Lude.Nothing,
      securityProfileTargetMappings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrsNextToken :: Lens.Lens' ListSecurityProfilesForTargetResponse (Lude.Maybe Lude.Text)
lspftrsNextToken = Lens.lens (nextToken :: ListSecurityProfilesForTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityProfilesForTargetResponse)
{-# DEPRECATED lspftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of security profiles and their associated targets.
--
-- /Note:/ Consider using 'securityProfileTargetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrsSecurityProfileTargetMappings :: Lens.Lens' ListSecurityProfilesForTargetResponse (Lude.Maybe [SecurityProfileTargetMapping])
lspftrsSecurityProfileTargetMappings = Lens.lens (securityProfileTargetMappings :: ListSecurityProfilesForTargetResponse -> Lude.Maybe [SecurityProfileTargetMapping]) (\s a -> s {securityProfileTargetMappings = a} :: ListSecurityProfilesForTargetResponse)
{-# DEPRECATED lspftrsSecurityProfileTargetMappings "Use generic-lens or generic-optics with 'securityProfileTargetMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrsResponseStatus :: Lens.Lens' ListSecurityProfilesForTargetResponse Lude.Int
lspftrsResponseStatus = Lens.lens (responseStatus :: ListSecurityProfilesForTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecurityProfilesForTargetResponse)
{-# DEPRECATED lspftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
