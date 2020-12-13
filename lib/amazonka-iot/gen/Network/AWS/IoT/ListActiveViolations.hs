{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListActiveViolations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active violations for a given Device Defender security profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListActiveViolations
  ( -- * Creating a request
    ListActiveViolations (..),
    mkListActiveViolations,

    -- ** Request lenses
    lavNextToken,
    lavSecurityProfileName,
    lavThingName,
    lavMaxResults,

    -- * Destructuring the response
    ListActiveViolationsResponse (..),
    mkListActiveViolationsResponse,

    -- ** Response lenses
    lavrsActiveViolations,
    lavrsNextToken,
    lavrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListActiveViolations' smart constructor.
data ListActiveViolations = ListActiveViolations'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the Device Defender security profile for which violations are listed.
    securityProfileName :: Lude.Maybe Lude.Text,
    -- | The name of the thing whose active violations are listed.
    thingName :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActiveViolations' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'securityProfileName' - The name of the Device Defender security profile for which violations are listed.
-- * 'thingName' - The name of the thing whose active violations are listed.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListActiveViolations ::
  ListActiveViolations
mkListActiveViolations =
  ListActiveViolations'
    { nextToken = Lude.Nothing,
      securityProfileName = Lude.Nothing,
      thingName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListActiveViolations (Lude.Maybe Lude.Text)
lavNextToken = Lens.lens (nextToken :: ListActiveViolations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActiveViolations)
{-# DEPRECATED lavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Device Defender security profile for which violations are listed.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavSecurityProfileName :: Lens.Lens' ListActiveViolations (Lude.Maybe Lude.Text)
lavSecurityProfileName = Lens.lens (securityProfileName :: ListActiveViolations -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: ListActiveViolations)
{-# DEPRECATED lavSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The name of the thing whose active violations are listed.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavThingName :: Lens.Lens' ListActiveViolations (Lude.Maybe Lude.Text)
lavThingName = Lens.lens (thingName :: ListActiveViolations -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: ListActiveViolations)
{-# DEPRECATED lavThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxResults :: Lens.Lens' ListActiveViolations (Lude.Maybe Lude.Natural)
lavMaxResults = Lens.lens (maxResults :: ListActiveViolations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListActiveViolations)
{-# DEPRECATED lavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListActiveViolations where
  page rq rs
    | Page.stop (rs Lens.^. lavrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lavrsActiveViolations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lavNextToken Lens..~ rs Lens.^. lavrsNextToken

instance Lude.AWSRequest ListActiveViolations where
  type Rs ListActiveViolations = ListActiveViolationsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActiveViolationsResponse'
            Lude.<$> (x Lude..?> "activeViolations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListActiveViolations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListActiveViolations where
  toPath = Lude.const "/active-violations"

instance Lude.ToQuery ListActiveViolations where
  toQuery ListActiveViolations' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "securityProfileName" Lude.=: securityProfileName,
        "thingName" Lude.=: thingName,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListActiveViolationsResponse' smart constructor.
data ListActiveViolationsResponse = ListActiveViolationsResponse'
  { -- | The list of active violations.
    activeViolations :: Lude.Maybe [ActiveViolation],
    -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActiveViolationsResponse' with the minimum fields required to make a request.
--
-- * 'activeViolations' - The list of active violations.
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListActiveViolationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActiveViolationsResponse
mkListActiveViolationsResponse pResponseStatus_ =
  ListActiveViolationsResponse'
    { activeViolations = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of active violations.
--
-- /Note:/ Consider using 'activeViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsActiveViolations :: Lens.Lens' ListActiveViolationsResponse (Lude.Maybe [ActiveViolation])
lavrsActiveViolations = Lens.lens (activeViolations :: ListActiveViolationsResponse -> Lude.Maybe [ActiveViolation]) (\s a -> s {activeViolations = a} :: ListActiveViolationsResponse)
{-# DEPRECATED lavrsActiveViolations "Use generic-lens or generic-optics with 'activeViolations' instead." #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsNextToken :: Lens.Lens' ListActiveViolationsResponse (Lude.Maybe Lude.Text)
lavrsNextToken = Lens.lens (nextToken :: ListActiveViolationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActiveViolationsResponse)
{-# DEPRECATED lavrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsResponseStatus :: Lens.Lens' ListActiveViolationsResponse Lude.Int
lavrsResponseStatus = Lens.lens (responseStatus :: ListActiveViolationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActiveViolationsResponse)
{-# DEPRECATED lavrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
