{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListScheduledAudits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your scheduled audits.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListScheduledAudits
  ( -- * Creating a request
    ListScheduledAudits (..),
    mkListScheduledAudits,

    -- ** Request lenses
    lsaNextToken,
    lsaMaxResults,

    -- * Destructuring the response
    ListScheduledAuditsResponse (..),
    mkListScheduledAuditsResponse,

    -- ** Response lenses
    lsarsScheduledAudits,
    lsarsNextToken,
    lsarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListScheduledAudits' smart constructor.
data ListScheduledAudits = ListScheduledAudits'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListScheduledAudits' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
mkListScheduledAudits ::
  ListScheduledAudits
mkListScheduledAudits =
  ListScheduledAudits'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaNextToken :: Lens.Lens' ListScheduledAudits (Lude.Maybe Lude.Text)
lsaNextToken = Lens.lens (nextToken :: ListScheduledAudits -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListScheduledAudits)
{-# DEPRECATED lsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaMaxResults :: Lens.Lens' ListScheduledAudits (Lude.Maybe Lude.Natural)
lsaMaxResults = Lens.lens (maxResults :: ListScheduledAudits -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListScheduledAudits)
{-# DEPRECATED lsaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListScheduledAudits where
  page rq rs
    | Page.stop (rs Lens.^. lsarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsarsScheduledAudits) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsaNextToken Lens..~ rs Lens.^. lsarsNextToken

instance Lude.AWSRequest ListScheduledAudits where
  type Rs ListScheduledAudits = ListScheduledAuditsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListScheduledAuditsResponse'
            Lude.<$> (x Lude..?> "scheduledAudits" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListScheduledAudits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListScheduledAudits where
  toPath = Lude.const "/audit/scheduledaudits"

instance Lude.ToQuery ListScheduledAudits where
  toQuery ListScheduledAudits' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListScheduledAuditsResponse' smart constructor.
data ListScheduledAuditsResponse = ListScheduledAuditsResponse'
  { -- | The list of scheduled audits.
    scheduledAudits :: Lude.Maybe [ScheduledAuditMetadata],
    -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListScheduledAuditsResponse' with the minimum fields required to make a request.
--
-- * 'scheduledAudits' - The list of scheduled audits.
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListScheduledAuditsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListScheduledAuditsResponse
mkListScheduledAuditsResponse pResponseStatus_ =
  ListScheduledAuditsResponse'
    { scheduledAudits = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of scheduled audits.
--
-- /Note:/ Consider using 'scheduledAudits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsScheduledAudits :: Lens.Lens' ListScheduledAuditsResponse (Lude.Maybe [ScheduledAuditMetadata])
lsarsScheduledAudits = Lens.lens (scheduledAudits :: ListScheduledAuditsResponse -> Lude.Maybe [ScheduledAuditMetadata]) (\s a -> s {scheduledAudits = a} :: ListScheduledAuditsResponse)
{-# DEPRECATED lsarsScheduledAudits "Use generic-lens or generic-optics with 'scheduledAudits' instead." #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsNextToken :: Lens.Lens' ListScheduledAuditsResponse (Lude.Maybe Lude.Text)
lsarsNextToken = Lens.lens (nextToken :: ListScheduledAuditsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListScheduledAuditsResponse)
{-# DEPRECATED lsarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsResponseStatus :: Lens.Lens' ListScheduledAuditsResponse Lude.Int
lsarsResponseStatus = Lens.lens (responseStatus :: ListScheduledAuditsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListScheduledAuditsResponse)
{-# DEPRECATED lsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
