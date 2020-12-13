{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOTAUpdates
  ( -- * Creating a request
    ListOTAUpdates (..),
    mkListOTAUpdates,

    -- ** Request lenses
    lotauNextToken,
    lotauOtaUpdateStatus,
    lotauMaxResults,

    -- * Destructuring the response
    ListOTAUpdatesResponse (..),
    mkListOTAUpdatesResponse,

    -- ** Response lenses
    lotaursNextToken,
    lotaursOtaUpdates,
    lotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { -- | A token used to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The OTA update job status.
    otaUpdateStatus :: Lude.Maybe OTAUpdateStatus,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOTAUpdates' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token used to retrieve the next set of results.
-- * 'otaUpdateStatus' - The OTA update job status.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListOTAUpdates ::
  ListOTAUpdates
mkListOTAUpdates =
  ListOTAUpdates'
    { nextToken = Lude.Nothing,
      otaUpdateStatus = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A token used to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauNextToken :: Lens.Lens' ListOTAUpdates (Lude.Maybe Lude.Text)
lotauNextToken = Lens.lens (nextToken :: ListOTAUpdates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOTAUpdates)
{-# DEPRECATED lotauNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The OTA update job status.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauOtaUpdateStatus :: Lens.Lens' ListOTAUpdates (Lude.Maybe OTAUpdateStatus)
lotauOtaUpdateStatus = Lens.lens (otaUpdateStatus :: ListOTAUpdates -> Lude.Maybe OTAUpdateStatus) (\s a -> s {otaUpdateStatus = a} :: ListOTAUpdates)
{-# DEPRECATED lotauOtaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauMaxResults :: Lens.Lens' ListOTAUpdates (Lude.Maybe Lude.Natural)
lotauMaxResults = Lens.lens (maxResults :: ListOTAUpdates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOTAUpdates)
{-# DEPRECATED lotauMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOTAUpdates where
  page rq rs
    | Page.stop (rs Lens.^. lotaursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lotaursOtaUpdates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lotauNextToken Lens..~ rs Lens.^. lotaursNextToken

instance Lude.AWSRequest ListOTAUpdates where
  type Rs ListOTAUpdates = ListOTAUpdatesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOTAUpdatesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "otaUpdates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOTAUpdates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListOTAUpdates where
  toPath = Lude.const "/otaUpdates"

instance Lude.ToQuery ListOTAUpdates where
  toQuery ListOTAUpdates' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "otaUpdateStatus" Lude.=: otaUpdateStatus,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { -- | A token to use to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of OTA update jobs.
    otaUpdates :: Lude.Maybe [OTAUpdateSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOTAUpdatesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to use to get the next set of results.
-- * 'otaUpdates' - A list of OTA update jobs.
-- * 'responseStatus' - The response status code.
mkListOTAUpdatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOTAUpdatesResponse
mkListOTAUpdatesResponse pResponseStatus_ =
  ListOTAUpdatesResponse'
    { nextToken = Lude.Nothing,
      otaUpdates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token to use to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaursNextToken :: Lens.Lens' ListOTAUpdatesResponse (Lude.Maybe Lude.Text)
lotaursNextToken = Lens.lens (nextToken :: ListOTAUpdatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOTAUpdatesResponse)
{-# DEPRECATED lotaursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of OTA update jobs.
--
-- /Note:/ Consider using 'otaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaursOtaUpdates :: Lens.Lens' ListOTAUpdatesResponse (Lude.Maybe [OTAUpdateSummary])
lotaursOtaUpdates = Lens.lens (otaUpdates :: ListOTAUpdatesResponse -> Lude.Maybe [OTAUpdateSummary]) (\s a -> s {otaUpdates = a} :: ListOTAUpdatesResponse)
{-# DEPRECATED lotaursOtaUpdates "Use generic-lens or generic-optics with 'otaUpdates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaursResponseStatus :: Lens.Lens' ListOTAUpdatesResponse Lude.Int
lotaursResponseStatus = Lens.lens (responseStatus :: ListOTAUpdatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOTAUpdatesResponse)
{-# DEPRECATED lotaursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
