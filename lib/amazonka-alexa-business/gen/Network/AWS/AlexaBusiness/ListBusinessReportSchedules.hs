{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of the schedules that a user configured. A download URL of the report associated with each schedule is returned every time this action is called. A new download URL is returned each time, and is valid for 24 hours.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListBusinessReportSchedules
  ( -- * Creating a request
    ListBusinessReportSchedules (..),
    mkListBusinessReportSchedules,

    -- ** Request lenses
    lbrsNextToken,
    lbrsMaxResults,

    -- * Destructuring the response
    ListBusinessReportSchedulesResponse (..),
    mkListBusinessReportSchedulesResponse,

    -- ** Response lenses
    lbrsrsBusinessReportSchedules,
    lbrsrsNextToken,
    lbrsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBusinessReportSchedules' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of schedules listed in the call.
-- * 'nextToken' - The token used to list the remaining schedules from the previous API call.
mkListBusinessReportSchedules ::
  ListBusinessReportSchedules
mkListBusinessReportSchedules =
  ListBusinessReportSchedules'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBusinessReportSchedules (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBusinessReportSchedules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBusinessReportSchedules)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of schedules listed in the call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsMaxResults :: Lens.Lens' ListBusinessReportSchedules (Lude.Maybe Lude.Natural)
lbrsMaxResults = Lens.lens (maxResults :: ListBusinessReportSchedules -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBusinessReportSchedules)
{-# DEPRECATED lbrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBusinessReportSchedules where
  page rq rs
    | Page.stop (rs Lens.^. lbrsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsrsBusinessReportSchedules) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbrsNextToken Lens..~ rs Lens.^. lbrsrsNextToken

instance Lude.AWSRequest ListBusinessReportSchedules where
  type
    Rs ListBusinessReportSchedules =
      ListBusinessReportSchedulesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBusinessReportSchedulesResponse'
            Lude.<$> (x Lude..?> "BusinessReportSchedules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBusinessReportSchedules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.ListBusinessReportSchedules" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBusinessReportSchedules where
  toJSON ListBusinessReportSchedules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListBusinessReportSchedules where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBusinessReportSchedules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { businessReportSchedules ::
      Lude.Maybe
        [BusinessReportSchedule],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListBusinessReportSchedulesResponse' with the minimum fields required to make a request.
--
-- * 'businessReportSchedules' - The schedule of the reports.
-- * 'nextToken' - The token used to list the remaining schedules from the previous API call.
-- * 'responseStatus' - The response status code.
mkListBusinessReportSchedulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBusinessReportSchedulesResponse
mkListBusinessReportSchedulesResponse pResponseStatus_ =
  ListBusinessReportSchedulesResponse'
    { businessReportSchedules =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The schedule of the reports.
--
-- /Note:/ Consider using 'businessReportSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrsBusinessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Lude.Maybe [BusinessReportSchedule])
lbrsrsBusinessReportSchedules = Lens.lens (businessReportSchedules :: ListBusinessReportSchedulesResponse -> Lude.Maybe [BusinessReportSchedule]) (\s a -> s {businessReportSchedules = a} :: ListBusinessReportSchedulesResponse)
{-# DEPRECATED lbrsrsBusinessReportSchedules "Use generic-lens or generic-optics with 'businessReportSchedules' instead." #-}

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrsNextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Lude.Maybe Lude.Text)
lbrsrsNextToken = Lens.lens (nextToken :: ListBusinessReportSchedulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBusinessReportSchedulesResponse)
{-# DEPRECATED lbrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrsResponseStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Lude.Int
lbrsrsResponseStatus = Lens.lens (responseStatus :: ListBusinessReportSchedulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBusinessReportSchedulesResponse)
{-# DEPRECATED lbrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
