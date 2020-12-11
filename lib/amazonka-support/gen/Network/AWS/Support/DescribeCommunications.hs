{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns communications and attachments for one or more support cases. Use the @afterTime@ and @beforeTime@ parameters to filter by date. You can use the @caseId@ parameter to restrict the results to a specific case.
--
-- Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.
-- You can use the @maxResults@ and @nextToken@ parameters to control the pagination of the results. Set @maxResults@ to the number of cases that you want to display on each page, and use @nextToken@ to specify the resumption of pagination.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCommunications
  ( -- * Creating a request
    DescribeCommunications (..),
    mkDescribeCommunications,

    -- ** Request lenses
    dAfterTime,
    dBeforeTime,
    dNextToken,
    dMaxResults,
    dCaseId,

    -- * Destructuring the response
    DescribeCommunicationsResponse (..),
    mkDescribeCommunicationsResponse,

    -- ** Response lenses
    dcrsNextToken,
    dcrsCommunications,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeCommunications' smart constructor.
data DescribeCommunications = DescribeCommunications'
  { afterTime ::
      Lude.Maybe Lude.Text,
    beforeTime :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    caseId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCommunications' with the minimum fields required to make a request.
--
-- * 'afterTime' - The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
-- * 'beforeTime' - The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
-- * 'maxResults' - The maximum number of results to return before paginating.
-- * 'nextToken' - A resumption point for pagination.
mkDescribeCommunications ::
  -- | 'caseId'
  Lude.Text ->
  DescribeCommunications
mkDescribeCommunications pCaseId_ =
  DescribeCommunications'
    { afterTime = Lude.Nothing,
      beforeTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      caseId = pCaseId_
    }

-- | The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'afterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAfterTime :: Lens.Lens' DescribeCommunications (Lude.Maybe Lude.Text)
dAfterTime = Lens.lens (afterTime :: DescribeCommunications -> Lude.Maybe Lude.Text) (\s a -> s {afterTime = a} :: DescribeCommunications)
{-# DEPRECATED dAfterTime "Use generic-lens or generic-optics with 'afterTime' instead." #-}

-- | The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'beforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBeforeTime :: Lens.Lens' DescribeCommunications (Lude.Maybe Lude.Text)
dBeforeTime = Lens.lens (beforeTime :: DescribeCommunications -> Lude.Maybe Lude.Text) (\s a -> s {beforeTime = a} :: DescribeCommunications)
{-# DEPRECATED dBeforeTime "Use generic-lens or generic-optics with 'beforeTime' instead." #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeCommunications (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeCommunications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCommunications)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return before paginating.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeCommunications (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeCommunications -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCommunications)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCaseId :: Lens.Lens' DescribeCommunications Lude.Text
dCaseId = Lens.lens (caseId :: DescribeCommunications -> Lude.Text) (\s a -> s {caseId = a} :: DescribeCommunications)
{-# DEPRECATED dCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

instance Page.AWSPager DescribeCommunications where
  page rq rs
    | Page.stop (rs Lens.^. dcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrsCommunications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. dcrsNextToken

instance Lude.AWSRequest DescribeCommunications where
  type Rs DescribeCommunications = DescribeCommunicationsResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCommunicationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "communications" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCommunications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.DescribeCommunications" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCommunications where
  toJSON DescribeCommunications' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("afterTime" Lude..=) Lude.<$> afterTime,
            ("beforeTime" Lude..=) Lude.<$> beforeTime,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("caseId" Lude..= caseId)
          ]
      )

instance Lude.ToPath DescribeCommunications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCommunications where
  toQuery = Lude.const Lude.mempty

-- | The communications returned by the 'DescribeCommunications' operation.
--
-- /See:/ 'mkDescribeCommunicationsResponse' smart constructor.
data DescribeCommunicationsResponse = DescribeCommunicationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    communications ::
      Lude.Maybe [Communication],
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

-- | Creates a value of 'DescribeCommunicationsResponse' with the minimum fields required to make a request.
--
-- * 'communications' - The communications for the case.
-- * 'nextToken' - A resumption point for pagination.
-- * 'responseStatus' - The response status code.
mkDescribeCommunicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCommunicationsResponse
mkDescribeCommunicationsResponse pResponseStatus_ =
  DescribeCommunicationsResponse'
    { nextToken = Lude.Nothing,
      communications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsNextToken :: Lens.Lens' DescribeCommunicationsResponse (Lude.Maybe Lude.Text)
dcrsNextToken = Lens.lens (nextToken :: DescribeCommunicationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCommunicationsResponse)
{-# DEPRECATED dcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The communications for the case.
--
-- /Note:/ Consider using 'communications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCommunications :: Lens.Lens' DescribeCommunicationsResponse (Lude.Maybe [Communication])
dcrsCommunications = Lens.lens (communications :: DescribeCommunicationsResponse -> Lude.Maybe [Communication]) (\s a -> s {communications = a} :: DescribeCommunicationsResponse)
{-# DEPRECATED dcrsCommunications "Use generic-lens or generic-optics with 'communications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCommunicationsResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCommunicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCommunicationsResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
