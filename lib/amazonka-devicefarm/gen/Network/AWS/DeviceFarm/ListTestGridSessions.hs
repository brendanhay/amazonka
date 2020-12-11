{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sessions for a 'TestGridProject' .
module Network.AWS.DeviceFarm.ListTestGridSessions
  ( -- * Creating a request
    ListTestGridSessions (..),
    mkListTestGridSessions,

    -- ** Request lenses
    ltgsStatus,
    ltgsMaxResult,
    ltgsCreationTimeAfter,
    ltgsEndTimeBefore,
    ltgsEndTimeAfter,
    ltgsNextToken,
    ltgsCreationTimeBefore,
    ltgsProjectARN,

    -- * Destructuring the response
    ListTestGridSessionsResponse (..),
    mkListTestGridSessionsResponse,

    -- ** Response lenses
    ltgsrsNextToken,
    ltgsrsTestGridSessions,
    ltgsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { status ::
      Lude.Maybe TestGridSessionStatus,
    maxResult :: Lude.Maybe Lude.Natural,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    endTimeBefore :: Lude.Maybe Lude.Timestamp,
    endTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    projectARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTestGridSessions' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - Return only sessions created after this time.
-- * 'creationTimeBefore' - Return only sessions created before this time.
-- * 'endTimeAfter' - Return only sessions that ended after this time.
-- * 'endTimeBefore' - Return only sessions that ended before this time.
-- * 'maxResult' - Return only this many results at a time.
-- * 'nextToken' - Pagination token.
-- * 'projectARN' - ARN of a 'TestGridProject' .
-- * 'status' - Return only sessions in this state.
mkListTestGridSessions ::
  -- | 'projectARN'
  Lude.Text ->
  ListTestGridSessions
mkListTestGridSessions pProjectARN_ =
  ListTestGridSessions'
    { status = Lude.Nothing,
      maxResult = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      endTimeBefore = Lude.Nothing,
      endTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      projectARN = pProjectARN_
    }

-- | Return only sessions in this state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsStatus :: Lens.Lens' ListTestGridSessions (Lude.Maybe TestGridSessionStatus)
ltgsStatus = Lens.lens (status :: ListTestGridSessions -> Lude.Maybe TestGridSessionStatus) (\s a -> s {status = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Return only this many results at a time.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsMaxResult :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Natural)
ltgsMaxResult = Lens.lens (maxResult :: ListTestGridSessions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResult = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsMaxResult "Use generic-lens or generic-optics with 'maxResult' instead." #-}

-- | Return only sessions created after this time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsCreationTimeAfter :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Timestamp)
ltgsCreationTimeAfter = Lens.lens (creationTimeAfter :: ListTestGridSessions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | Return only sessions that ended before this time.
--
-- /Note:/ Consider using 'endTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsEndTimeBefore :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Timestamp)
ltgsEndTimeBefore = Lens.lens (endTimeBefore :: ListTestGridSessions -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTimeBefore = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsEndTimeBefore "Use generic-lens or generic-optics with 'endTimeBefore' instead." #-}

-- | Return only sessions that ended after this time.
--
-- /Note:/ Consider using 'endTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsEndTimeAfter :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Timestamp)
ltgsEndTimeAfter = Lens.lens (endTimeAfter :: ListTestGridSessions -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTimeAfter = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsEndTimeAfter "Use generic-lens or generic-optics with 'endTimeAfter' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsNextToken :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Text)
ltgsNextToken = Lens.lens (nextToken :: ListTestGridSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Return only sessions created before this time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsCreationTimeBefore :: Lens.Lens' ListTestGridSessions (Lude.Maybe Lude.Timestamp)
ltgsCreationTimeBefore = Lens.lens (creationTimeBefore :: ListTestGridSessions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | ARN of a 'TestGridProject' .
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsProjectARN :: Lens.Lens' ListTestGridSessions Lude.Text
ltgsProjectARN = Lens.lens (projectARN :: ListTestGridSessions -> Lude.Text) (\s a -> s {projectARN = a} :: ListTestGridSessions)
{-# DEPRECATED ltgsProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest ListTestGridSessions where
  type Rs ListTestGridSessions = ListTestGridSessionsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTestGridSessionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "testGridSessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTestGridSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListTestGridSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTestGridSessions where
  toJSON ListTestGridSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("maxResult" Lude..=) Lude.<$> maxResult,
            ("creationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("endTimeBefore" Lude..=) Lude.<$> endTimeBefore,
            ("endTimeAfter" Lude..=) Lude.<$> endTimeAfter,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("creationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            Lude.Just ("projectArn" Lude..= projectARN)
          ]
      )

instance Lude.ToPath ListTestGridSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTestGridSessions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTestGridSessionsResponse' smart constructor.
data ListTestGridSessionsResponse = ListTestGridSessionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    testGridSessions ::
      Lude.Maybe [TestGridSession],
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

-- | Creates a value of 'ListTestGridSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'testGridSessions' - The sessions that match the criteria in a 'ListTestGridSessionsRequest' .
mkListTestGridSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTestGridSessionsResponse
mkListTestGridSessionsResponse pResponseStatus_ =
  ListTestGridSessionsResponse'
    { nextToken = Lude.Nothing,
      testGridSessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrsNextToken :: Lens.Lens' ListTestGridSessionsResponse (Lude.Maybe Lude.Text)
ltgsrsNextToken = Lens.lens (nextToken :: ListTestGridSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridSessionsResponse)
{-# DEPRECATED ltgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sessions that match the criteria in a 'ListTestGridSessionsRequest' .
--
-- /Note:/ Consider using 'testGridSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrsTestGridSessions :: Lens.Lens' ListTestGridSessionsResponse (Lude.Maybe [TestGridSession])
ltgsrsTestGridSessions = Lens.lens (testGridSessions :: ListTestGridSessionsResponse -> Lude.Maybe [TestGridSession]) (\s a -> s {testGridSessions = a} :: ListTestGridSessionsResponse)
{-# DEPRECATED ltgsrsTestGridSessions "Use generic-lens or generic-optics with 'testGridSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrsResponseStatus :: Lens.Lens' ListTestGridSessionsResponse Lude.Int
ltgsrsResponseStatus = Lens.lens (responseStatus :: ListTestGridSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTestGridSessionsResponse)
{-# DEPRECATED ltgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
