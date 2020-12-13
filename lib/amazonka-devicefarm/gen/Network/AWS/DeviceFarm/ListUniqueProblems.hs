{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListUniqueProblems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique problems, such as exceptions or crashes.
--
-- Unique problems are defined as a single instance of an error across a run, job, or suite. For example, if a call in your application consistently raises an exception (@OutOfBoundsException in MyActivity.java:386@ ), @ListUniqueProblems@ returns a single entry instead of many individual entries for that exception.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUniqueProblems
  ( -- * Creating a request
    ListUniqueProblems (..),
    mkListUniqueProblems,

    -- ** Request lenses
    lupArn,
    lupNextToken,

    -- * Destructuring the response
    ListUniqueProblemsResponse (..),
    mkListUniqueProblemsResponse,

    -- ** Response lenses
    luprsNextToken,
    luprsUniqueProblems,
    luprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list unique problems operation.
--
-- /See:/ 'mkListUniqueProblems' smart constructor.
data ListUniqueProblems = ListUniqueProblems'
  { -- | The unique problems' ARNs.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUniqueProblems' with the minimum fields required to make a request.
--
-- * 'arn' - The unique problems' ARNs.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListUniqueProblems ::
  -- | 'arn'
  Lude.Text ->
  ListUniqueProblems
mkListUniqueProblems pArn_ =
  ListUniqueProblems' {arn = pArn_, nextToken = Lude.Nothing}

-- | The unique problems' ARNs.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupArn :: Lens.Lens' ListUniqueProblems Lude.Text
lupArn = Lens.lens (arn :: ListUniqueProblems -> Lude.Text) (\s a -> s {arn = a} :: ListUniqueProblems)
{-# DEPRECATED lupArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUniqueProblems (Lude.Maybe Lude.Text)
lupNextToken = Lens.lens (nextToken :: ListUniqueProblems -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUniqueProblems)
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListUniqueProblems where
  page rq rs
    | Page.stop (rs Lens.^. luprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. luprsUniqueProblems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lupNextToken Lens..~ rs Lens.^. luprsNextToken

instance Lude.AWSRequest ListUniqueProblems where
  type Rs ListUniqueProblems = ListUniqueProblemsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUniqueProblemsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "uniqueProblems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUniqueProblems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListUniqueProblems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUniqueProblems where
  toJSON ListUniqueProblems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListUniqueProblems where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUniqueProblems where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list unique problems request.
--
-- /See:/ 'mkListUniqueProblemsResponse' smart constructor.
data ListUniqueProblemsResponse = ListUniqueProblemsResponse'
  { -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the unique problems.
    --
    -- Allowed values include:
    --
    --     * PENDING
    --
    --
    --     * PASSED
    --
    --
    --     * WARNED
    --
    --
    --     * FAILED
    --
    --
    --     * SKIPPED
    --
    --
    --     * ERRORED
    --
    --
    --     * STOPPED
    uniqueProblems :: Lude.Maybe (Lude.HashMap ExecutionResult ([UniqueProblem])),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUniqueProblemsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'uniqueProblems' - Information about the unique problems.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
-- * 'responseStatus' - The response status code.
mkListUniqueProblemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUniqueProblemsResponse
mkListUniqueProblemsResponse pResponseStatus_ =
  ListUniqueProblemsResponse'
    { nextToken = Lude.Nothing,
      uniqueProblems = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsNextToken :: Lens.Lens' ListUniqueProblemsResponse (Lude.Maybe Lude.Text)
luprsNextToken = Lens.lens (nextToken :: ListUniqueProblemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUniqueProblemsResponse)
{-# DEPRECATED luprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the unique problems.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
--
-- /Note:/ Consider using 'uniqueProblems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsUniqueProblems :: Lens.Lens' ListUniqueProblemsResponse (Lude.Maybe (Lude.HashMap ExecutionResult ([UniqueProblem])))
luprsUniqueProblems = Lens.lens (uniqueProblems :: ListUniqueProblemsResponse -> Lude.Maybe (Lude.HashMap ExecutionResult ([UniqueProblem]))) (\s a -> s {uniqueProblems = a} :: ListUniqueProblemsResponse)
{-# DEPRECATED luprsUniqueProblems "Use generic-lens or generic-optics with 'uniqueProblems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsResponseStatus :: Lens.Lens' ListUniqueProblemsResponse Lude.Int
luprsResponseStatus = Lens.lens (responseStatus :: ListUniqueProblemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUniqueProblemsResponse)
{-# DEPRECATED luprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
