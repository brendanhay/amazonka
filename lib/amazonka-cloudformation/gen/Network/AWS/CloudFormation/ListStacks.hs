{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the summary information for stacks whose status matches the specified StackStatusFilter. Summary information for stacks that have been deleted is kept for 90 days after the stack is deleted. If no StackStatusFilter is specified, summary information for all stacks is returned (including existing stacks and stacks that have been deleted).
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStacks
  ( -- * Creating a request
    ListStacks (..),
    mkListStacks,

    -- ** Request lenses
    lsNextToken,
    lsStackStatusFilter,

    -- * Destructuring the response
    ListStacksResponse (..),
    mkListStacksResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsStackSummaries,
    lsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'ListStacks' action.
--
-- /See:/ 'mkListStacks' smart constructor.
data ListStacks = ListStacks'
  { -- | A string that identifies the next page of stacks that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Stack status to use as a filter. Specify one or more stack status codes to list only stacks with the specified status codes. For a complete list of stack status codes, see the @StackStatus@ parameter of the 'Stack' data type.
    stackStatusFilter :: Lude.Maybe [StackStatus]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStacks' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of stacks that you want to retrieve.
-- * 'stackStatusFilter' - Stack status to use as a filter. Specify one or more stack status codes to list only stacks with the specified status codes. For a complete list of stack status codes, see the @StackStatus@ parameter of the 'Stack' data type.
mkListStacks ::
  ListStacks
mkListStacks =
  ListStacks'
    { nextToken = Lude.Nothing,
      stackStatusFilter = Lude.Nothing
    }

-- | A string that identifies the next page of stacks that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStacks (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListStacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStacks)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Stack status to use as a filter. Specify one or more stack status codes to list only stacks with the specified status codes. For a complete list of stack status codes, see the @StackStatus@ parameter of the 'Stack' data type.
--
-- /Note:/ Consider using 'stackStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStackStatusFilter :: Lens.Lens' ListStacks (Lude.Maybe [StackStatus])
lsStackStatusFilter = Lens.lens (stackStatusFilter :: ListStacks -> Lude.Maybe [StackStatus]) (\s a -> s {stackStatusFilter = a} :: ListStacks)
{-# DEPRECATED lsStackStatusFilter "Use generic-lens or generic-optics with 'stackStatusFilter' instead." #-}

instance Page.AWSPager ListStacks where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsStackSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListStacks where
  type Rs ListStacks = ListStacksResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStacksResult"
      ( \s h x ->
          ListStacksResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "StackSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStacks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStacks where
  toQuery ListStacks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListStacks" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackStatusFilter"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> stackStatusFilter)
      ]

-- | The output for 'ListStacks' action.
--
-- /See:/ 'mkListStacksResponse' smart constructor.
data ListStacksResponse = ListStacksResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @StackSummary@ structures containing information about the specified stacks.
    stackSummaries :: Lude.Maybe [StackSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStacksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
-- * 'stackSummaries' - A list of @StackSummary@ structures containing information about the specified stacks.
-- * 'responseStatus' - The response status code.
mkListStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStacksResponse
mkListStacksResponse pResponseStatus_ =
  ListStacksResponse'
    { nextToken = Lude.Nothing,
      stackSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListStacksResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListStacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStacksResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSummary@ structures containing information about the specified stacks.
--
-- /Note:/ Consider using 'stackSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStackSummaries :: Lens.Lens' ListStacksResponse (Lude.Maybe [StackSummary])
lsrsStackSummaries = Lens.lens (stackSummaries :: ListStacksResponse -> Lude.Maybe [StackSummary]) (\s a -> s {stackSummaries = a} :: ListStacksResponse)
{-# DEPRECATED lsrsStackSummaries "Use generic-lens or generic-optics with 'stackSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStacksResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStacksResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
