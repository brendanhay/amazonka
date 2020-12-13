{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListChangeSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the @CREATE_IN_PROGRESS@ or @CREATE_PENDING@ state.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListChangeSets
  ( -- * Creating a request
    ListChangeSets (..),
    mkListChangeSets,

    -- ** Request lenses
    lcsNextToken,
    lcsStackName,

    -- * Destructuring the response
    ListChangeSetsResponse (..),
    mkListChangeSetsResponse,

    -- ** Response lenses
    lcsrsNextToken,
    lcsrsSummaries,
    lcsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'ListChangeSets' action.
--
-- /See:/ 'mkListChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { -- | A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChangeSets' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
-- * 'stackName' - The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
mkListChangeSets ::
  -- | 'stackName'
  Lude.Text ->
  ListChangeSets
mkListChangeSets pStackName_ =
  ListChangeSets'
    { nextToken = Lude.Nothing,
      stackName = pStackName_
    }

-- | A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListChangeSets (Lude.Maybe Lude.Text)
lcsNextToken = Lens.lens (nextToken :: ListChangeSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChangeSets)
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsStackName :: Lens.Lens' ListChangeSets Lude.Text
lcsStackName = Lens.lens (stackName :: ListChangeSets -> Lude.Text) (\s a -> s {stackName = a} :: ListChangeSets)
{-# DEPRECATED lcsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager ListChangeSets where
  page rq rs
    | Page.stop (rs Lens.^. lcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcsrsSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcsNextToken Lens..~ rs Lens.^. lcsrsNextToken

instance Lude.AWSRequest ListChangeSets where
  type Rs ListChangeSets = ListChangeSetsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListChangeSetsResult"
      ( \s h x ->
          ListChangeSetsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Summaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListChangeSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListChangeSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListChangeSets where
  toQuery ListChangeSets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListChangeSets" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for the 'ListChangeSets' action.
--
-- /See:/ 'mkListChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
    summaries :: Lude.Maybe [ChangeSetSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChangeSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
-- * 'summaries' - A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
-- * 'responseStatus' - The response status code.
mkListChangeSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListChangeSetsResponse
mkListChangeSetsResponse pResponseStatus_ =
  ListChangeSetsResponse'
    { nextToken = Lude.Nothing,
      summaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsNextToken :: Lens.Lens' ListChangeSetsResponse (Lude.Maybe Lude.Text)
lcsrsNextToken = Lens.lens (nextToken :: ListChangeSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChangeSetsResponse)
{-# DEPRECATED lcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsSummaries :: Lens.Lens' ListChangeSetsResponse (Lude.Maybe [ChangeSetSummary])
lcsrsSummaries = Lens.lens (summaries :: ListChangeSetsResponse -> Lude.Maybe [ChangeSetSummary]) (\s a -> s {summaries = a} :: ListChangeSetsResponse)
{-# DEPRECATED lcsrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsResponseStatus :: Lens.Lens' ListChangeSetsResponse Lude.Int
lcsrsResponseStatus = Lens.lens (responseStatus :: ListChangeSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListChangeSetsResponse)
{-# DEPRECATED lcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
