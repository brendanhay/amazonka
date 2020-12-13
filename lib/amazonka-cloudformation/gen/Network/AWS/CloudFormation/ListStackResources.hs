{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for up to 90 days after the stack has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackResources
  ( -- * Creating a request
    ListStackResources (..),
    mkListStackResources,

    -- ** Request lenses
    lsrNextToken,
    lsrStackName,

    -- * Destructuring the response
    ListStackResourcesResponse (..),
    mkListStackResourcesResponse,

    -- ** Response lenses
    lsrrsNextToken,
    lsrrsStackResourceSummaries,
    lsrrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'ListStackResource' action.
--
-- /See:/ 'mkListStackResources' smart constructor.
data ListStackResources = ListStackResources'
  { -- | A string that identifies the next page of stack resources that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
    --
    --
    --     * Running stacks: You can specify either the stack's name or its unique stack ID.
    --
    --
    --     * Deleted stacks: You must specify the unique stack ID.
    --
    --
    -- Default: There is no default value.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackResources' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of stack resources that you want to retrieve.
-- * 'stackName' - The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
mkListStackResources ::
  -- | 'stackName'
  Lude.Text ->
  ListStackResources
mkListStackResources pStackName_ =
  ListStackResources'
    { nextToken = Lude.Nothing,
      stackName = pStackName_
    }

-- | A string that identifies the next page of stack resources that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrNextToken :: Lens.Lens' ListStackResources (Lude.Maybe Lude.Text)
lsrNextToken = Lens.lens (nextToken :: ListStackResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackResources)
{-# DEPRECATED lsrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrStackName :: Lens.Lens' ListStackResources Lude.Text
lsrStackName = Lens.lens (stackName :: ListStackResources -> Lude.Text) (\s a -> s {stackName = a} :: ListStackResources)
{-# DEPRECATED lsrStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager ListStackResources where
  page rq rs
    | Page.stop (rs Lens.^. lsrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrrsStackResourceSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsrNextToken Lens..~ rs Lens.^. lsrrsNextToken

instance Lude.AWSRequest ListStackResources where
  type Rs ListStackResources = ListStackResourcesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStackResourcesResult"
      ( \s h x ->
          ListStackResourcesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "StackResourceSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStackResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackResources where
  toQuery ListStackResources' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListStackResources" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'ListStackResources' action.
--
-- /See:/ 'mkListStackResourcesResponse' smart constructor.
data ListStackResourcesResponse = ListStackResourcesResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of stack resources. If no additional page exists, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @StackResourceSummary@ structures.
    stackResourceSummaries :: Lude.Maybe [StackResourceSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the output exceeds 1 MB, a string that identifies the next page of stack resources. If no additional page exists, this value is null.
-- * 'stackResourceSummaries' - A list of @StackResourceSummary@ structures.
-- * 'responseStatus' - The response status code.
mkListStackResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackResourcesResponse
mkListStackResourcesResponse pResponseStatus_ =
  ListStackResourcesResponse'
    { nextToken = Lude.Nothing,
      stackResourceSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of stack resources. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListStackResourcesResponse (Lude.Maybe Lude.Text)
lsrrsNextToken = Lens.lens (nextToken :: ListStackResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackResourcesResponse)
{-# DEPRECATED lsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackResourceSummary@ structures.
--
-- /Note:/ Consider using 'stackResourceSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsStackResourceSummaries :: Lens.Lens' ListStackResourcesResponse (Lude.Maybe [StackResourceSummary])
lsrrsStackResourceSummaries = Lens.lens (stackResourceSummaries :: ListStackResourcesResponse -> Lude.Maybe [StackResourceSummary]) (\s a -> s {stackResourceSummaries = a} :: ListStackResourcesResponse)
{-# DEPRECATED lsrrsStackResourceSummaries "Use generic-lens or generic-optics with 'stackResourceSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStackResourcesResponse Lude.Int
lsrrsResponseStatus = Lens.lens (responseStatus :: ListStackResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackResourcesResponse)
{-# DEPRECATED lsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
