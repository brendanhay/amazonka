{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStacks
  ( -- * Creating a request
    DescribeStacks (..),
    mkDescribeStacks,

    -- ** Request lenses
    dsNextToken,
    dsStackName,

    -- * Destructuring the response
    DescribeStacksResponse (..),
    mkDescribeStacksResponse,

    -- ** Response lenses
    dsrsNextToken,
    dsrsStacks,
    dsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'DescribeStacks' action.
--
-- /See:/ 'mkDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | A string that identifies the next page of stacks that you want to retrieve.
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
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of stacks that you want to retrieve.
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
mkDescribeStacks ::
  DescribeStacks
mkDescribeStacks =
  DescribeStacks'
    { nextToken = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | A string that identifies the next page of stacks that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeStacks (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeStacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStacks)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
dsStackName :: Lens.Lens' DescribeStacks (Lude.Maybe Lude.Text)
dsStackName = Lens.lens (stackName :: DescribeStacks -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeStacks)
{-# DEPRECATED dsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager DescribeStacks where
  page rq rs
    | Page.stop (rs Lens.^. dsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsStacks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dsrsNextToken

instance Lude.AWSRequest DescribeStacks where
  type Rs DescribeStacks = DescribeStacksResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStacksResult"
      ( \s h x ->
          DescribeStacksResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Stacks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStacks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStacks where
  toQuery DescribeStacks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStacks" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'DescribeStacks' action.
--
-- /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of stack structures.
    stacks :: Lude.Maybe [Stack],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
-- * 'stacks' - A list of stack structures.
-- * 'responseStatus' - The response status code.
mkDescribeStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStacksResponse
mkDescribeStacksResponse pResponseStatus_ =
  DescribeStacksResponse'
    { nextToken = Lude.Nothing,
      stacks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsNextToken :: Lens.Lens' DescribeStacksResponse (Lude.Maybe Lude.Text)
dsrsNextToken = Lens.lens (nextToken :: DescribeStacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStacksResponse)
{-# DEPRECATED dsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of stack structures.
--
-- /Note:/ Consider using 'stacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsStacks :: Lens.Lens' DescribeStacksResponse (Lude.Maybe [Stack])
dsrsStacks = Lens.lens (stacks :: DescribeStacksResponse -> Lude.Maybe [Stack]) (\s a -> s {stacks = a} :: DescribeStacksResponse)
{-# DEPRECATED dsrsStacks "Use generic-lens or generic-optics with 'stacks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeStacksResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeStacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStacksResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
