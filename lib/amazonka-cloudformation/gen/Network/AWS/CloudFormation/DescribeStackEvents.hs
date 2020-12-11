{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks> in the AWS CloudFormation User Guide.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStackEvents
  ( -- * Creating a request
    DescribeStackEvents (..),
    mkDescribeStackEvents,

    -- ** Request lenses
    dseNextToken,
    dseStackName,

    -- * Destructuring the response
    DescribeStackEventsResponse (..),
    mkDescribeStackEventsResponse,

    -- ** Response lenses
    dsersNextToken,
    dsersStackEvents,
    dsersResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'DescribeStackEvents' action.
--
-- /See:/ 'mkDescribeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
  { nextToken ::
      Lude.Maybe Lude.Text,
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackEvents' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of events that you want to retrieve.
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
mkDescribeStackEvents ::
  DescribeStackEvents
mkDescribeStackEvents =
  DescribeStackEvents'
    { nextToken = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | A string that identifies the next page of events that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseNextToken :: Lens.Lens' DescribeStackEvents (Lude.Maybe Lude.Text)
dseNextToken = Lens.lens (nextToken :: DescribeStackEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStackEvents)
{-# DEPRECATED dseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
dseStackName :: Lens.Lens' DescribeStackEvents (Lude.Maybe Lude.Text)
dseStackName = Lens.lens (stackName :: DescribeStackEvents -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeStackEvents)
{-# DEPRECATED dseStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager DescribeStackEvents where
  page rq rs
    | Page.stop (rs Lens.^. dsersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsersStackEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dseNextToken Lens..~ rs Lens.^. dsersNextToken

instance Lude.AWSRequest DescribeStackEvents where
  type Rs DescribeStackEvents = DescribeStackEventsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackEventsResult"
      ( \s h x ->
          DescribeStackEventsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "StackEvents" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackEvents where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackEvents where
  toQuery DescribeStackEvents' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStackEvents" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'DescribeStackEvents' action.
--
-- /See:/ 'mkDescribeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    stackEvents ::
      Lude.Maybe [StackEvent],
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

-- | Creates a value of 'DescribeStackEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'stackEvents' - A list of @StackEvents@ structures.
mkDescribeStackEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackEventsResponse
mkDescribeStackEventsResponse pResponseStatus_ =
  DescribeStackEventsResponse'
    { nextToken = Lude.Nothing,
      stackEvents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsersNextToken :: Lens.Lens' DescribeStackEventsResponse (Lude.Maybe Lude.Text)
dsersNextToken = Lens.lens (nextToken :: DescribeStackEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStackEventsResponse)
{-# DEPRECATED dsersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackEvents@ structures.
--
-- /Note:/ Consider using 'stackEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsersStackEvents :: Lens.Lens' DescribeStackEventsResponse (Lude.Maybe [StackEvent])
dsersStackEvents = Lens.lens (stackEvents :: DescribeStackEventsResponse -> Lude.Maybe [StackEvent]) (\s a -> s {stackEvents = a} :: DescribeStackEventsResponse)
{-# DEPRECATED dsersStackEvents "Use generic-lens or generic-optics with 'stackEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsersResponseStatus :: Lens.Lens' DescribeStackEventsResponse Lude.Int
dsersResponseStatus = Lens.lens (responseStatus :: DescribeStackEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackEventsResponse)
{-# DEPRECATED dsersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
