{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more stacks.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStacks
  ( -- * Creating a request
    DescribeStacks (..),
    mkDescribeStacks,

    -- ** Request lenses
    dsStackIds,

    -- * Destructuring the response
    DescribeStacksResponse (..),
    mkDescribeStacksResponse,

    -- ** Response lenses
    dsrsStacks,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStacks' smart constructor.
newtype DescribeStacks = DescribeStacks'
  { stackIds ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- * 'stackIds' - An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
mkDescribeStacks ::
  DescribeStacks
mkDescribeStacks = DescribeStacks' {stackIds = Lude.Nothing}

-- | An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
--
-- /Note:/ Consider using 'stackIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackIds :: Lens.Lens' DescribeStacks (Lude.Maybe [Lude.Text])
dsStackIds = Lens.lens (stackIds :: DescribeStacks -> Lude.Maybe [Lude.Text]) (\s a -> s {stackIds = a} :: DescribeStacks)
{-# DEPRECATED dsStackIds "Use generic-lens or generic-optics with 'stackIds' instead." #-}

instance Lude.AWSRequest DescribeStacks where
  type Rs DescribeStacks = DescribeStacksResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Lude.<$> (x Lude..?> "Stacks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeStacks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Lude.object
      (Lude.catMaybes [("StackIds" Lude..=) Lude.<$> stackIds])

instance Lude.ToPath DescribeStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStacks where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeStacks@ request.
--
-- /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { stacks ::
      Lude.Maybe [Stack],
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

-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stacks' - An array of @Stack@ objects that describe the stacks.
mkDescribeStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStacksResponse
mkDescribeStacksResponse pResponseStatus_ =
  DescribeStacksResponse'
    { stacks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Stack@ objects that describe the stacks.
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
