{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the number of layers and apps in a specified stack, and the number of instances in each state, such as @running_setup@ or @online@ .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStackSummary
  ( -- * Creating a request
    DescribeStackSummary (..),
    mkDescribeStackSummary,

    -- ** Request lenses
    dssStackId,

    -- * Destructuring the response
    DescribeStackSummaryResponse (..),
    mkDescribeStackSummaryResponse,

    -- ** Response lenses
    dssrsStackSummary,
    dssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackSummary' smart constructor.
newtype DescribeStackSummary = DescribeStackSummary'
  { stackId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackSummary' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
mkDescribeStackSummary ::
  -- | 'stackId'
  Lude.Text ->
  DescribeStackSummary
mkDescribeStackSummary pStackId_ =
  DescribeStackSummary' {stackId = pStackId_}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStackId :: Lens.Lens' DescribeStackSummary Lude.Text
dssStackId = Lens.lens (stackId :: DescribeStackSummary -> Lude.Text) (\s a -> s {stackId = a} :: DescribeStackSummary)
{-# DEPRECATED dssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeStackSummary where
  type Rs DescribeStackSummary = DescribeStackSummaryResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStackSummaryResponse'
            Lude.<$> (x Lude..?> "StackSummary") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeStackSummary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStackSummary where
  toJSON DescribeStackSummary' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StackId" Lude..= stackId)])

instance Lude.ToPath DescribeStackSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackSummary where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeStackSummary@ request.
--
-- /See:/ 'mkDescribeStackSummaryResponse' smart constructor.
data DescribeStackSummaryResponse = DescribeStackSummaryResponse'
  { stackSummary ::
      Lude.Maybe StackSummary,
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

-- | Creates a value of 'DescribeStackSummaryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackSummary' - A @StackSummary@ object that contains the results.
mkDescribeStackSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackSummaryResponse
mkDescribeStackSummaryResponse pResponseStatus_ =
  DescribeStackSummaryResponse'
    { stackSummary = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @StackSummary@ object that contains the results.
--
-- /Note:/ Consider using 'stackSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsStackSummary :: Lens.Lens' DescribeStackSummaryResponse (Lude.Maybe StackSummary)
dssrsStackSummary = Lens.lens (stackSummary :: DescribeStackSummaryResponse -> Lude.Maybe StackSummary) (\s a -> s {stackSummary = a} :: DescribeStackSummaryResponse)
{-# DEPRECATED dssrsStackSummary "Use generic-lens or generic-optics with 'stackSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeStackSummaryResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeStackSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackSummaryResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
