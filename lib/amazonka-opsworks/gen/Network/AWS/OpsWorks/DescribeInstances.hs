{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a set of instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeInstances
  ( -- * Creating a request
    DescribeInstances (..),
    mkDescribeInstances,

    -- ** Request lenses
    diInstanceIds,
    diStackId,
    diLayerId,

    -- * Destructuring the response
    DescribeInstancesResponse (..),
    mkDescribeInstancesResponse,

    -- ** Response lenses
    dirsInstances,
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
    layerId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
-- * 'stackId' - A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
-- * 'layerId' - A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
mkDescribeInstances ::
  DescribeInstances
mkDescribeInstances =
  DescribeInstances'
    { instanceIds = Lude.Nothing,
      stackId = Lude.Nothing,
      layerId = Lude.Nothing
    }

-- | An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceIds :: Lens.Lens' DescribeInstances (Lude.Maybe [Lude.Text])
diInstanceIds = Lens.lens (instanceIds :: DescribeInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeInstances)
{-# DEPRECATED diInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStackId :: Lens.Lens' DescribeInstances (Lude.Maybe Lude.Text)
diStackId = Lens.lens (stackId :: DescribeInstances -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeInstances)
{-# DEPRECATED diStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLayerId :: Lens.Lens' DescribeInstances (Lude.Maybe Lude.Text)
diLayerId = Lens.lens (layerId :: DescribeInstances -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: DescribeInstances)
{-# DEPRECATED diLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest DescribeInstances where
  type Rs DescribeInstances = DescribeInstancesResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Lude.<$> (x Lude..?> "Instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceIds" Lude..=) Lude.<$> instanceIds,
            ("StackId" Lude..=) Lude.<$> stackId,
            ("LayerId" Lude..=) Lude.<$> layerId
          ]
      )

instance Lude.ToPath DescribeInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstances where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeInstances@ request.
--
-- /See:/ 'mkDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | An array of @Instance@ objects that describe the instances.
    instances :: Lude.Maybe [Instance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instances' - An array of @Instance@ objects that describe the instances.
-- * 'responseStatus' - The response status code.
mkDescribeInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancesResponse
mkDescribeInstancesResponse pResponseStatus_ =
  DescribeInstancesResponse'
    { instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Instance@ objects that describe the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsInstances :: Lens.Lens' DescribeInstancesResponse (Lude.Maybe [Instance])
dirsInstances = Lens.lens (instances :: DescribeInstancesResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: DescribeInstancesResponse)
{-# DEPRECATED dirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeInstancesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
