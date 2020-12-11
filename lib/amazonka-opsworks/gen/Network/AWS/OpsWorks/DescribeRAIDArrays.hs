{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRAIDArrays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an instance's RAID arrays.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeRAIDArrays
  ( -- * Creating a request
    DescribeRAIDArrays (..),
    mkDescribeRAIDArrays,

    -- ** Request lenses
    draiaInstanceId,
    draiaRAIDArrayIds,
    draiaStackId,

    -- * Destructuring the response
    DescribeRAIdArraysResponse (..),
    mkDescribeRAIdArraysResponse,

    -- ** Response lenses
    draiarsRAIDArrays,
    draiarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRAIDArrays' smart constructor.
data DescribeRAIDArrays = DescribeRAIDArrays'
  { instanceId ::
      Lude.Maybe Lude.Text,
    raidArrayIds :: Lude.Maybe [Lude.Text],
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRAIDArrays' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance.
-- * 'raidArrayIds' - An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
-- * 'stackId' - The stack ID.
mkDescribeRAIDArrays ::
  DescribeRAIDArrays
mkDescribeRAIDArrays =
  DescribeRAIDArrays'
    { instanceId = Lude.Nothing,
      raidArrayIds = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draiaInstanceId :: Lens.Lens' DescribeRAIDArrays (Lude.Maybe Lude.Text)
draiaInstanceId = Lens.lens (instanceId :: DescribeRAIDArrays -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeRAIDArrays)
{-# DEPRECATED draiaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
--
-- /Note:/ Consider using 'raidArrayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draiaRAIDArrayIds :: Lens.Lens' DescribeRAIDArrays (Lude.Maybe [Lude.Text])
draiaRAIDArrayIds = Lens.lens (raidArrayIds :: DescribeRAIDArrays -> Lude.Maybe [Lude.Text]) (\s a -> s {raidArrayIds = a} :: DescribeRAIDArrays)
{-# DEPRECATED draiaRAIDArrayIds "Use generic-lens or generic-optics with 'raidArrayIds' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draiaStackId :: Lens.Lens' DescribeRAIDArrays (Lude.Maybe Lude.Text)
draiaStackId = Lens.lens (stackId :: DescribeRAIDArrays -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeRAIDArrays)
{-# DEPRECATED draiaStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeRAIDArrays where
  type Rs DescribeRAIDArrays = DescribeRAIdArraysResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRAIdArraysResponse'
            Lude.<$> (x Lude..?> "RaidArrays" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRAIDArrays where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeRaidArrays" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRAIDArrays where
  toJSON DescribeRAIDArrays' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("RaidArrayIds" Lude..=) Lude.<$> raidArrayIds,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeRAIDArrays where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRAIDArrays where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeRaidArrays@ request.
--
-- /See:/ 'mkDescribeRAIdArraysResponse' smart constructor.
data DescribeRAIdArraysResponse = DescribeRAIdArraysResponse'
  { raidArrays ::
      Lude.Maybe [RAIDArray],
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

-- | Creates a value of 'DescribeRAIdArraysResponse' with the minimum fields required to make a request.
--
-- * 'raidArrays' - A @RaidArrays@ object that describes the specified RAID arrays.
-- * 'responseStatus' - The response status code.
mkDescribeRAIdArraysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRAIdArraysResponse
mkDescribeRAIdArraysResponse pResponseStatus_ =
  DescribeRAIdArraysResponse'
    { raidArrays = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @RaidArrays@ object that describes the specified RAID arrays.
--
-- /Note:/ Consider using 'raidArrays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draiarsRAIDArrays :: Lens.Lens' DescribeRAIdArraysResponse (Lude.Maybe [RAIDArray])
draiarsRAIDArrays = Lens.lens (raidArrays :: DescribeRAIdArraysResponse -> Lude.Maybe [RAIDArray]) (\s a -> s {raidArrays = a} :: DescribeRAIdArraysResponse)
{-# DEPRECATED draiarsRAIDArrays "Use generic-lens or generic-optics with 'raidArrays' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draiarsResponseStatus :: Lens.Lens' DescribeRAIdArraysResponse Lude.Int
draiarsResponseStatus = Lens.lens (responseStatus :: DescribeRAIdArraysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRAIdArraysResponse)
{-# DEPRECATED draiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
