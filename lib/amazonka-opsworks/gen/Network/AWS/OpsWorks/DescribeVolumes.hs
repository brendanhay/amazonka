{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an instance's Amazon EBS volumes.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeVolumes
  ( -- * Creating a request
    DescribeVolumes (..),
    mkDescribeVolumes,

    -- ** Request lenses
    dvInstanceId,
    dvVolumeIds,
    dvRAIDArrayId,
    dvStackId,

    -- * Destructuring the response
    DescribeVolumesResponse (..),
    mkDescribeVolumesResponse,

    -- ** Response lenses
    dvrsVolumes,
    dvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
    volumeIds :: Lude.Maybe [Lude.Text],
    -- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
    raidArrayId :: Lude.Maybe Lude.Text,
    -- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumes' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
-- * 'volumeIds' - Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
-- * 'raidArrayId' - The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
-- * 'stackId' - A stack ID. The action describes the stack's registered Amazon EBS volumes.
mkDescribeVolumes ::
  DescribeVolumes
mkDescribeVolumes =
  DescribeVolumes'
    { instanceId = Lude.Nothing,
      volumeIds = Lude.Nothing,
      raidArrayId = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvInstanceId :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Text)
dvInstanceId = Lens.lens (instanceId :: DescribeVolumes -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeVolumes)
{-# DEPRECATED dvInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeIds :: Lens.Lens' DescribeVolumes (Lude.Maybe [Lude.Text])
dvVolumeIds = Lens.lens (volumeIds :: DescribeVolumes -> Lude.Maybe [Lude.Text]) (\s a -> s {volumeIds = a} :: DescribeVolumes)
{-# DEPRECATED dvVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvRAIDArrayId :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Text)
dvRAIDArrayId = Lens.lens (raidArrayId :: DescribeVolumes -> Lude.Maybe Lude.Text) (\s a -> s {raidArrayId = a} :: DescribeVolumes)
{-# DEPRECATED dvRAIDArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead." #-}

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvStackId :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Text)
dvStackId = Lens.lens (stackId :: DescribeVolumes -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeVolumes)
{-# DEPRECATED dvStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeVolumes where
  type Rs DescribeVolumes = DescribeVolumesResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVolumesResponse'
            Lude.<$> (x Lude..?> "Volumes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVolumes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeVolumes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVolumes where
  toJSON DescribeVolumes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("VolumeIds" Lude..=) Lude.<$> volumeIds,
            ("RaidArrayId" Lude..=) Lude.<$> raidArrayId,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeVolumes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVolumes where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'mkDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | An array of volume IDs.
    volumes :: Lude.Maybe [Volume],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumesResponse' with the minimum fields required to make a request.
--
-- * 'volumes' - An array of volume IDs.
-- * 'responseStatus' - The response status code.
mkDescribeVolumesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVolumesResponse
mkDescribeVolumesResponse pResponseStatus_ =
  DescribeVolumesResponse'
    { volumes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of volume IDs.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsVolumes :: Lens.Lens' DescribeVolumesResponse (Lude.Maybe [Volume])
dvrsVolumes = Lens.lens (volumes :: DescribeVolumesResponse -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: DescribeVolumesResponse)
{-# DEPRECATED dvrsVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsResponseStatus :: Lens.Lens' DescribeVolumesResponse Lude.Int
dvrsResponseStatus = Lens.lens (responseStatus :: DescribeVolumesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVolumesResponse)
{-# DEPRECATED dvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
