{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the security groups currently in effect for a mount target. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not @deleted@ .
--
-- This operation requires permissions for the following actions:
--
--     * @elasticfilesystem:DescribeMountTargetSecurityGroups@ action on the mount target's file system.
--
--
--     * @ec2:DescribeNetworkInterfaceAttribute@ action on the mount target's network interface.
module Network.AWS.EFS.DescribeMountTargetSecurityGroups
  ( -- * Creating a request
    DescribeMountTargetSecurityGroups (..),
    mkDescribeMountTargetSecurityGroups,

    -- ** Request lenses
    dmtsgMountTargetId,

    -- * Destructuring the response
    DescribeMountTargetSecurityGroupsResponse (..),
    mkDescribeMountTargetSecurityGroupsResponse,

    -- ** Response lenses
    dmtsgrsSecurityGroups,
    dmtsgrsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeMountTargetSecurityGroups' smart constructor.
newtype DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
  { -- | The ID of the mount target whose security groups you want to retrieve.
    mountTargetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMountTargetSecurityGroups' with the minimum fields required to make a request.
--
-- * 'mountTargetId' - The ID of the mount target whose security groups you want to retrieve.
mkDescribeMountTargetSecurityGroups ::
  -- | 'mountTargetId'
  Lude.Text ->
  DescribeMountTargetSecurityGroups
mkDescribeMountTargetSecurityGroups pMountTargetId_ =
  DescribeMountTargetSecurityGroups'
    { mountTargetId =
        pMountTargetId_
    }

-- | The ID of the mount target whose security groups you want to retrieve.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgMountTargetId :: Lens.Lens' DescribeMountTargetSecurityGroups Lude.Text
dmtsgMountTargetId = Lens.lens (mountTargetId :: DescribeMountTargetSecurityGroups -> Lude.Text) (\s a -> s {mountTargetId = a} :: DescribeMountTargetSecurityGroups)
{-# DEPRECATED dmtsgMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

instance Lude.AWSRequest DescribeMountTargetSecurityGroups where
  type
    Rs DescribeMountTargetSecurityGroups =
      DescribeMountTargetSecurityGroupsResponse
  request = Req.get efsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMountTargetSecurityGroupsResponse'
            Lude.<$> (x Lude..?> "SecurityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMountTargetSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeMountTargetSecurityGroups where
  toPath DescribeMountTargetSecurityGroups' {..} =
    Lude.mconcat
      [ "/2015-02-01/mount-targets/",
        Lude.toBS mountTargetId,
        "/security-groups"
      ]

instance Lude.ToQuery DescribeMountTargetSecurityGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMountTargetSecurityGroupsResponse' smart constructor.
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
  { -- | An array of security groups.
    securityGroups :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMountTargetSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'securityGroups' - An array of security groups.
-- * 'responseStatus' - The response status code.
mkDescribeMountTargetSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMountTargetSecurityGroupsResponse
mkDescribeMountTargetSecurityGroupsResponse pResponseStatus_ =
  DescribeMountTargetSecurityGroupsResponse'
    { securityGroups =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | An array of security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgrsSecurityGroups :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse [Lude.Text]
dmtsgrsSecurityGroups = Lens.lens (securityGroups :: DescribeMountTargetSecurityGroupsResponse -> [Lude.Text]) (\s a -> s {securityGroups = a} :: DescribeMountTargetSecurityGroupsResponse)
{-# DEPRECATED dmtsgrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgrsResponseStatus :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse Lude.Int
dmtsgrsResponseStatus = Lens.lens (responseStatus :: DescribeMountTargetSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMountTargetSecurityGroupsResponse)
{-# DEPRECATED dmtsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
