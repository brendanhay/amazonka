{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ModifyInstanceGroups modifies the number of nodes and configuration settings of an instance group. The input parameters include the new target instance count for the group and the instance group ID. The call will either succeed or fail atomically.
module Network.AWS.EMR.ModifyInstanceGroups
  ( -- * Creating a request
    ModifyInstanceGroups (..),
    mkModifyInstanceGroups,

    -- ** Request lenses
    migClusterId,
    migInstanceGroups,

    -- * Destructuring the response
    ModifyInstanceGroupsResponse (..),
    mkModifyInstanceGroupsResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Change the size of some instance groups.
--
-- /See:/ 'mkModifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { clusterId ::
      Lude.Maybe Lude.Text,
    instanceGroups ::
      Lude.Maybe [InstanceGroupModifyConfig]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceGroups' with the minimum fields required to make a request.
--
-- * 'clusterId' - The ID of the cluster to which the instance group belongs.
-- * 'instanceGroups' - Instance groups to change.
mkModifyInstanceGroups ::
  ModifyInstanceGroups
mkModifyInstanceGroups =
  ModifyInstanceGroups'
    { clusterId = Lude.Nothing,
      instanceGroups = Lude.Nothing
    }

-- | The ID of the cluster to which the instance group belongs.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
migClusterId :: Lens.Lens' ModifyInstanceGroups (Lude.Maybe Lude.Text)
migClusterId = Lens.lens (clusterId :: ModifyInstanceGroups -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: ModifyInstanceGroups)
{-# DEPRECATED migClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Instance groups to change.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
migInstanceGroups :: Lens.Lens' ModifyInstanceGroups (Lude.Maybe [InstanceGroupModifyConfig])
migInstanceGroups = Lens.lens (instanceGroups :: ModifyInstanceGroups -> Lude.Maybe [InstanceGroupModifyConfig]) (\s a -> s {instanceGroups = a} :: ModifyInstanceGroups)
{-# DEPRECATED migInstanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead." #-}

instance Lude.AWSRequest ModifyInstanceGroups where
  type Rs ModifyInstanceGroups = ModifyInstanceGroupsResponse
  request = Req.postJSON emrService
  response = Res.receiveNull ModifyInstanceGroupsResponse'

instance Lude.ToHeaders ModifyInstanceGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ModifyInstanceGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyInstanceGroups where
  toJSON ModifyInstanceGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClusterId" Lude..=) Lude.<$> clusterId,
            ("InstanceGroups" Lude..=) Lude.<$> instanceGroups
          ]
      )

instance Lude.ToPath ModifyInstanceGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceGroupsResponse' with the minimum fields required to make a request.
mkModifyInstanceGroupsResponse ::
  ModifyInstanceGroupsResponse
mkModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
