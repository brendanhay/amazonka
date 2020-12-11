{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterEcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Amazon ECS cluster from a stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html> .
module Network.AWS.OpsWorks.DeregisterEcsCluster
  ( -- * Creating a request
    DeregisterEcsCluster (..),
    mkDeregisterEcsCluster,

    -- ** Request lenses
    decEcsClusterARN,

    -- * Destructuring the response
    DeregisterEcsClusterResponse (..),
    mkDeregisterEcsClusterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterEcsCluster' smart constructor.
newtype DeregisterEcsCluster = DeregisterEcsCluster'
  { ecsClusterARN ::
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

-- | Creates a value of 'DeregisterEcsCluster' with the minimum fields required to make a request.
--
-- * 'ecsClusterARN' - The cluster's Amazon Resource Number (ARN).
mkDeregisterEcsCluster ::
  -- | 'ecsClusterARN'
  Lude.Text ->
  DeregisterEcsCluster
mkDeregisterEcsCluster pEcsClusterARN_ =
  DeregisterEcsCluster' {ecsClusterARN = pEcsClusterARN_}

-- | The cluster's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEcsClusterARN :: Lens.Lens' DeregisterEcsCluster Lude.Text
decEcsClusterARN = Lens.lens (ecsClusterARN :: DeregisterEcsCluster -> Lude.Text) (\s a -> s {ecsClusterARN = a} :: DeregisterEcsCluster)
{-# DEPRECATED decEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

instance Lude.AWSRequest DeregisterEcsCluster where
  type Rs DeregisterEcsCluster = DeregisterEcsClusterResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeregisterEcsClusterResponse'

instance Lude.ToHeaders DeregisterEcsCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeregisterEcsCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterEcsCluster where
  toJSON DeregisterEcsCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EcsClusterArn" Lude..= ecsClusterARN)]
      )

instance Lude.ToPath DeregisterEcsCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterEcsCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterEcsClusterResponse' smart constructor.
data DeregisterEcsClusterResponse = DeregisterEcsClusterResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterEcsClusterResponse' with the minimum fields required to make a request.
mkDeregisterEcsClusterResponse ::
  DeregisterEcsClusterResponse
mkDeregisterEcsClusterResponse = DeregisterEcsClusterResponse'
