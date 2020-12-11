{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CancelInstanceRefresh
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an instance refresh operation in progress. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.CancelInstanceRefresh
  ( -- * Creating a request
    CancelInstanceRefresh (..),
    mkCancelInstanceRefresh,

    -- ** Request lenses
    cirAutoScalingGroupName,

    -- * Destructuring the response
    CancelInstanceRefreshResponse (..),
    mkCancelInstanceRefreshResponse,

    -- ** Response lenses
    cirrsInstanceRefreshId,
    cirrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelInstanceRefresh' smart constructor.
newtype CancelInstanceRefresh = CancelInstanceRefresh'
  { autoScalingGroupName ::
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

-- | Creates a value of 'CancelInstanceRefresh' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkCancelInstanceRefresh ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  CancelInstanceRefresh
mkCancelInstanceRefresh pAutoScalingGroupName_ =
  CancelInstanceRefresh'
    { autoScalingGroupName =
        pAutoScalingGroupName_
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirAutoScalingGroupName :: Lens.Lens' CancelInstanceRefresh Lude.Text
cirAutoScalingGroupName = Lens.lens (autoScalingGroupName :: CancelInstanceRefresh -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: CancelInstanceRefresh)
{-# DEPRECATED cirAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest CancelInstanceRefresh where
  type Rs CancelInstanceRefresh = CancelInstanceRefreshResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "CancelInstanceRefreshResult"
      ( \s h x ->
          CancelInstanceRefreshResponse'
            Lude.<$> (x Lude..@? "InstanceRefreshId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelInstanceRefresh where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelInstanceRefresh where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelInstanceRefresh where
  toQuery CancelInstanceRefresh' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelInstanceRefresh" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkCancelInstanceRefreshResponse' smart constructor.
data CancelInstanceRefreshResponse = CancelInstanceRefreshResponse'
  { instanceRefreshId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CancelInstanceRefreshResponse' with the minimum fields required to make a request.
--
-- * 'instanceRefreshId' - The instance refresh ID.
-- * 'responseStatus' - The response status code.
mkCancelInstanceRefreshResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelInstanceRefreshResponse
mkCancelInstanceRefreshResponse pResponseStatus_ =
  CancelInstanceRefreshResponse'
    { instanceRefreshId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance refresh ID.
--
-- /Note:/ Consider using 'instanceRefreshId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsInstanceRefreshId :: Lens.Lens' CancelInstanceRefreshResponse (Lude.Maybe Lude.Text)
cirrsInstanceRefreshId = Lens.lens (instanceRefreshId :: CancelInstanceRefreshResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceRefreshId = a} :: CancelInstanceRefreshResponse)
{-# DEPRECATED cirrsInstanceRefreshId "Use generic-lens or generic-optics with 'instanceRefreshId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CancelInstanceRefreshResponse Lude.Int
cirrsResponseStatus = Lens.lens (responseStatus :: CancelInstanceRefreshResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelInstanceRefreshResponse)
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
