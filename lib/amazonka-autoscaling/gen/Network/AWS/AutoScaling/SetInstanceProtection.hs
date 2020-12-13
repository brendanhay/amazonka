{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the instance protection settings of the specified instances.
--
-- For more information about preventing instances that are part of an Auto Scaling group from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you exceed your maximum limit of instance IDs, which is 50 per Auto Scaling group, the call fails.
module Network.AWS.AutoScaling.SetInstanceProtection
  ( -- * Creating a request
    SetInstanceProtection (..),
    mkSetInstanceProtection,

    -- ** Request lenses
    sipProtectedFromScaleIn,
    sipInstanceIds,
    sipAutoScalingGroupName,

    -- * Destructuring the response
    SetInstanceProtectionResponse (..),
    mkSetInstanceProtectionResponse,

    -- ** Response lenses
    siprsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetInstanceProtection' smart constructor.
data SetInstanceProtection = SetInstanceProtection'
  { -- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
    protectedFromScaleIn :: Lude.Bool,
    -- | One or more instance IDs. You can specify up to 50 instances.
    instanceIds :: [Lude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetInstanceProtection' with the minimum fields required to make a request.
--
-- * 'protectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
-- * 'instanceIds' - One or more instance IDs. You can specify up to 50 instances.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkSetInstanceProtection ::
  -- | 'protectedFromScaleIn'
  Lude.Bool ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  SetInstanceProtection
mkSetInstanceProtection
  pProtectedFromScaleIn_
  pAutoScalingGroupName_ =
    SetInstanceProtection'
      { protectedFromScaleIn =
          pProtectedFromScaleIn_,
        instanceIds = Lude.mempty,
        autoScalingGroupName = pAutoScalingGroupName_
      }

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipProtectedFromScaleIn :: Lens.Lens' SetInstanceProtection Lude.Bool
sipProtectedFromScaleIn = Lens.lens (protectedFromScaleIn :: SetInstanceProtection -> Lude.Bool) (\s a -> s {protectedFromScaleIn = a} :: SetInstanceProtection)
{-# DEPRECATED sipProtectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead." #-}

-- | One or more instance IDs. You can specify up to 50 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipInstanceIds :: Lens.Lens' SetInstanceProtection [Lude.Text]
sipInstanceIds = Lens.lens (instanceIds :: SetInstanceProtection -> [Lude.Text]) (\s a -> s {instanceIds = a} :: SetInstanceProtection)
{-# DEPRECATED sipInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipAutoScalingGroupName :: Lens.Lens' SetInstanceProtection Lude.Text
sipAutoScalingGroupName = Lens.lens (autoScalingGroupName :: SetInstanceProtection -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: SetInstanceProtection)
{-# DEPRECATED sipAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest SetInstanceProtection where
  type Rs SetInstanceProtection = SetInstanceProtectionResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "SetInstanceProtectionResult"
      ( \s h x ->
          SetInstanceProtectionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetInstanceProtection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetInstanceProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery SetInstanceProtection where
  toQuery SetInstanceProtection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetInstanceProtection" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "ProtectedFromScaleIn" Lude.=: protectedFromScaleIn,
        "InstanceIds" Lude.=: Lude.toQueryList "member" instanceIds,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkSetInstanceProtectionResponse' smart constructor.
newtype SetInstanceProtectionResponse = SetInstanceProtectionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetInstanceProtectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetInstanceProtectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetInstanceProtectionResponse
mkSetInstanceProtectionResponse pResponseStatus_ =
  SetInstanceProtectionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprsResponseStatus :: Lens.Lens' SetInstanceProtectionResponse Lude.Int
siprsResponseStatus = Lens.lens (responseStatus :: SetInstanceProtectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetInstanceProtectionResponse)
{-# DEPRECATED siprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
