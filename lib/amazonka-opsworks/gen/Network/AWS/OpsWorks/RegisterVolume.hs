{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon EBS volume with a specified stack. A volume can be registered with only one stack at a time. If the volume is already registered, you must first deregister it by calling 'DeregisterVolume' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterVolume
  ( -- * Creating a request
    RegisterVolume (..),
    mkRegisterVolume,

    -- ** Request lenses
    rvStackId,
    rvEC2VolumeId,

    -- * Destructuring the response
    RegisterVolumeResponse (..),
    mkRegisterVolumeResponse,

    -- ** Response lenses
    rvrsVolumeId,
    rvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterVolume' smart constructor.
data RegisterVolume = RegisterVolume'
  { -- | The stack ID.
    stackId :: Lude.Text,
    -- | The Amazon EBS volume ID.
    ec2VolumeId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterVolume' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
-- * 'ec2VolumeId' - The Amazon EBS volume ID.
mkRegisterVolume ::
  -- | 'stackId'
  Lude.Text ->
  RegisterVolume
mkRegisterVolume pStackId_ =
  RegisterVolume' {stackId = pStackId_, ec2VolumeId = Lude.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvStackId :: Lens.Lens' RegisterVolume Lude.Text
rvStackId = Lens.lens (stackId :: RegisterVolume -> Lude.Text) (\s a -> s {stackId = a} :: RegisterVolume)
{-# DEPRECATED rvStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The Amazon EBS volume ID.
--
-- /Note:/ Consider using 'ec2VolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvEC2VolumeId :: Lens.Lens' RegisterVolume (Lude.Maybe Lude.Text)
rvEC2VolumeId = Lens.lens (ec2VolumeId :: RegisterVolume -> Lude.Maybe Lude.Text) (\s a -> s {ec2VolumeId = a} :: RegisterVolume)
{-# DEPRECATED rvEC2VolumeId "Use generic-lens or generic-optics with 'ec2VolumeId' instead." #-}

instance Lude.AWSRequest RegisterVolume where
  type Rs RegisterVolume = RegisterVolumeResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterVolumeResponse'
            Lude.<$> (x Lude..?> "VolumeId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RegisterVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterVolume where
  toJSON RegisterVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StackId" Lude..= stackId),
            ("Ec2VolumeId" Lude..=) Lude.<$> ec2VolumeId
          ]
      )

instance Lude.ToPath RegisterVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterVolume where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @RegisterVolume@ request.
--
-- /See:/ 'mkRegisterVolumeResponse' smart constructor.
data RegisterVolumeResponse = RegisterVolumeResponse'
  { -- | The volume ID.
    volumeId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterVolumeResponse' with the minimum fields required to make a request.
--
-- * 'volumeId' - The volume ID.
-- * 'responseStatus' - The response status code.
mkRegisterVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterVolumeResponse
mkRegisterVolumeResponse pResponseStatus_ =
  RegisterVolumeResponse'
    { volumeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvrsVolumeId :: Lens.Lens' RegisterVolumeResponse (Lude.Maybe Lude.Text)
rvrsVolumeId = Lens.lens (volumeId :: RegisterVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: RegisterVolumeResponse)
{-# DEPRECATED rvrsVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvrsResponseStatus :: Lens.Lens' RegisterVolumeResponse Lude.Int
rvrsResponseStatus = Lens.lens (responseStatus :: RegisterVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterVolumeResponse)
{-# DEPRECATED rvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
