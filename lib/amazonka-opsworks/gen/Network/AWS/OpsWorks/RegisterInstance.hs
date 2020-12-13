{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers instances that were created outside of AWS OpsWorks Stacks with a specified stack.
--
-- Registered instances have the same requirements as instances that are created by using the 'CreateInstance' API. For example, registered instances must be running a supported Linux-based operating system, and they must have a supported instance type. For more information about requirements for instances that you want to register, see <https://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register-registering-preparer.html Preparing the Instance> .
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterInstance
  ( -- * Creating a request
    RegisterInstance (..),
    mkRegisterInstance,

    -- ** Request lenses
    riPrivateIP,
    riHostname,
    riInstanceIdentity,
    riPublicIP,
    riStackId,
    riRsaPublicKeyFingerprint,
    riRsaPublicKey,

    -- * Destructuring the response
    RegisterInstanceResponse (..),
    mkRegisterInstanceResponse,

    -- ** Response lenses
    rirsInstanceId,
    rirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { -- | The instance's private IP address.
    privateIP :: Lude.Maybe Lude.Text,
    -- | The instance's hostname.
    hostname :: Lude.Maybe Lude.Text,
    -- | An InstanceIdentity object that contains the instance's identity.
    instanceIdentity :: Lude.Maybe InstanceIdentity,
    -- | The instance's public IP address.
    publicIP :: Lude.Maybe Lude.Text,
    -- | The ID of the stack that the instance is to be registered with.
    stackId :: Lude.Text,
    -- | The instances public RSA key fingerprint.
    rsaPublicKeyFingerprint :: Lude.Maybe Lude.Text,
    -- | The instances public RSA key. This key is used to encrypt communication between the instance and the service.
    rsaPublicKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstance' with the minimum fields required to make a request.
--
-- * 'privateIP' - The instance's private IP address.
-- * 'hostname' - The instance's hostname.
-- * 'instanceIdentity' - An InstanceIdentity object that contains the instance's identity.
-- * 'publicIP' - The instance's public IP address.
-- * 'stackId' - The ID of the stack that the instance is to be registered with.
-- * 'rsaPublicKeyFingerprint' - The instances public RSA key fingerprint.
-- * 'rsaPublicKey' - The instances public RSA key. This key is used to encrypt communication between the instance and the service.
mkRegisterInstance ::
  -- | 'stackId'
  Lude.Text ->
  RegisterInstance
mkRegisterInstance pStackId_ =
  RegisterInstance'
    { privateIP = Lude.Nothing,
      hostname = Lude.Nothing,
      instanceIdentity = Lude.Nothing,
      publicIP = Lude.Nothing,
      stackId = pStackId_,
      rsaPublicKeyFingerprint = Lude.Nothing,
      rsaPublicKey = Lude.Nothing
    }

-- | The instance's private IP address.
--
-- /Note:/ Consider using 'privateIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPrivateIP :: Lens.Lens' RegisterInstance (Lude.Maybe Lude.Text)
riPrivateIP = Lens.lens (privateIP :: RegisterInstance -> Lude.Maybe Lude.Text) (\s a -> s {privateIP = a} :: RegisterInstance)
{-# DEPRECATED riPrivateIP "Use generic-lens or generic-optics with 'privateIP' instead." #-}

-- | The instance's hostname.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riHostname :: Lens.Lens' RegisterInstance (Lude.Maybe Lude.Text)
riHostname = Lens.lens (hostname :: RegisterInstance -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: RegisterInstance)
{-# DEPRECATED riHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | An InstanceIdentity object that contains the instance's identity.
--
-- /Note:/ Consider using 'instanceIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceIdentity :: Lens.Lens' RegisterInstance (Lude.Maybe InstanceIdentity)
riInstanceIdentity = Lens.lens (instanceIdentity :: RegisterInstance -> Lude.Maybe InstanceIdentity) (\s a -> s {instanceIdentity = a} :: RegisterInstance)
{-# DEPRECATED riInstanceIdentity "Use generic-lens or generic-optics with 'instanceIdentity' instead." #-}

-- | The instance's public IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPublicIP :: Lens.Lens' RegisterInstance (Lude.Maybe Lude.Text)
riPublicIP = Lens.lens (publicIP :: RegisterInstance -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: RegisterInstance)
{-# DEPRECATED riPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | The ID of the stack that the instance is to be registered with.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riStackId :: Lens.Lens' RegisterInstance Lude.Text
riStackId = Lens.lens (stackId :: RegisterInstance -> Lude.Text) (\s a -> s {stackId = a} :: RegisterInstance)
{-# DEPRECATED riStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The instances public RSA key fingerprint.
--
-- /Note:/ Consider using 'rsaPublicKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRsaPublicKeyFingerprint :: Lens.Lens' RegisterInstance (Lude.Maybe Lude.Text)
riRsaPublicKeyFingerprint = Lens.lens (rsaPublicKeyFingerprint :: RegisterInstance -> Lude.Maybe Lude.Text) (\s a -> s {rsaPublicKeyFingerprint = a} :: RegisterInstance)
{-# DEPRECATED riRsaPublicKeyFingerprint "Use generic-lens or generic-optics with 'rsaPublicKeyFingerprint' instead." #-}

-- | The instances public RSA key. This key is used to encrypt communication between the instance and the service.
--
-- /Note:/ Consider using 'rsaPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRsaPublicKey :: Lens.Lens' RegisterInstance (Lude.Maybe Lude.Text)
riRsaPublicKey = Lens.lens (rsaPublicKey :: RegisterInstance -> Lude.Maybe Lude.Text) (\s a -> s {rsaPublicKey = a} :: RegisterInstance)
{-# DEPRECATED riRsaPublicKey "Use generic-lens or generic-optics with 'rsaPublicKey' instead." #-}

instance Lude.AWSRequest RegisterInstance where
  type Rs RegisterInstance = RegisterInstanceResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterInstanceResponse'
            Lude.<$> (x Lude..?> "InstanceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RegisterInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterInstance where
  toJSON RegisterInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PrivateIp" Lude..=) Lude.<$> privateIP,
            ("Hostname" Lude..=) Lude.<$> hostname,
            ("InstanceIdentity" Lude..=) Lude.<$> instanceIdentity,
            ("PublicIp" Lude..=) Lude.<$> publicIP,
            Lude.Just ("StackId" Lude..= stackId),
            ("RsaPublicKeyFingerprint" Lude..=)
              Lude.<$> rsaPublicKeyFingerprint,
            ("RsaPublicKey" Lude..=) Lude.<$> rsaPublicKey
          ]
      )

instance Lude.ToPath RegisterInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterInstance where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @RegisterInstanceResult@ request.
--
-- /See:/ 'mkRegisterInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
  { -- | The registered instance's AWS OpsWorks Stacks ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The registered instance's AWS OpsWorks Stacks ID.
-- * 'responseStatus' - The response status code.
mkRegisterInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterInstanceResponse
mkRegisterInstanceResponse pResponseStatus_ =
  RegisterInstanceResponse'
    { instanceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registered instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsInstanceId :: Lens.Lens' RegisterInstanceResponse (Lude.Maybe Lude.Text)
rirsInstanceId = Lens.lens (instanceId :: RegisterInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: RegisterInstanceResponse)
{-# DEPRECATED rirsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsResponseStatus :: Lens.Lens' RegisterInstanceResponse Lude.Int
rirsResponseStatus = Lens.lens (responseStatus :: RegisterInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterInstanceResponse)
{-# DEPRECATED rirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
