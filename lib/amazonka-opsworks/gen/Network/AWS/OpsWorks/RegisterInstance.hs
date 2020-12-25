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
    riStackId,
    riHostname,
    riInstanceIdentity,
    riPrivateIp,
    riPublicIp,
    riRsaPublicKey,
    riRsaPublicKeyFingerprint,

    -- * Destructuring the response
    RegisterInstanceResponse (..),
    mkRegisterInstanceResponse,

    -- ** Response lenses
    rirrsInstanceId,
    rirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { -- | The ID of the stack that the instance is to be registered with.
    stackId :: Types.String,
    -- | The instance's hostname.
    hostname :: Core.Maybe Types.String,
    -- | An InstanceIdentity object that contains the instance's identity.
    instanceIdentity :: Core.Maybe Types.InstanceIdentity,
    -- | The instance's private IP address.
    privateIp :: Core.Maybe Types.String,
    -- | The instance's public IP address.
    publicIp :: Core.Maybe Types.String,
    -- | The instances public RSA key. This key is used to encrypt communication between the instance and the service.
    rsaPublicKey :: Core.Maybe Types.String,
    -- | The instances public RSA key fingerprint.
    rsaPublicKeyFingerprint :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstance' value with any optional fields omitted.
mkRegisterInstance ::
  -- | 'stackId'
  Types.String ->
  RegisterInstance
mkRegisterInstance stackId =
  RegisterInstance'
    { stackId,
      hostname = Core.Nothing,
      instanceIdentity = Core.Nothing,
      privateIp = Core.Nothing,
      publicIp = Core.Nothing,
      rsaPublicKey = Core.Nothing,
      rsaPublicKeyFingerprint = Core.Nothing
    }

-- | The ID of the stack that the instance is to be registered with.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riStackId :: Lens.Lens' RegisterInstance Types.String
riStackId = Lens.field @"stackId"
{-# DEPRECATED riStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The instance's hostname.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riHostname :: Lens.Lens' RegisterInstance (Core.Maybe Types.String)
riHostname = Lens.field @"hostname"
{-# DEPRECATED riHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | An InstanceIdentity object that contains the instance's identity.
--
-- /Note:/ Consider using 'instanceIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceIdentity :: Lens.Lens' RegisterInstance (Core.Maybe Types.InstanceIdentity)
riInstanceIdentity = Lens.field @"instanceIdentity"
{-# DEPRECATED riInstanceIdentity "Use generic-lens or generic-optics with 'instanceIdentity' instead." #-}

-- | The instance's private IP address.
--
-- /Note:/ Consider using 'privateIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPrivateIp :: Lens.Lens' RegisterInstance (Core.Maybe Types.String)
riPrivateIp = Lens.field @"privateIp"
{-# DEPRECATED riPrivateIp "Use generic-lens or generic-optics with 'privateIp' instead." #-}

-- | The instance's public IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPublicIp :: Lens.Lens' RegisterInstance (Core.Maybe Types.String)
riPublicIp = Lens.field @"publicIp"
{-# DEPRECATED riPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | The instances public RSA key. This key is used to encrypt communication between the instance and the service.
--
-- /Note:/ Consider using 'rsaPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRsaPublicKey :: Lens.Lens' RegisterInstance (Core.Maybe Types.String)
riRsaPublicKey = Lens.field @"rsaPublicKey"
{-# DEPRECATED riRsaPublicKey "Use generic-lens or generic-optics with 'rsaPublicKey' instead." #-}

-- | The instances public RSA key fingerprint.
--
-- /Note:/ Consider using 'rsaPublicKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRsaPublicKeyFingerprint :: Lens.Lens' RegisterInstance (Core.Maybe Types.String)
riRsaPublicKeyFingerprint = Lens.field @"rsaPublicKeyFingerprint"
{-# DEPRECATED riRsaPublicKeyFingerprint "Use generic-lens or generic-optics with 'rsaPublicKeyFingerprint' instead." #-}

instance Core.FromJSON RegisterInstance where
  toJSON RegisterInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            ("Hostname" Core..=) Core.<$> hostname,
            ("InstanceIdentity" Core..=) Core.<$> instanceIdentity,
            ("PrivateIp" Core..=) Core.<$> privateIp,
            ("PublicIp" Core..=) Core.<$> publicIp,
            ("RsaPublicKey" Core..=) Core.<$> rsaPublicKey,
            ("RsaPublicKeyFingerprint" Core..=)
              Core.<$> rsaPublicKeyFingerprint
          ]
      )

instance Core.AWSRequest RegisterInstance where
  type Rs RegisterInstance = RegisterInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.RegisterInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterInstanceResponse'
            Core.<$> (x Core..:? "InstanceId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @RegisterInstanceResult@ request.
--
-- /See:/ 'mkRegisterInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
  { -- | The registered instance's AWS OpsWorks Stacks ID.
    instanceId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstanceResponse' value with any optional fields omitted.
mkRegisterInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterInstanceResponse
mkRegisterInstanceResponse responseStatus =
  RegisterInstanceResponse'
    { instanceId = Core.Nothing,
      responseStatus
    }

-- | The registered instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsInstanceId :: Lens.Lens' RegisterInstanceResponse (Core.Maybe Types.String)
rirrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED rirrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RegisterInstanceResponse Core.Int
rirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
