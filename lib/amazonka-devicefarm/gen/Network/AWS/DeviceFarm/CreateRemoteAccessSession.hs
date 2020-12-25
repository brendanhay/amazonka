{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies and starts a remote access session.
module Network.AWS.DeviceFarm.CreateRemoteAccessSession
  ( -- * Creating a request
    CreateRemoteAccessSession (..),
    mkCreateRemoteAccessSession,

    -- ** Request lenses
    crasProjectArn,
    crasDeviceArn,
    crasClientId,
    crasConfiguration,
    crasInstanceArn,
    crasInteractionMode,
    crasName,
    crasRemoteDebugEnabled,
    crasRemoteRecordAppArn,
    crasRemoteRecordEnabled,
    crasSkipAppResign,
    crasSshPublicKey,

    -- * Destructuring the response
    CreateRemoteAccessSessionResponse (..),
    mkCreateRemoteAccessSessionResponse,

    -- ** Response lenses
    crasrrsRemoteAccessSession,
    crasrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates and submits a request to start a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSession' smart constructor.
data CreateRemoteAccessSession = CreateRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
    projectArn :: Types.ProjectArn,
    -- | The ARN of the device for which you want to create a remote access session.
    deviceArn :: Types.DeviceArn,
    -- | Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    clientId :: Core.Maybe Types.ClientId,
    -- | The configuration information for the remote access session request.
    configuration :: Core.Maybe Types.CreateRemoteAccessSessionConfiguration,
    -- | The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
    instanceArn :: Core.Maybe Types.InstanceArn,
    -- | The interaction mode of the remote access session. Valid values are:
    --
    --
    --     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.
    --
    --
    --     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.
    --
    --
    --     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
    interactionMode :: Core.Maybe Types.InteractionMode,
    -- | The name of the remote access session to create.
    name :: Core.Maybe Types.Name,
    -- | Set to @true@ if you want to access devices remotely for debugging in your remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    remoteDebugEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
    remoteRecordAppArn :: Core.Maybe Types.RemoteRecordAppArn,
    -- | Set to @true@ to enable remote recording for the remote access session.
    remoteRecordEnabled :: Core.Maybe Core.Bool,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
    skipAppResign :: Core.Maybe Core.Bool,
    -- | Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    sshPublicKey :: Core.Maybe Types.SshPublicKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRemoteAccessSession' value with any optional fields omitted.
mkCreateRemoteAccessSession ::
  -- | 'projectArn'
  Types.ProjectArn ->
  -- | 'deviceArn'
  Types.DeviceArn ->
  CreateRemoteAccessSession
mkCreateRemoteAccessSession projectArn deviceArn =
  CreateRemoteAccessSession'
    { projectArn,
      deviceArn,
      clientId = Core.Nothing,
      configuration = Core.Nothing,
      instanceArn = Core.Nothing,
      interactionMode = Core.Nothing,
      name = Core.Nothing,
      remoteDebugEnabled = Core.Nothing,
      remoteRecordAppArn = Core.Nothing,
      remoteRecordEnabled = Core.Nothing,
      skipAppResign = Core.Nothing,
      sshPublicKey = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasProjectArn :: Lens.Lens' CreateRemoteAccessSession Types.ProjectArn
crasProjectArn = Lens.field @"projectArn"
{-# DEPRECATED crasProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The ARN of the device for which you want to create a remote access session.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasDeviceArn :: Lens.Lens' CreateRemoteAccessSession Types.DeviceArn
crasDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED crasDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasClientId :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.ClientId)
crasClientId = Lens.field @"clientId"
{-# DEPRECATED crasClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The configuration information for the remote access session request.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasConfiguration :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.CreateRemoteAccessSessionConfiguration)
crasConfiguration = Lens.field @"configuration"
{-# DEPRECATED crasConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
--
-- /Note:/ Consider using 'instanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasInstanceArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.InstanceArn)
crasInstanceArn = Lens.field @"instanceArn"
{-# DEPRECATED crasInstanceArn "Use generic-lens or generic-optics with 'instanceArn' instead." #-}

-- | The interaction mode of the remote access session. Valid values are:
--
--
--     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.
--
--
--     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.
--
--
--     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
--
--
--
-- /Note:/ Consider using 'interactionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasInteractionMode :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.InteractionMode)
crasInteractionMode = Lens.field @"interactionMode"
{-# DEPRECATED crasInteractionMode "Use generic-lens or generic-optics with 'interactionMode' instead." #-}

-- | The name of the remote access session to create.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasName :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.Name)
crasName = Lens.field @"name"
{-# DEPRECATED crasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Set to @true@ if you want to access devices remotely for debugging in your remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteDebugEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasRemoteDebugEnabled = Lens.field @"remoteDebugEnabled"
{-# DEPRECATED crasRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
--
-- /Note:/ Consider using 'remoteRecordAppArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordAppArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.RemoteRecordAppArn)
crasRemoteRecordAppArn = Lens.field @"remoteRecordAppArn"
{-# DEPRECATED crasRemoteRecordAppArn "Use generic-lens or generic-optics with 'remoteRecordAppArn' instead." #-}

-- | Set to @true@ to enable remote recording for the remote access session.
--
-- /Note:/ Consider using 'remoteRecordEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasRemoteRecordEnabled = Lens.field @"remoteRecordEnabled"
{-# DEPRECATED crasRemoteRecordEnabled "Use generic-lens or generic-optics with 'remoteRecordEnabled' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSkipAppResign :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasSkipAppResign = Lens.field @"skipAppResign"
{-# DEPRECATED crasSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSshPublicKey :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.SshPublicKey)
crasSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED crasSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

instance Core.FromJSON CreateRemoteAccessSession where
  toJSON CreateRemoteAccessSession {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("deviceArn" Core..= deviceArn),
            ("clientId" Core..=) Core.<$> clientId,
            ("configuration" Core..=) Core.<$> configuration,
            ("instanceArn" Core..=) Core.<$> instanceArn,
            ("interactionMode" Core..=) Core.<$> interactionMode,
            ("name" Core..=) Core.<$> name,
            ("remoteDebugEnabled" Core..=) Core.<$> remoteDebugEnabled,
            ("remoteRecordAppArn" Core..=) Core.<$> remoteRecordAppArn,
            ("remoteRecordEnabled" Core..=) Core.<$> remoteRecordEnabled,
            ("skipAppResign" Core..=) Core.<$> skipAppResign,
            ("sshPublicKey" Core..=) Core.<$> sshPublicKey
          ]
      )

instance Core.AWSRequest CreateRemoteAccessSession where
  type
    Rs CreateRemoteAccessSession =
      CreateRemoteAccessSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.CreateRemoteAccessSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRemoteAccessSessionResponse'
            Core.<$> (x Core..:? "remoteAccessSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the server response from a request to create a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSessionResponse' smart constructor.
data CreateRemoteAccessSessionResponse = CreateRemoteAccessSessionResponse'
  { -- | A container that describes the remote access session when the request to create a remote access session is sent.
    remoteAccessSession :: Core.Maybe Types.RemoteAccessSession,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateRemoteAccessSessionResponse' value with any optional fields omitted.
mkCreateRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRemoteAccessSessionResponse
mkCreateRemoteAccessSessionResponse responseStatus =
  CreateRemoteAccessSessionResponse'
    { remoteAccessSession =
        Core.Nothing,
      responseStatus
    }

-- | A container that describes the remote access session when the request to create a remote access session is sent.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrrsRemoteAccessSession :: Lens.Lens' CreateRemoteAccessSessionResponse (Core.Maybe Types.RemoteAccessSession)
crasrrsRemoteAccessSession = Lens.field @"remoteAccessSession"
{-# DEPRECATED crasrrsRemoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrrsResponseStatus :: Lens.Lens' CreateRemoteAccessSessionResponse Core.Int
crasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
