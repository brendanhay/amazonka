{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRemoteAccessSession (..)
    , mkCreateRemoteAccessSession
    -- ** Request lenses
    , crasProjectArn
    , crasDeviceArn
    , crasClientId
    , crasConfiguration
    , crasInstanceArn
    , crasInteractionMode
    , crasName
    , crasRemoteDebugEnabled
    , crasRemoteRecordAppArn
    , crasRemoteRecordEnabled
    , crasSkipAppResign
    , crasSshPublicKey

    -- * Destructuring the response
    , CreateRemoteAccessSessionResponse (..)
    , mkCreateRemoteAccessSessionResponse
    -- ** Response lenses
    , crasrrsRemoteAccessSession
    , crasrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates and submits a request to start a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSession' smart constructor.
data CreateRemoteAccessSession = CreateRemoteAccessSession'
  { projectArn :: Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
  , deviceArn :: Types.DeviceArn
    -- ^ The ARN of the device for which you want to create a remote access session.
  , clientId :: Core.Maybe Types.ClientId
    -- ^ Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
  , configuration :: Core.Maybe Types.CreateRemoteAccessSessionConfiguration
    -- ^ The configuration information for the remote access session request.
  , instanceArn :: Core.Maybe Types.InstanceArn
    -- ^ The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
  , interactionMode :: Core.Maybe Types.InteractionMode
    -- ^ The interaction mode of the remote access session. Valid values are:
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
  , name :: Core.Maybe Types.Name
    -- ^ The name of the remote access session to create.
  , remoteDebugEnabled :: Core.Maybe Core.Bool
    -- ^ Set to @true@ if you want to access devices remotely for debugging in your remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
  , remoteRecordAppArn :: Core.Maybe Types.RemoteRecordAppArn
    -- ^ The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
  , remoteRecordEnabled :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to enable remote recording for the remote access session.
  , skipAppResign :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> 
  , sshPublicKey :: Core.Maybe Types.SshPublicKey
    -- ^ Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRemoteAccessSession' value with any optional fields omitted.
mkCreateRemoteAccessSession
    :: Types.ProjectArn -- ^ 'projectArn'
    -> Types.DeviceArn -- ^ 'deviceArn'
    -> CreateRemoteAccessSession
mkCreateRemoteAccessSession projectArn deviceArn
  = CreateRemoteAccessSession'{projectArn, deviceArn,
                               clientId = Core.Nothing, configuration = Core.Nothing,
                               instanceArn = Core.Nothing, interactionMode = Core.Nothing,
                               name = Core.Nothing, remoteDebugEnabled = Core.Nothing,
                               remoteRecordAppArn = Core.Nothing,
                               remoteRecordEnabled = Core.Nothing, skipAppResign = Core.Nothing,
                               sshPublicKey = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasProjectArn :: Lens.Lens' CreateRemoteAccessSession Types.ProjectArn
crasProjectArn = Lens.field @"projectArn"
{-# INLINEABLE crasProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The ARN of the device for which you want to create a remote access session.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasDeviceArn :: Lens.Lens' CreateRemoteAccessSession Types.DeviceArn
crasDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE crasDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasClientId :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.ClientId)
crasClientId = Lens.field @"clientId"
{-# INLINEABLE crasClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The configuration information for the remote access session request.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasConfiguration :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.CreateRemoteAccessSessionConfiguration)
crasConfiguration = Lens.field @"configuration"
{-# INLINEABLE crasConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
--
-- /Note:/ Consider using 'instanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasInstanceArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.InstanceArn)
crasInstanceArn = Lens.field @"instanceArn"
{-# INLINEABLE crasInstanceArn #-}
{-# DEPRECATED instanceArn "Use generic-lens or generic-optics with 'instanceArn' instead"  #-}

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
{-# INLINEABLE crasInteractionMode #-}
{-# DEPRECATED interactionMode "Use generic-lens or generic-optics with 'interactionMode' instead"  #-}

-- | The name of the remote access session to create.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasName :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.Name)
crasName = Lens.field @"name"
{-# INLINEABLE crasName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Set to @true@ if you want to access devices remotely for debugging in your remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteDebugEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasRemoteDebugEnabled = Lens.field @"remoteDebugEnabled"
{-# INLINEABLE crasRemoteDebugEnabled #-}
{-# DEPRECATED remoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead"  #-}

-- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
--
-- /Note:/ Consider using 'remoteRecordAppArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordAppArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.RemoteRecordAppArn)
crasRemoteRecordAppArn = Lens.field @"remoteRecordAppArn"
{-# INLINEABLE crasRemoteRecordAppArn #-}
{-# DEPRECATED remoteRecordAppArn "Use generic-lens or generic-optics with 'remoteRecordAppArn' instead"  #-}

-- | Set to @true@ to enable remote recording for the remote access session.
--
-- /Note:/ Consider using 'remoteRecordEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasRemoteRecordEnabled = Lens.field @"remoteRecordEnabled"
{-# INLINEABLE crasRemoteRecordEnabled #-}
{-# DEPRECATED remoteRecordEnabled "Use generic-lens or generic-optics with 'remoteRecordEnabled' instead"  #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> 
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSkipAppResign :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
crasSkipAppResign = Lens.field @"skipAppResign"
{-# INLINEABLE crasSkipAppResign #-}
{-# DEPRECATED skipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead"  #-}

-- | Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSshPublicKey :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Types.SshPublicKey)
crasSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE crasSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

instance Core.ToQuery CreateRemoteAccessSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRemoteAccessSession where
        toHeaders CreateRemoteAccessSession{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateRemoteAccessSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRemoteAccessSession where
        toJSON CreateRemoteAccessSession{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
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
                  ("sshPublicKey" Core..=) Core.<$> sshPublicKey])

instance Core.AWSRequest CreateRemoteAccessSession where
        type Rs CreateRemoteAccessSession =
             CreateRemoteAccessSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRemoteAccessSessionResponse' Core.<$>
                   (x Core..:? "remoteAccessSession") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the server response from a request to create a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSessionResponse' smart constructor.
data CreateRemoteAccessSessionResponse = CreateRemoteAccessSessionResponse'
  { remoteAccessSession :: Core.Maybe Types.RemoteAccessSession
    -- ^ A container that describes the remote access session when the request to create a remote access session is sent.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateRemoteAccessSessionResponse' value with any optional fields omitted.
mkCreateRemoteAccessSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRemoteAccessSessionResponse
mkCreateRemoteAccessSessionResponse responseStatus
  = CreateRemoteAccessSessionResponse'{remoteAccessSession =
                                         Core.Nothing,
                                       responseStatus}

-- | A container that describes the remote access session when the request to create a remote access session is sent.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrrsRemoteAccessSession :: Lens.Lens' CreateRemoteAccessSessionResponse (Core.Maybe Types.RemoteAccessSession)
crasrrsRemoteAccessSession = Lens.field @"remoteAccessSession"
{-# INLINEABLE crasrrsRemoteAccessSession #-}
{-# DEPRECATED remoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrrsResponseStatus :: Lens.Lens' CreateRemoteAccessSessionResponse Core.Int
crasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
