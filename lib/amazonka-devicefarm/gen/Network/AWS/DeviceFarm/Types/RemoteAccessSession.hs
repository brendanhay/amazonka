{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RemoteAccessSession
  ( RemoteAccessSession (..),

    -- * Smart constructor
    mkRemoteAccessSession,

    -- * Lenses
    rasArn,
    rasBillingMethod,
    rasClientId,
    rasCreated,
    rasDevice,
    rasDeviceMinutes,
    rasDeviceUdid,
    rasEndpoint,
    rasHostAddress,
    rasInstanceArn,
    rasInteractionMode,
    rasMessage,
    rasName,
    rasRemoteDebugEnabled,
    rasRemoteRecordAppArn,
    rasRemoteRecordEnabled,
    rasResult,
    rasSkipAppResign,
    rasStarted,
    rasStatus,
    rasStopped,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.BillingMethod as Types
import qualified Network.AWS.DeviceFarm.Types.ClientId as Types
import qualified Network.AWS.DeviceFarm.Types.Device as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceMinutes as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionStatus as Types
import qualified Network.AWS.DeviceFarm.Types.HostAddress as Types
import qualified Network.AWS.DeviceFarm.Types.InteractionMode as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the remote access session.
--
-- /See:/ 'mkRemoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session.
    arn :: Core.Maybe Types.AmazonResourceName,
    -- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
    billingMethod :: Core.Maybe Types.BillingMethod,
    -- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    clientId :: Core.Maybe Types.ClientId,
    -- | The date and time the remote access session was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The device (phone or tablet) used in the remote access session.
    device :: Core.Maybe Types.Device,
    -- | The number of minutes a device is used in a remote access session (including setup and teardown minutes).
    deviceMinutes :: Core.Maybe Types.DeviceMinutes,
    -- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    deviceUdid :: Core.Maybe Types.String,
    -- | The endpoint for the remote access sesssion.
    endpoint :: Core.Maybe Types.String,
    -- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    hostAddress :: Core.Maybe Types.HostAddress,
    -- | The ARN of the instance.
    instanceArn :: Core.Maybe Types.AmazonResourceName,
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
    -- | A message about the remote access session.
    message :: Core.Maybe Types.Message,
    -- | The name of the remote access session.
    name :: Core.Maybe Types.Name,
    -- | This flag is set to @true@ if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    remoteDebugEnabled :: Core.Maybe Core.Bool,
    -- | The ARN for the app to be recorded in the remote access session.
    remoteRecordAppArn :: Core.Maybe Types.AmazonResourceName,
    -- | This flag is set to @true@ if remote recording is enabled for the remote access session.
    remoteRecordEnabled :: Core.Maybe Core.Bool,
    -- | The result of the remote access session. Can be any of the following:
    --
    --
    --     * PENDING.
    --
    --
    --     * PASSED.
    --
    --
    --     * WARNED.
    --
    --
    --     * FAILED.
    --
    --
    --     * SKIPPED.
    --
    --
    --     * ERRORED.
    --
    --
    --     * STOPPED.
    result :: Core.Maybe Types.ExecutionResult,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
    skipAppResign :: Core.Maybe Core.Bool,
    -- | The date and time the remote access session was started.
    started :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the remote access session. Can be any of the following:
    --
    --
    --     * PENDING.
    --
    --
    --     * PENDING_CONCURRENCY.
    --
    --
    --     * PENDING_DEVICE.
    --
    --
    --     * PROCESSING.
    --
    --
    --     * SCHEDULING.
    --
    --
    --     * PREPARING.
    --
    --
    --     * RUNNING.
    --
    --
    --     * COMPLETED.
    --
    --
    --     * STOPPING.
    status :: Core.Maybe Types.ExecutionStatus,
    -- | The date and time the remote access session was stopped.
    stopped :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RemoteAccessSession' value with any optional fields omitted.
mkRemoteAccessSession ::
  RemoteAccessSession
mkRemoteAccessSession =
  RemoteAccessSession'
    { arn = Core.Nothing,
      billingMethod = Core.Nothing,
      clientId = Core.Nothing,
      created = Core.Nothing,
      device = Core.Nothing,
      deviceMinutes = Core.Nothing,
      deviceUdid = Core.Nothing,
      endpoint = Core.Nothing,
      hostAddress = Core.Nothing,
      instanceArn = Core.Nothing,
      interactionMode = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      remoteDebugEnabled = Core.Nothing,
      remoteRecordAppArn = Core.Nothing,
      remoteRecordEnabled = Core.Nothing,
      result = Core.Nothing,
      skipAppResign = Core.Nothing,
      started = Core.Nothing,
      status = Core.Nothing,
      stopped = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the remote access session.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasArn :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.AmazonResourceName)
rasArn = Lens.field @"arn"
{-# DEPRECATED rasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasBillingMethod :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.BillingMethod)
rasBillingMethod = Lens.field @"billingMethod"
{-# DEPRECATED rasBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasClientId :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.ClientId)
rasClientId = Lens.field @"clientId"
{-# DEPRECATED rasClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The date and time the remote access session was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasCreated :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.NominalDiffTime)
rasCreated = Lens.field @"created"
{-# DEPRECATED rasCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The device (phone or tablet) used in the remote access session.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDevice :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.Device)
rasDevice = Lens.field @"device"
{-# DEPRECATED rasDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The number of minutes a device is used in a remote access session (including setup and teardown minutes).
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDeviceMinutes :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.DeviceMinutes)
rasDeviceMinutes = Lens.field @"deviceMinutes"
{-# DEPRECATED rasDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'deviceUdid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDeviceUdid :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.String)
rasDeviceUdid = Lens.field @"deviceUdid"
{-# DEPRECATED rasDeviceUdid "Use generic-lens or generic-optics with 'deviceUdid' instead." #-}

-- | The endpoint for the remote access sesssion.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEndpoint :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.String)
rasEndpoint = Lens.field @"endpoint"
{-# DEPRECATED rasEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'hostAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasHostAddress :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.HostAddress)
rasHostAddress = Lens.field @"hostAddress"
{-# DEPRECATED rasHostAddress "Use generic-lens or generic-optics with 'hostAddress' instead." #-}

-- | The ARN of the instance.
--
-- /Note:/ Consider using 'instanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasInstanceArn :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.AmazonResourceName)
rasInstanceArn = Lens.field @"instanceArn"
{-# DEPRECATED rasInstanceArn "Use generic-lens or generic-optics with 'instanceArn' instead." #-}

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
rasInteractionMode :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.InteractionMode)
rasInteractionMode = Lens.field @"interactionMode"
{-# DEPRECATED rasInteractionMode "Use generic-lens or generic-optics with 'interactionMode' instead." #-}

-- | A message about the remote access session.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasMessage :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.Message)
rasMessage = Lens.field @"message"
{-# DEPRECATED rasMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The name of the remote access session.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasName :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.Name)
rasName = Lens.field @"name"
{-# DEPRECATED rasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | This flag is set to @true@ if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteDebugEnabled :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.Bool)
rasRemoteDebugEnabled = Lens.field @"remoteDebugEnabled"
{-# DEPRECATED rasRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | The ARN for the app to be recorded in the remote access session.
--
-- /Note:/ Consider using 'remoteRecordAppArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteRecordAppArn :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.AmazonResourceName)
rasRemoteRecordAppArn = Lens.field @"remoteRecordAppArn"
{-# DEPRECATED rasRemoteRecordAppArn "Use generic-lens or generic-optics with 'remoteRecordAppArn' instead." #-}

-- | This flag is set to @true@ if remote recording is enabled for the remote access session.
--
-- /Note:/ Consider using 'remoteRecordEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteRecordEnabled :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.Bool)
rasRemoteRecordEnabled = Lens.field @"remoteRecordEnabled"
{-# DEPRECATED rasRemoteRecordEnabled "Use generic-lens or generic-optics with 'remoteRecordEnabled' instead." #-}

-- | The result of the remote access session. Can be any of the following:
--
--
--     * PENDING.
--
--
--     * PASSED.
--
--
--     * WARNED.
--
--
--     * FAILED.
--
--
--     * SKIPPED.
--
--
--     * ERRORED.
--
--
--     * STOPPED.
--
--
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasResult :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.ExecutionResult)
rasResult = Lens.field @"result"
{-# DEPRECATED rasResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasSkipAppResign :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.Bool)
rasSkipAppResign = Lens.field @"skipAppResign"
{-# DEPRECATED rasSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The date and time the remote access session was started.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasStarted :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.NominalDiffTime)
rasStarted = Lens.field @"started"
{-# DEPRECATED rasStarted "Use generic-lens or generic-optics with 'started' instead." #-}

-- | The status of the remote access session. Can be any of the following:
--
--
--     * PENDING.
--
--
--     * PENDING_CONCURRENCY.
--
--
--     * PENDING_DEVICE.
--
--
--     * PROCESSING.
--
--
--     * SCHEDULING.
--
--
--     * PREPARING.
--
--
--     * RUNNING.
--
--
--     * COMPLETED.
--
--
--     * STOPPING.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasStatus :: Lens.Lens' RemoteAccessSession (Core.Maybe Types.ExecutionStatus)
rasStatus = Lens.field @"status"
{-# DEPRECATED rasStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the remote access session was stopped.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasStopped :: Lens.Lens' RemoteAccessSession (Core.Maybe Core.NominalDiffTime)
rasStopped = Lens.field @"stopped"
{-# DEPRECATED rasStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

instance Core.FromJSON RemoteAccessSession where
  parseJSON =
    Core.withObject "RemoteAccessSession" Core.$
      \x ->
        RemoteAccessSession'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "billingMethod")
          Core.<*> (x Core..:? "clientId")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "device")
          Core.<*> (x Core..:? "deviceMinutes")
          Core.<*> (x Core..:? "deviceUdid")
          Core.<*> (x Core..:? "endpoint")
          Core.<*> (x Core..:? "hostAddress")
          Core.<*> (x Core..:? "instanceArn")
          Core.<*> (x Core..:? "interactionMode")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "remoteDebugEnabled")
          Core.<*> (x Core..:? "remoteRecordAppArn")
          Core.<*> (x Core..:? "remoteRecordEnabled")
          Core.<*> (x Core..:? "result")
          Core.<*> (x Core..:? "skipAppResign")
          Core.<*> (x Core..:? "started")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "stopped")
