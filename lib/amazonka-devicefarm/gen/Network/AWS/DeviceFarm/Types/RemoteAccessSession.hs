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
    rasBillingMethod,
    rasClientId,
    rasDeviceUdid,
    rasSkipAppResign,
    rasInstanceARN,
    rasStatus,
    rasRemoteRecordEnabled,
    rasArn,
    rasRemoteRecordAppARN,
    rasCreated,
    rasDevice,
    rasStopped,
    rasResult,
    rasName,
    rasDeviceMinutes,
    rasRemoteDebugEnabled,
    rasEndpoint,
    rasMessage,
    rasHostAddress,
    rasInteractionMode,
    rasStarted,
  )
where

import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.InteractionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the remote access session.
--
-- /See:/ 'mkRemoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
  { -- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
    billingMethod :: Lude.Maybe BillingMethod,
    -- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    clientId :: Lude.Maybe Lude.Text,
    -- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    deviceUdid :: Lude.Maybe Lude.Text,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
    skipAppResign :: Lude.Maybe Lude.Bool,
    -- | The ARN of the instance.
    instanceARN :: Lude.Maybe Lude.Text,
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
    status :: Lude.Maybe ExecutionStatus,
    -- | This flag is set to @true@ if remote recording is enabled for the remote access session.
    remoteRecordEnabled :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the remote access session.
    arn :: Lude.Maybe Lude.Text,
    -- | The ARN for the app to be recorded in the remote access session.
    remoteRecordAppARN :: Lude.Maybe Lude.Text,
    -- | The date and time the remote access session was created.
    created :: Lude.Maybe Lude.Timestamp,
    -- | The device (phone or tablet) used in the remote access session.
    device :: Lude.Maybe Device,
    -- | The date and time the remote access session was stopped.
    stopped :: Lude.Maybe Lude.Timestamp,
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
    result :: Lude.Maybe ExecutionResult,
    -- | The name of the remote access session.
    name :: Lude.Maybe Lude.Text,
    -- | The number of minutes a device is used in a remote access session (including setup and teardown minutes).
    deviceMinutes :: Lude.Maybe DeviceMinutes,
    -- | This flag is set to @true@ if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    remoteDebugEnabled :: Lude.Maybe Lude.Bool,
    -- | The endpoint for the remote access sesssion.
    endpoint :: Lude.Maybe Lude.Text,
    -- | A message about the remote access session.
    message :: Lude.Maybe Lude.Text,
    -- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    hostAddress :: Lude.Maybe Lude.Text,
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
    interactionMode :: Lude.Maybe InteractionMode,
    -- | The date and time the remote access session was started.
    started :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'billingMethod' - The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
-- * 'clientId' - Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'deviceUdid' - Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'skipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
-- * 'instanceARN' - The ARN of the instance.
-- * 'status' - The status of the remote access session. Can be any of the following:
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
-- * 'remoteRecordEnabled' - This flag is set to @true@ if remote recording is enabled for the remote access session.
-- * 'arn' - The Amazon Resource Name (ARN) of the remote access session.
-- * 'remoteRecordAppARN' - The ARN for the app to be recorded in the remote access session.
-- * 'created' - The date and time the remote access session was created.
-- * 'device' - The device (phone or tablet) used in the remote access session.
-- * 'stopped' - The date and time the remote access session was stopped.
-- * 'result' - The result of the remote access session. Can be any of the following:
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
-- * 'name' - The name of the remote access session.
-- * 'deviceMinutes' - The number of minutes a device is used in a remote access session (including setup and teardown minutes).
-- * 'remoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'endpoint' - The endpoint for the remote access sesssion.
-- * 'message' - A message about the remote access session.
-- * 'hostAddress' - IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'interactionMode' - The interaction mode of the remote access session. Valid values are:
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
-- * 'started' - The date and time the remote access session was started.
mkRemoteAccessSession ::
  RemoteAccessSession
mkRemoteAccessSession =
  RemoteAccessSession'
    { billingMethod = Lude.Nothing,
      clientId = Lude.Nothing,
      deviceUdid = Lude.Nothing,
      skipAppResign = Lude.Nothing,
      instanceARN = Lude.Nothing,
      status = Lude.Nothing,
      remoteRecordEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      remoteRecordAppARN = Lude.Nothing,
      created = Lude.Nothing,
      device = Lude.Nothing,
      stopped = Lude.Nothing,
      result = Lude.Nothing,
      name = Lude.Nothing,
      deviceMinutes = Lude.Nothing,
      remoteDebugEnabled = Lude.Nothing,
      endpoint = Lude.Nothing,
      message = Lude.Nothing,
      hostAddress = Lude.Nothing,
      interactionMode = Lude.Nothing,
      started = Lude.Nothing
    }

-- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasBillingMethod :: Lens.Lens' RemoteAccessSession (Lude.Maybe BillingMethod)
rasBillingMethod = Lens.lens (billingMethod :: RemoteAccessSession -> Lude.Maybe BillingMethod) (\s a -> s {billingMethod = a} :: RemoteAccessSession)
{-# DEPRECATED rasBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasClientId :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasClientId = Lens.lens (clientId :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: RemoteAccessSession)
{-# DEPRECATED rasClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'deviceUdid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDeviceUdid :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasDeviceUdid = Lens.lens (deviceUdid :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {deviceUdid = a} :: RemoteAccessSession)
{-# DEPRECATED rasDeviceUdid "Use generic-lens or generic-optics with 'deviceUdid' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasSkipAppResign :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Bool)
rasSkipAppResign = Lens.lens (skipAppResign :: RemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {skipAppResign = a} :: RemoteAccessSession)
{-# DEPRECATED rasSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The ARN of the instance.
--
-- /Note:/ Consider using 'instanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasInstanceARN :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasInstanceARN = Lens.lens (instanceARN :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {instanceARN = a} :: RemoteAccessSession)
{-# DEPRECATED rasInstanceARN "Use generic-lens or generic-optics with 'instanceARN' instead." #-}

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
rasStatus :: Lens.Lens' RemoteAccessSession (Lude.Maybe ExecutionStatus)
rasStatus = Lens.lens (status :: RemoteAccessSession -> Lude.Maybe ExecutionStatus) (\s a -> s {status = a} :: RemoteAccessSession)
{-# DEPRECATED rasStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | This flag is set to @true@ if remote recording is enabled for the remote access session.
--
-- /Note:/ Consider using 'remoteRecordEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteRecordEnabled :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Bool)
rasRemoteRecordEnabled = Lens.lens (remoteRecordEnabled :: RemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {remoteRecordEnabled = a} :: RemoteAccessSession)
{-# DEPRECATED rasRemoteRecordEnabled "Use generic-lens or generic-optics with 'remoteRecordEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the remote access session.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasArn :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasArn = Lens.lens (arn :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RemoteAccessSession)
{-# DEPRECATED rasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ARN for the app to be recorded in the remote access session.
--
-- /Note:/ Consider using 'remoteRecordAppARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteRecordAppARN :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasRemoteRecordAppARN = Lens.lens (remoteRecordAppARN :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {remoteRecordAppARN = a} :: RemoteAccessSession)
{-# DEPRECATED rasRemoteRecordAppARN "Use generic-lens or generic-optics with 'remoteRecordAppARN' instead." #-}

-- | The date and time the remote access session was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasCreated :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Timestamp)
rasCreated = Lens.lens (created :: RemoteAccessSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: RemoteAccessSession)
{-# DEPRECATED rasCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The device (phone or tablet) used in the remote access session.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDevice :: Lens.Lens' RemoteAccessSession (Lude.Maybe Device)
rasDevice = Lens.lens (device :: RemoteAccessSession -> Lude.Maybe Device) (\s a -> s {device = a} :: RemoteAccessSession)
{-# DEPRECATED rasDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The date and time the remote access session was stopped.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasStopped :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Timestamp)
rasStopped = Lens.lens (stopped :: RemoteAccessSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopped = a} :: RemoteAccessSession)
{-# DEPRECATED rasStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

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
rasResult :: Lens.Lens' RemoteAccessSession (Lude.Maybe ExecutionResult)
rasResult = Lens.lens (result :: RemoteAccessSession -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: RemoteAccessSession)
{-# DEPRECATED rasResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The name of the remote access session.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasName :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasName = Lens.lens (name :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RemoteAccessSession)
{-# DEPRECATED rasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of minutes a device is used in a remote access session (including setup and teardown minutes).
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasDeviceMinutes :: Lens.Lens' RemoteAccessSession (Lude.Maybe DeviceMinutes)
rasDeviceMinutes = Lens.lens (deviceMinutes :: RemoteAccessSession -> Lude.Maybe DeviceMinutes) (\s a -> s {deviceMinutes = a} :: RemoteAccessSession)
{-# DEPRECATED rasDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | This flag is set to @true@ if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasRemoteDebugEnabled :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Bool)
rasRemoteDebugEnabled = Lens.lens (remoteDebugEnabled :: RemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {remoteDebugEnabled = a} :: RemoteAccessSession)
{-# DEPRECATED rasRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | The endpoint for the remote access sesssion.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEndpoint :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasEndpoint = Lens.lens (endpoint :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: RemoteAccessSession)
{-# DEPRECATED rasEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | A message about the remote access session.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasMessage :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasMessage = Lens.lens (message :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: RemoteAccessSession)
{-# DEPRECATED rasMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'hostAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasHostAddress :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Text)
rasHostAddress = Lens.lens (hostAddress :: RemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {hostAddress = a} :: RemoteAccessSession)
{-# DEPRECATED rasHostAddress "Use generic-lens or generic-optics with 'hostAddress' instead." #-}

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
rasInteractionMode :: Lens.Lens' RemoteAccessSession (Lude.Maybe InteractionMode)
rasInteractionMode = Lens.lens (interactionMode :: RemoteAccessSession -> Lude.Maybe InteractionMode) (\s a -> s {interactionMode = a} :: RemoteAccessSession)
{-# DEPRECATED rasInteractionMode "Use generic-lens or generic-optics with 'interactionMode' instead." #-}

-- | The date and time the remote access session was started.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasStarted :: Lens.Lens' RemoteAccessSession (Lude.Maybe Lude.Timestamp)
rasStarted = Lens.lens (started :: RemoteAccessSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: RemoteAccessSession)
{-# DEPRECATED rasStarted "Use generic-lens or generic-optics with 'started' instead." #-}

instance Lude.FromJSON RemoteAccessSession where
  parseJSON =
    Lude.withObject
      "RemoteAccessSession"
      ( \x ->
          RemoteAccessSession'
            Lude.<$> (x Lude..:? "billingMethod")
            Lude.<*> (x Lude..:? "clientId")
            Lude.<*> (x Lude..:? "deviceUdid")
            Lude.<*> (x Lude..:? "skipAppResign")
            Lude.<*> (x Lude..:? "instanceArn")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "remoteRecordEnabled")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "remoteRecordAppArn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "device")
            Lude.<*> (x Lude..:? "stopped")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "deviceMinutes")
            Lude.<*> (x Lude..:? "remoteDebugEnabled")
            Lude.<*> (x Lude..:? "endpoint")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "hostAddress")
            Lude.<*> (x Lude..:? "interactionMode")
            Lude.<*> (x Lude..:? "started")
      )
