{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RemoteAccessSession where

import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.InteractionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the remote access session.
--
-- /See:/ 'newRemoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
  { -- | Unique device identifier for the remote device. Only returned if remote
    -- debugging is enabled for the remote access session.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    deviceUdid :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of your client for the remote access session. Only
    -- returned if remote debugging is enabled for the remote access session.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The status of the remote access session. Can be any of the following:
    --
    -- -   PENDING.
    --
    -- -   PENDING_CONCURRENCY.
    --
    -- -   PENDING_DEVICE.
    --
    -- -   PROCESSING.
    --
    -- -   SCHEDULING.
    --
    -- -   PREPARING.
    --
    -- -   RUNNING.
    --
    -- -   COMPLETED.
    --
    -- -   STOPPING.
    status :: Prelude.Maybe ExecutionStatus,
    -- | The result of the remote access session. Can be any of the following:
    --
    -- -   PENDING.
    --
    -- -   PASSED.
    --
    -- -   WARNED.
    --
    -- -   FAILED.
    --
    -- -   SKIPPED.
    --
    -- -   ERRORED.
    --
    -- -   STOPPED.
    result :: Prelude.Maybe ExecutionResult,
    -- | The interaction mode of the remote access session. Valid values are:
    --
    -- -   INTERACTIVE: You can interact with the iOS device by viewing,
    --     touching, and rotating the screen. You cannot run XCUITest
    --     framework-based tests in this mode.
    --
    -- -   NO_VIDEO: You are connected to the device, but cannot interact with
    --     it or view the screen. This mode has the fastest test execution
    --     speed. You can run XCUITest framework-based tests in this mode.
    --
    -- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
    --     You can run XCUITest framework-based tests and watch the screen in
    --     this mode.
    interactionMode :: Prelude.Maybe InteractionMode,
    -- | The date and time the remote access session was started.
    started :: Prelude.Maybe Prelude.POSIX,
    -- | A message about the remote access session.
    message :: Prelude.Maybe Prelude.Text,
    -- | The device (phone or tablet) used in the remote access session.
    device :: Prelude.Maybe Device,
    -- | The Amazon Resource Name (ARN) of the remote access session.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the remote access session.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The billing method of the remote access session. Possible values include
    -- @METERED@ or @UNMETERED@. For more information about metered devices,
    -- see
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology>.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | This flag is set to @true@ if remote recording is enabled for the remote
    -- access session.
    remoteRecordEnabled :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Prelude.Maybe Prelude.Bool,
    -- | The date and time the remote access session was stopped.
    stopped :: Prelude.Maybe Prelude.POSIX,
    -- | IP address of the EC2 host where you need to connect to remotely debug
    -- devices. Only returned if remote debugging is enabled for the remote
    -- access session.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    hostAddress :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the remote access sesssion.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The date and time the remote access session was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | This flag is set to @true@ if remote debugging is enabled for the remote
    -- access session.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    remoteDebugEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN for the app to be recorded in the remote access session.
    remoteRecordAppArn :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes a device is used in a remote access session
    -- (including setup and teardown minutes).
    deviceMinutes :: Prelude.Maybe DeviceMinutes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceUdid', 'remoteAccessSession_deviceUdid' - Unique device identifier for the remote device. Only returned if remote
-- debugging is enabled for the remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'clientId', 'remoteAccessSession_clientId' - Unique identifier of your client for the remote access session. Only
-- returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'status', 'remoteAccessSession_status' - The status of the remote access session. Can be any of the following:
--
-- -   PENDING.
--
-- -   PENDING_CONCURRENCY.
--
-- -   PENDING_DEVICE.
--
-- -   PROCESSING.
--
-- -   SCHEDULING.
--
-- -   PREPARING.
--
-- -   RUNNING.
--
-- -   COMPLETED.
--
-- -   STOPPING.
--
-- 'result', 'remoteAccessSession_result' - The result of the remote access session. Can be any of the following:
--
-- -   PENDING.
--
-- -   PASSED.
--
-- -   WARNED.
--
-- -   FAILED.
--
-- -   SKIPPED.
--
-- -   ERRORED.
--
-- -   STOPPED.
--
-- 'interactionMode', 'remoteAccessSession_interactionMode' - The interaction mode of the remote access session. Valid values are:
--
-- -   INTERACTIVE: You can interact with the iOS device by viewing,
--     touching, and rotating the screen. You cannot run XCUITest
--     framework-based tests in this mode.
--
-- -   NO_VIDEO: You are connected to the device, but cannot interact with
--     it or view the screen. This mode has the fastest test execution
--     speed. You can run XCUITest framework-based tests in this mode.
--
-- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
--     You can run XCUITest framework-based tests and watch the screen in
--     this mode.
--
-- 'started', 'remoteAccessSession_started' - The date and time the remote access session was started.
--
-- 'message', 'remoteAccessSession_message' - A message about the remote access session.
--
-- 'device', 'remoteAccessSession_device' - The device (phone or tablet) used in the remote access session.
--
-- 'arn', 'remoteAccessSession_arn' - The Amazon Resource Name (ARN) of the remote access session.
--
-- 'name', 'remoteAccessSession_name' - The name of the remote access session.
--
-- 'instanceArn', 'remoteAccessSession_instanceArn' - The ARN of the instance.
--
-- 'billingMethod', 'remoteAccessSession_billingMethod' - The billing method of the remote access session. Possible values include
-- @METERED@ or @UNMETERED@. For more information about metered devices,
-- see
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology>.
--
-- 'remoteRecordEnabled', 'remoteAccessSession_remoteRecordEnabled' - This flag is set to @true@ if remote recording is enabled for the remote
-- access session.
--
-- 'skipAppResign', 'remoteAccessSession_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'stopped', 'remoteAccessSession_stopped' - The date and time the remote access session was stopped.
--
-- 'hostAddress', 'remoteAccessSession_hostAddress' - IP address of the EC2 host where you need to connect to remotely debug
-- devices. Only returned if remote debugging is enabled for the remote
-- access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'endpoint', 'remoteAccessSession_endpoint' - The endpoint for the remote access sesssion.
--
-- 'created', 'remoteAccessSession_created' - The date and time the remote access session was created.
--
-- 'remoteDebugEnabled', 'remoteAccessSession_remoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the remote
-- access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'remoteRecordAppArn', 'remoteAccessSession_remoteRecordAppArn' - The ARN for the app to be recorded in the remote access session.
--
-- 'deviceMinutes', 'remoteAccessSession_deviceMinutes' - The number of minutes a device is used in a remote access session
-- (including setup and teardown minutes).
newRemoteAccessSession ::
  RemoteAccessSession
newRemoteAccessSession =
  RemoteAccessSession'
    { deviceUdid = Prelude.Nothing,
      clientId = Prelude.Nothing,
      status = Prelude.Nothing,
      result = Prelude.Nothing,
      interactionMode = Prelude.Nothing,
      started = Prelude.Nothing,
      message = Prelude.Nothing,
      device = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      billingMethod = Prelude.Nothing,
      remoteRecordEnabled = Prelude.Nothing,
      skipAppResign = Prelude.Nothing,
      stopped = Prelude.Nothing,
      hostAddress = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      created = Prelude.Nothing,
      remoteDebugEnabled = Prelude.Nothing,
      remoteRecordAppArn = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing
    }

-- | Unique device identifier for the remote device. Only returned if remote
-- debugging is enabled for the remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
remoteAccessSession_deviceUdid :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_deviceUdid = Lens.lens (\RemoteAccessSession' {deviceUdid} -> deviceUdid) (\s@RemoteAccessSession' {} a -> s {deviceUdid = a} :: RemoteAccessSession)

-- | Unique identifier of your client for the remote access session. Only
-- returned if remote debugging is enabled for the remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
remoteAccessSession_clientId :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_clientId = Lens.lens (\RemoteAccessSession' {clientId} -> clientId) (\s@RemoteAccessSession' {} a -> s {clientId = a} :: RemoteAccessSession)

-- | The status of the remote access session. Can be any of the following:
--
-- -   PENDING.
--
-- -   PENDING_CONCURRENCY.
--
-- -   PENDING_DEVICE.
--
-- -   PROCESSING.
--
-- -   SCHEDULING.
--
-- -   PREPARING.
--
-- -   RUNNING.
--
-- -   COMPLETED.
--
-- -   STOPPING.
remoteAccessSession_status :: Lens.Lens' RemoteAccessSession (Prelude.Maybe ExecutionStatus)
remoteAccessSession_status = Lens.lens (\RemoteAccessSession' {status} -> status) (\s@RemoteAccessSession' {} a -> s {status = a} :: RemoteAccessSession)

-- | The result of the remote access session. Can be any of the following:
--
-- -   PENDING.
--
-- -   PASSED.
--
-- -   WARNED.
--
-- -   FAILED.
--
-- -   SKIPPED.
--
-- -   ERRORED.
--
-- -   STOPPED.
remoteAccessSession_result :: Lens.Lens' RemoteAccessSession (Prelude.Maybe ExecutionResult)
remoteAccessSession_result = Lens.lens (\RemoteAccessSession' {result} -> result) (\s@RemoteAccessSession' {} a -> s {result = a} :: RemoteAccessSession)

-- | The interaction mode of the remote access session. Valid values are:
--
-- -   INTERACTIVE: You can interact with the iOS device by viewing,
--     touching, and rotating the screen. You cannot run XCUITest
--     framework-based tests in this mode.
--
-- -   NO_VIDEO: You are connected to the device, but cannot interact with
--     it or view the screen. This mode has the fastest test execution
--     speed. You can run XCUITest framework-based tests in this mode.
--
-- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
--     You can run XCUITest framework-based tests and watch the screen in
--     this mode.
remoteAccessSession_interactionMode :: Lens.Lens' RemoteAccessSession (Prelude.Maybe InteractionMode)
remoteAccessSession_interactionMode = Lens.lens (\RemoteAccessSession' {interactionMode} -> interactionMode) (\s@RemoteAccessSession' {} a -> s {interactionMode = a} :: RemoteAccessSession)

-- | The date and time the remote access session was started.
remoteAccessSession_started :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.UTCTime)
remoteAccessSession_started = Lens.lens (\RemoteAccessSession' {started} -> started) (\s@RemoteAccessSession' {} a -> s {started = a} :: RemoteAccessSession) Prelude.. Lens.mapping Prelude._Time

-- | A message about the remote access session.
remoteAccessSession_message :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_message = Lens.lens (\RemoteAccessSession' {message} -> message) (\s@RemoteAccessSession' {} a -> s {message = a} :: RemoteAccessSession)

-- | The device (phone or tablet) used in the remote access session.
remoteAccessSession_device :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Device)
remoteAccessSession_device = Lens.lens (\RemoteAccessSession' {device} -> device) (\s@RemoteAccessSession' {} a -> s {device = a} :: RemoteAccessSession)

-- | The Amazon Resource Name (ARN) of the remote access session.
remoteAccessSession_arn :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_arn = Lens.lens (\RemoteAccessSession' {arn} -> arn) (\s@RemoteAccessSession' {} a -> s {arn = a} :: RemoteAccessSession)

-- | The name of the remote access session.
remoteAccessSession_name :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_name = Lens.lens (\RemoteAccessSession' {name} -> name) (\s@RemoteAccessSession' {} a -> s {name = a} :: RemoteAccessSession)

-- | The ARN of the instance.
remoteAccessSession_instanceArn :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_instanceArn = Lens.lens (\RemoteAccessSession' {instanceArn} -> instanceArn) (\s@RemoteAccessSession' {} a -> s {instanceArn = a} :: RemoteAccessSession)

-- | The billing method of the remote access session. Possible values include
-- @METERED@ or @UNMETERED@. For more information about metered devices,
-- see
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology>.
remoteAccessSession_billingMethod :: Lens.Lens' RemoteAccessSession (Prelude.Maybe BillingMethod)
remoteAccessSession_billingMethod = Lens.lens (\RemoteAccessSession' {billingMethod} -> billingMethod) (\s@RemoteAccessSession' {} a -> s {billingMethod = a} :: RemoteAccessSession)

-- | This flag is set to @true@ if remote recording is enabled for the remote
-- access session.
remoteAccessSession_remoteRecordEnabled :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Bool)
remoteAccessSession_remoteRecordEnabled = Lens.lens (\RemoteAccessSession' {remoteRecordEnabled} -> remoteRecordEnabled) (\s@RemoteAccessSession' {} a -> s {remoteRecordEnabled = a} :: RemoteAccessSession)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
remoteAccessSession_skipAppResign :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Bool)
remoteAccessSession_skipAppResign = Lens.lens (\RemoteAccessSession' {skipAppResign} -> skipAppResign) (\s@RemoteAccessSession' {} a -> s {skipAppResign = a} :: RemoteAccessSession)

-- | The date and time the remote access session was stopped.
remoteAccessSession_stopped :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.UTCTime)
remoteAccessSession_stopped = Lens.lens (\RemoteAccessSession' {stopped} -> stopped) (\s@RemoteAccessSession' {} a -> s {stopped = a} :: RemoteAccessSession) Prelude.. Lens.mapping Prelude._Time

-- | IP address of the EC2 host where you need to connect to remotely debug
-- devices. Only returned if remote debugging is enabled for the remote
-- access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
remoteAccessSession_hostAddress :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_hostAddress = Lens.lens (\RemoteAccessSession' {hostAddress} -> hostAddress) (\s@RemoteAccessSession' {} a -> s {hostAddress = a} :: RemoteAccessSession)

-- | The endpoint for the remote access sesssion.
remoteAccessSession_endpoint :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_endpoint = Lens.lens (\RemoteAccessSession' {endpoint} -> endpoint) (\s@RemoteAccessSession' {} a -> s {endpoint = a} :: RemoteAccessSession)

-- | The date and time the remote access session was created.
remoteAccessSession_created :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.UTCTime)
remoteAccessSession_created = Lens.lens (\RemoteAccessSession' {created} -> created) (\s@RemoteAccessSession' {} a -> s {created = a} :: RemoteAccessSession) Prelude.. Lens.mapping Prelude._Time

-- | This flag is set to @true@ if remote debugging is enabled for the remote
-- access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
remoteAccessSession_remoteDebugEnabled :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Bool)
remoteAccessSession_remoteDebugEnabled = Lens.lens (\RemoteAccessSession' {remoteDebugEnabled} -> remoteDebugEnabled) (\s@RemoteAccessSession' {} a -> s {remoteDebugEnabled = a} :: RemoteAccessSession)

-- | The ARN for the app to be recorded in the remote access session.
remoteAccessSession_remoteRecordAppArn :: Lens.Lens' RemoteAccessSession (Prelude.Maybe Prelude.Text)
remoteAccessSession_remoteRecordAppArn = Lens.lens (\RemoteAccessSession' {remoteRecordAppArn} -> remoteRecordAppArn) (\s@RemoteAccessSession' {} a -> s {remoteRecordAppArn = a} :: RemoteAccessSession)

-- | The number of minutes a device is used in a remote access session
-- (including setup and teardown minutes).
remoteAccessSession_deviceMinutes :: Lens.Lens' RemoteAccessSession (Prelude.Maybe DeviceMinutes)
remoteAccessSession_deviceMinutes = Lens.lens (\RemoteAccessSession' {deviceMinutes} -> deviceMinutes) (\s@RemoteAccessSession' {} a -> s {deviceMinutes = a} :: RemoteAccessSession)

instance Prelude.FromJSON RemoteAccessSession where
  parseJSON =
    Prelude.withObject
      "RemoteAccessSession"
      ( \x ->
          RemoteAccessSession'
            Prelude.<$> (x Prelude..:? "deviceUdid")
            Prelude.<*> (x Prelude..:? "clientId")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..:? "interactionMode")
            Prelude.<*> (x Prelude..:? "started")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "device")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "instanceArn")
            Prelude.<*> (x Prelude..:? "billingMethod")
            Prelude.<*> (x Prelude..:? "remoteRecordEnabled")
            Prelude.<*> (x Prelude..:? "skipAppResign")
            Prelude.<*> (x Prelude..:? "stopped")
            Prelude.<*> (x Prelude..:? "hostAddress")
            Prelude.<*> (x Prelude..:? "endpoint")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "remoteDebugEnabled")
            Prelude.<*> (x Prelude..:? "remoteRecordAppArn")
            Prelude.<*> (x Prelude..:? "deviceMinutes")
      )

instance Prelude.Hashable RemoteAccessSession

instance Prelude.NFData RemoteAccessSession
