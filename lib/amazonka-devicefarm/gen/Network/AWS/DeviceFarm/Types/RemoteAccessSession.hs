{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the remote access session.
--
--
--
-- /See:/ 'remoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
  { _rasBillingMethod ::
      !(Maybe BillingMethod),
    _rasClientId :: !(Maybe Text),
    _rasDeviceUdid :: !(Maybe Text),
    _rasSkipAppResign :: !(Maybe Bool),
    _rasInstanceARN :: !(Maybe Text),
    _rasStatus :: !(Maybe ExecutionStatus),
    _rasRemoteRecordEnabled :: !(Maybe Bool),
    _rasArn :: !(Maybe Text),
    _rasRemoteRecordAppARN :: !(Maybe Text),
    _rasCreated :: !(Maybe POSIX),
    _rasDevice :: !(Maybe Device),
    _rasStopped :: !(Maybe POSIX),
    _rasResult :: !(Maybe ExecutionResult),
    _rasName :: !(Maybe Text),
    _rasDeviceMinutes :: !(Maybe DeviceMinutes),
    _rasRemoteDebugEnabled :: !(Maybe Bool),
    _rasEndpoint :: !(Maybe Text),
    _rasMessage :: !(Maybe Text),
    _rasHostAddress :: !(Maybe Text),
    _rasInteractionMode :: !(Maybe InteractionMode),
    _rasStarted :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasBillingMethod' - The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
--
-- * 'rasClientId' - Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- * 'rasDeviceUdid' - Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- * 'rasSkipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'rasInstanceARN' - The ARN of the instance.
--
-- * 'rasStatus' - The status of the remote access session. Can be any of the following:     * PENDING.     * PENDING_CONCURRENCY.     * PENDING_DEVICE.     * PROCESSING.     * SCHEDULING.     * PREPARING.     * RUNNING.     * COMPLETED.     * STOPPING.
--
-- * 'rasRemoteRecordEnabled' - This flag is set to @true@ if remote recording is enabled for the remote access session.
--
-- * 'rasArn' - The Amazon Resource Name (ARN) of the remote access session.
--
-- * 'rasRemoteRecordAppARN' - The ARN for the app to be recorded in the remote access session.
--
-- * 'rasCreated' - The date and time the remote access session was created.
--
-- * 'rasDevice' - The device (phone or tablet) used in the remote access session.
--
-- * 'rasStopped' - The date and time the remote access session was stopped.
--
-- * 'rasResult' - The result of the remote access session. Can be any of the following:     * PENDING.     * PASSED.     * WARNED.     * FAILED.     * SKIPPED.     * ERRORED.     * STOPPED.
--
-- * 'rasName' - The name of the remote access session.
--
-- * 'rasDeviceMinutes' - The number of minutes a device is used in a remote access session (including setup and teardown minutes).
--
-- * 'rasRemoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- * 'rasEndpoint' - The endpoint for the remote access sesssion.
--
-- * 'rasMessage' - A message about the remote access session.
--
-- * 'rasHostAddress' - IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- * 'rasInteractionMode' - The interaction mode of the remote access session. Valid values are:     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
--
-- * 'rasStarted' - The date and time the remote access session was started.
remoteAccessSession ::
  RemoteAccessSession
remoteAccessSession =
  RemoteAccessSession'
    { _rasBillingMethod = Nothing,
      _rasClientId = Nothing,
      _rasDeviceUdid = Nothing,
      _rasSkipAppResign = Nothing,
      _rasInstanceARN = Nothing,
      _rasStatus = Nothing,
      _rasRemoteRecordEnabled = Nothing,
      _rasArn = Nothing,
      _rasRemoteRecordAppARN = Nothing,
      _rasCreated = Nothing,
      _rasDevice = Nothing,
      _rasStopped = Nothing,
      _rasResult = Nothing,
      _rasName = Nothing,
      _rasDeviceMinutes = Nothing,
      _rasRemoteDebugEnabled = Nothing,
      _rasEndpoint = Nothing,
      _rasMessage = Nothing,
      _rasHostAddress = Nothing,
      _rasInteractionMode = Nothing,
      _rasStarted = Nothing
    }

-- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> .
rasBillingMethod :: Lens' RemoteAccessSession (Maybe BillingMethod)
rasBillingMethod = lens _rasBillingMethod (\s a -> s {_rasBillingMethod = a})

-- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
rasClientId :: Lens' RemoteAccessSession (Maybe Text)
rasClientId = lens _rasClientId (\s a -> s {_rasClientId = a})

-- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
rasDeviceUdid :: Lens' RemoteAccessSession (Maybe Text)
rasDeviceUdid = lens _rasDeviceUdid (\s a -> s {_rasDeviceUdid = a})

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
rasSkipAppResign :: Lens' RemoteAccessSession (Maybe Bool)
rasSkipAppResign = lens _rasSkipAppResign (\s a -> s {_rasSkipAppResign = a})

-- | The ARN of the instance.
rasInstanceARN :: Lens' RemoteAccessSession (Maybe Text)
rasInstanceARN = lens _rasInstanceARN (\s a -> s {_rasInstanceARN = a})

-- | The status of the remote access session. Can be any of the following:     * PENDING.     * PENDING_CONCURRENCY.     * PENDING_DEVICE.     * PROCESSING.     * SCHEDULING.     * PREPARING.     * RUNNING.     * COMPLETED.     * STOPPING.
rasStatus :: Lens' RemoteAccessSession (Maybe ExecutionStatus)
rasStatus = lens _rasStatus (\s a -> s {_rasStatus = a})

-- | This flag is set to @true@ if remote recording is enabled for the remote access session.
rasRemoteRecordEnabled :: Lens' RemoteAccessSession (Maybe Bool)
rasRemoteRecordEnabled = lens _rasRemoteRecordEnabled (\s a -> s {_rasRemoteRecordEnabled = a})

-- | The Amazon Resource Name (ARN) of the remote access session.
rasArn :: Lens' RemoteAccessSession (Maybe Text)
rasArn = lens _rasArn (\s a -> s {_rasArn = a})

-- | The ARN for the app to be recorded in the remote access session.
rasRemoteRecordAppARN :: Lens' RemoteAccessSession (Maybe Text)
rasRemoteRecordAppARN = lens _rasRemoteRecordAppARN (\s a -> s {_rasRemoteRecordAppARN = a})

-- | The date and time the remote access session was created.
rasCreated :: Lens' RemoteAccessSession (Maybe UTCTime)
rasCreated = lens _rasCreated (\s a -> s {_rasCreated = a}) . mapping _Time

-- | The device (phone or tablet) used in the remote access session.
rasDevice :: Lens' RemoteAccessSession (Maybe Device)
rasDevice = lens _rasDevice (\s a -> s {_rasDevice = a})

-- | The date and time the remote access session was stopped.
rasStopped :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStopped = lens _rasStopped (\s a -> s {_rasStopped = a}) . mapping _Time

-- | The result of the remote access session. Can be any of the following:     * PENDING.     * PASSED.     * WARNED.     * FAILED.     * SKIPPED.     * ERRORED.     * STOPPED.
rasResult :: Lens' RemoteAccessSession (Maybe ExecutionResult)
rasResult = lens _rasResult (\s a -> s {_rasResult = a})

-- | The name of the remote access session.
rasName :: Lens' RemoteAccessSession (Maybe Text)
rasName = lens _rasName (\s a -> s {_rasName = a})

-- | The number of minutes a device is used in a remote access session (including setup and teardown minutes).
rasDeviceMinutes :: Lens' RemoteAccessSession (Maybe DeviceMinutes)
rasDeviceMinutes = lens _rasDeviceMinutes (\s a -> s {_rasDeviceMinutes = a})

-- | This flag is set to @true@ if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
rasRemoteDebugEnabled :: Lens' RemoteAccessSession (Maybe Bool)
rasRemoteDebugEnabled = lens _rasRemoteDebugEnabled (\s a -> s {_rasRemoteDebugEnabled = a})

-- | The endpoint for the remote access sesssion.
rasEndpoint :: Lens' RemoteAccessSession (Maybe Text)
rasEndpoint = lens _rasEndpoint (\s a -> s {_rasEndpoint = a})

-- | A message about the remote access session.
rasMessage :: Lens' RemoteAccessSession (Maybe Text)
rasMessage = lens _rasMessage (\s a -> s {_rasMessage = a})

-- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
rasHostAddress :: Lens' RemoteAccessSession (Maybe Text)
rasHostAddress = lens _rasHostAddress (\s a -> s {_rasHostAddress = a})

-- | The interaction mode of the remote access session. Valid values are:     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
rasInteractionMode :: Lens' RemoteAccessSession (Maybe InteractionMode)
rasInteractionMode = lens _rasInteractionMode (\s a -> s {_rasInteractionMode = a})

-- | The date and time the remote access session was started.
rasStarted :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStarted = lens _rasStarted (\s a -> s {_rasStarted = a}) . mapping _Time

instance FromJSON RemoteAccessSession where
  parseJSON =
    withObject
      "RemoteAccessSession"
      ( \x ->
          RemoteAccessSession'
            <$> (x .:? "billingMethod")
            <*> (x .:? "clientId")
            <*> (x .:? "deviceUdid")
            <*> (x .:? "skipAppResign")
            <*> (x .:? "instanceArn")
            <*> (x .:? "status")
            <*> (x .:? "remoteRecordEnabled")
            <*> (x .:? "arn")
            <*> (x .:? "remoteRecordAppArn")
            <*> (x .:? "created")
            <*> (x .:? "device")
            <*> (x .:? "stopped")
            <*> (x .:? "result")
            <*> (x .:? "name")
            <*> (x .:? "deviceMinutes")
            <*> (x .:? "remoteDebugEnabled")
            <*> (x .:? "endpoint")
            <*> (x .:? "message")
            <*> (x .:? "hostAddress")
            <*> (x .:? "interactionMode")
            <*> (x .:? "started")
      )

instance Hashable RemoteAccessSession

instance NFData RemoteAccessSession
