{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Fleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Fleet where

import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.FleetState
import Network.AWS.AppStream.Types.FleetType
import Network.AWS.AppStream.Types.StreamView
import Network.AWS.AppStream.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a fleet.
--
--
--
-- /See:/ 'fleet' smart constructor.
data Fleet = Fleet'
  { _fDomainJoinInfo :: !(Maybe DomainJoinInfo),
    _fIAMRoleARN :: !(Maybe Text),
    _fDisconnectTimeoutInSeconds :: !(Maybe Int),
    _fMaxUserDurationInSeconds :: !(Maybe Int),
    _fCreatedTime :: !(Maybe POSIX),
    _fIdleDisconnectTimeoutInSeconds :: !(Maybe Int),
    _fFleetType :: !(Maybe FleetType),
    _fVPCConfig :: !(Maybe VPCConfig),
    _fImageARN :: !(Maybe Text),
    _fFleetErrors :: !(Maybe [FleetError]),
    _fDisplayName :: !(Maybe Text),
    _fEnableDefaultInternetAccess :: !(Maybe Bool),
    _fImageName :: !(Maybe Text),
    _fDescription :: !(Maybe Text),
    _fStreamView :: !(Maybe StreamView),
    _fARN :: !Text,
    _fName :: !Text,
    _fInstanceType :: !Text,
    _fComputeCapacityStatus :: !ComputeCapacityStatus,
    _fState :: !FleetState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- * 'fIAMRoleARN' - The ARN of the IAM role that is applied to the fleet. To assume a role, the fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'fDisconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users disconnect. If they try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance. Specify a value between 60 and 360000.
--
-- * 'fMaxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.  Specify a value between 600 and 360000.
--
-- * 'fCreatedTime' - The time the fleet was created.
--
-- * 'fIdleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected. To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- * 'fFleetType' - The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
-- * 'fVPCConfig' - The VPC configuration for the fleet.
--
-- * 'fImageARN' - The ARN for the public, private, or shared image.
--
-- * 'fFleetErrors' - The fleet errors.
--
-- * 'fDisplayName' - The fleet name to display.
--
-- * 'fEnableDefaultInternetAccess' - Indicates whether default internet access is enabled for the fleet.
--
-- * 'fImageName' - The name of the image used to create the fleet.
--
-- * 'fDescription' - The description to display.
--
-- * 'fStreamView' - The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
--
-- * 'fARN' - The Amazon Resource Name (ARN) for the fleet.
--
-- * 'fName' - The name of the fleet.
--
-- * 'fInstanceType' - The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
--
-- * 'fComputeCapacityStatus' - The capacity status for the fleet.
--
-- * 'fState' - The current state for the fleet.
fleet ::
  -- | 'fARN'
  Text ->
  -- | 'fName'
  Text ->
  -- | 'fInstanceType'
  Text ->
  -- | 'fComputeCapacityStatus'
  ComputeCapacityStatus ->
  -- | 'fState'
  FleetState ->
  Fleet
fleet pARN_ pName_ pInstanceType_ pComputeCapacityStatus_ pState_ =
  Fleet'
    { _fDomainJoinInfo = Nothing,
      _fIAMRoleARN = Nothing,
      _fDisconnectTimeoutInSeconds = Nothing,
      _fMaxUserDurationInSeconds = Nothing,
      _fCreatedTime = Nothing,
      _fIdleDisconnectTimeoutInSeconds = Nothing,
      _fFleetType = Nothing,
      _fVPCConfig = Nothing,
      _fImageARN = Nothing,
      _fFleetErrors = Nothing,
      _fDisplayName = Nothing,
      _fEnableDefaultInternetAccess = Nothing,
      _fImageName = Nothing,
      _fDescription = Nothing,
      _fStreamView = Nothing,
      _fARN = pARN_,
      _fName = pName_,
      _fInstanceType = pInstanceType_,
      _fComputeCapacityStatus = pComputeCapacityStatus_,
      _fState = pState_
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
fDomainJoinInfo :: Lens' Fleet (Maybe DomainJoinInfo)
fDomainJoinInfo = lens _fDomainJoinInfo (\s a -> s {_fDomainJoinInfo = a})

-- | The ARN of the IAM role that is applied to the fleet. To assume a role, the fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
fIAMRoleARN :: Lens' Fleet (Maybe Text)
fIAMRoleARN = lens _fIAMRoleARN (\s a -> s {_fIAMRoleARN = a})

-- | The amount of time that a streaming session remains active after users disconnect. If they try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance. Specify a value between 60 and 360000.
fDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fDisconnectTimeoutInSeconds = lens _fDisconnectTimeoutInSeconds (\s a -> s {_fDisconnectTimeoutInSeconds = a})

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.  Specify a value between 600 and 360000.
fMaxUserDurationInSeconds :: Lens' Fleet (Maybe Int)
fMaxUserDurationInSeconds = lens _fMaxUserDurationInSeconds (\s a -> s {_fMaxUserDurationInSeconds = a})

-- | The time the fleet was created.
fCreatedTime :: Lens' Fleet (Maybe UTCTime)
fCreatedTime = lens _fCreatedTime (\s a -> s {_fCreatedTime = a}) . mapping _Time

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected. To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
fIdleDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fIdleDisconnectTimeoutInSeconds = lens _fIdleDisconnectTimeoutInSeconds (\s a -> s {_fIdleDisconnectTimeoutInSeconds = a})

-- | The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
fFleetType :: Lens' Fleet (Maybe FleetType)
fFleetType = lens _fFleetType (\s a -> s {_fFleetType = a})

-- | The VPC configuration for the fleet.
fVPCConfig :: Lens' Fleet (Maybe VPCConfig)
fVPCConfig = lens _fVPCConfig (\s a -> s {_fVPCConfig = a})

-- | The ARN for the public, private, or shared image.
fImageARN :: Lens' Fleet (Maybe Text)
fImageARN = lens _fImageARN (\s a -> s {_fImageARN = a})

-- | The fleet errors.
fFleetErrors :: Lens' Fleet [FleetError]
fFleetErrors = lens _fFleetErrors (\s a -> s {_fFleetErrors = a}) . _Default . _Coerce

-- | The fleet name to display.
fDisplayName :: Lens' Fleet (Maybe Text)
fDisplayName = lens _fDisplayName (\s a -> s {_fDisplayName = a})

-- | Indicates whether default internet access is enabled for the fleet.
fEnableDefaultInternetAccess :: Lens' Fleet (Maybe Bool)
fEnableDefaultInternetAccess = lens _fEnableDefaultInternetAccess (\s a -> s {_fEnableDefaultInternetAccess = a})

-- | The name of the image used to create the fleet.
fImageName :: Lens' Fleet (Maybe Text)
fImageName = lens _fImageName (\s a -> s {_fImageName = a})

-- | The description to display.
fDescription :: Lens' Fleet (Maybe Text)
fDescription = lens _fDescription (\s a -> s {_fDescription = a})

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
fStreamView :: Lens' Fleet (Maybe StreamView)
fStreamView = lens _fStreamView (\s a -> s {_fStreamView = a})

-- | The Amazon Resource Name (ARN) for the fleet.
fARN :: Lens' Fleet Text
fARN = lens _fARN (\s a -> s {_fARN = a})

-- | The name of the fleet.
fName :: Lens' Fleet Text
fName = lens _fName (\s a -> s {_fName = a})

-- | The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
fInstanceType :: Lens' Fleet Text
fInstanceType = lens _fInstanceType (\s a -> s {_fInstanceType = a})

-- | The capacity status for the fleet.
fComputeCapacityStatus :: Lens' Fleet ComputeCapacityStatus
fComputeCapacityStatus = lens _fComputeCapacityStatus (\s a -> s {_fComputeCapacityStatus = a})

-- | The current state for the fleet.
fState :: Lens' Fleet FleetState
fState = lens _fState (\s a -> s {_fState = a})

instance FromJSON Fleet where
  parseJSON =
    withObject
      "Fleet"
      ( \x ->
          Fleet'
            <$> (x .:? "DomainJoinInfo")
            <*> (x .:? "IamRoleArn")
            <*> (x .:? "DisconnectTimeoutInSeconds")
            <*> (x .:? "MaxUserDurationInSeconds")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "IdleDisconnectTimeoutInSeconds")
            <*> (x .:? "FleetType")
            <*> (x .:? "VpcConfig")
            <*> (x .:? "ImageArn")
            <*> (x .:? "FleetErrors" .!= mempty)
            <*> (x .:? "DisplayName")
            <*> (x .:? "EnableDefaultInternetAccess")
            <*> (x .:? "ImageName")
            <*> (x .:? "Description")
            <*> (x .:? "StreamView")
            <*> (x .: "Arn")
            <*> (x .: "Name")
            <*> (x .: "InstanceType")
            <*> (x .: "ComputeCapacityStatus")
            <*> (x .: "State")
      )

instance Hashable Fleet

instance NFData Fleet
