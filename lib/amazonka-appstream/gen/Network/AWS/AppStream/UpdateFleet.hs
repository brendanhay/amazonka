{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fleet.
--
--
-- If the fleet is in the @STOPPED@ state, you can update any attribute except the fleet name. If the fleet is in the @RUNNING@ state, you can update the @DisplayName@ , @ComputeCapacity@ , @ImageARN@ , @ImageName@ , @IdleDisconnectTimeoutInSeconds@ , and @DisconnectTimeoutInSeconds@ attributes. If the fleet is in the @STARTING@ or @STOPPING@ state, you can't update it.
module Network.AWS.AppStream.UpdateFleet
  ( -- * Creating a Request
    updateFleet,
    UpdateFleet,

    -- * Request Lenses
    ufDomainJoinInfo,
    ufIAMRoleARN,
    ufDisconnectTimeoutInSeconds,
    ufMaxUserDurationInSeconds,
    ufAttributesToDelete,
    ufIdleDisconnectTimeoutInSeconds,
    ufDeleteVPCConfig,
    ufInstanceType,
    ufVPCConfig,
    ufName,
    ufImageARN,
    ufDisplayName,
    ufEnableDefaultInternetAccess,
    ufImageName,
    ufDescription,
    ufStreamView,
    ufComputeCapacity,

    -- * Destructuring the Response
    updateFleetResponse,
    UpdateFleetResponse,

    -- * Response Lenses
    ufrsFleet,
    ufrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFleet' smart constructor.
data UpdateFleet = UpdateFleet'
  { _ufDomainJoinInfo ::
      !(Maybe DomainJoinInfo),
    _ufIAMRoleARN :: !(Maybe Text),
    _ufDisconnectTimeoutInSeconds :: !(Maybe Int),
    _ufMaxUserDurationInSeconds :: !(Maybe Int),
    _ufAttributesToDelete :: !(Maybe [FleetAttribute]),
    _ufIdleDisconnectTimeoutInSeconds :: !(Maybe Int),
    _ufDeleteVPCConfig :: !(Maybe Bool),
    _ufInstanceType :: !(Maybe Text),
    _ufVPCConfig :: !(Maybe VPCConfig),
    _ufName :: !(Maybe Text),
    _ufImageARN :: !(Maybe Text),
    _ufDisplayName :: !(Maybe Text),
    _ufEnableDefaultInternetAccess :: !(Maybe Bool),
    _ufImageName :: !(Maybe Text),
    _ufDescription :: !(Maybe Text),
    _ufStreamView :: !(Maybe StreamView),
    _ufComputeCapacity :: !(Maybe ComputeCapacity)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- * 'ufIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'ufDisconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.  Specify a value between 60 and 360000.
--
-- * 'ufMaxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance. Specify a value between 600 and 360000.
--
-- * 'ufAttributesToDelete' - The fleet attributes to delete.
--
-- * 'ufIdleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.  To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- * 'ufDeleteVPCConfig' - Deletes the VPC association for the specified fleet.
--
-- * 'ufInstanceType' - The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
--
-- * 'ufVPCConfig' - The VPC configuration for the fleet.
--
-- * 'ufName' - A unique name for the fleet.
--
-- * 'ufImageARN' - The ARN of the public, private, or shared image to use.
--
-- * 'ufDisplayName' - The fleet name to display.
--
-- * 'ufEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- * 'ufImageName' - The name of the image used to create the fleet.
--
-- * 'ufDescription' - The description to display.
--
-- * 'ufStreamView' - The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
--
-- * 'ufComputeCapacity' - The desired capacity for the fleet.
updateFleet ::
  UpdateFleet
updateFleet =
  UpdateFleet'
    { _ufDomainJoinInfo = Nothing,
      _ufIAMRoleARN = Nothing,
      _ufDisconnectTimeoutInSeconds = Nothing,
      _ufMaxUserDurationInSeconds = Nothing,
      _ufAttributesToDelete = Nothing,
      _ufIdleDisconnectTimeoutInSeconds = Nothing,
      _ufDeleteVPCConfig = Nothing,
      _ufInstanceType = Nothing,
      _ufVPCConfig = Nothing,
      _ufName = Nothing,
      _ufImageARN = Nothing,
      _ufDisplayName = Nothing,
      _ufEnableDefaultInternetAccess = Nothing,
      _ufImageName = Nothing,
      _ufDescription = Nothing,
      _ufStreamView = Nothing,
      _ufComputeCapacity = Nothing
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
ufDomainJoinInfo :: Lens' UpdateFleet (Maybe DomainJoinInfo)
ufDomainJoinInfo = lens _ufDomainJoinInfo (\s a -> s {_ufDomainJoinInfo = a})

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
ufIAMRoleARN :: Lens' UpdateFleet (Maybe Text)
ufIAMRoleARN = lens _ufIAMRoleARN (\s a -> s {_ufIAMRoleARN = a})

-- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.  Specify a value between 60 and 360000.
ufDisconnectTimeoutInSeconds :: Lens' UpdateFleet (Maybe Int)
ufDisconnectTimeoutInSeconds = lens _ufDisconnectTimeoutInSeconds (\s a -> s {_ufDisconnectTimeoutInSeconds = a})

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance. Specify a value between 600 and 360000.
ufMaxUserDurationInSeconds :: Lens' UpdateFleet (Maybe Int)
ufMaxUserDurationInSeconds = lens _ufMaxUserDurationInSeconds (\s a -> s {_ufMaxUserDurationInSeconds = a})

-- | The fleet attributes to delete.
ufAttributesToDelete :: Lens' UpdateFleet [FleetAttribute]
ufAttributesToDelete = lens _ufAttributesToDelete (\s a -> s {_ufAttributesToDelete = a}) . _Default . _Coerce

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.  To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
ufIdleDisconnectTimeoutInSeconds :: Lens' UpdateFleet (Maybe Int)
ufIdleDisconnectTimeoutInSeconds = lens _ufIdleDisconnectTimeoutInSeconds (\s a -> s {_ufIdleDisconnectTimeoutInSeconds = a})

-- | Deletes the VPC association for the specified fleet.
ufDeleteVPCConfig :: Lens' UpdateFleet (Maybe Bool)
ufDeleteVPCConfig = lens _ufDeleteVPCConfig (\s a -> s {_ufDeleteVPCConfig = a})

-- | The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
ufInstanceType :: Lens' UpdateFleet (Maybe Text)
ufInstanceType = lens _ufInstanceType (\s a -> s {_ufInstanceType = a})

-- | The VPC configuration for the fleet.
ufVPCConfig :: Lens' UpdateFleet (Maybe VPCConfig)
ufVPCConfig = lens _ufVPCConfig (\s a -> s {_ufVPCConfig = a})

-- | A unique name for the fleet.
ufName :: Lens' UpdateFleet (Maybe Text)
ufName = lens _ufName (\s a -> s {_ufName = a})

-- | The ARN of the public, private, or shared image to use.
ufImageARN :: Lens' UpdateFleet (Maybe Text)
ufImageARN = lens _ufImageARN (\s a -> s {_ufImageARN = a})

-- | The fleet name to display.
ufDisplayName :: Lens' UpdateFleet (Maybe Text)
ufDisplayName = lens _ufDisplayName (\s a -> s {_ufDisplayName = a})

-- | Enables or disables default internet access for the fleet.
ufEnableDefaultInternetAccess :: Lens' UpdateFleet (Maybe Bool)
ufEnableDefaultInternetAccess = lens _ufEnableDefaultInternetAccess (\s a -> s {_ufEnableDefaultInternetAccess = a})

-- | The name of the image used to create the fleet.
ufImageName :: Lens' UpdateFleet (Maybe Text)
ufImageName = lens _ufImageName (\s a -> s {_ufImageName = a})

-- | The description to display.
ufDescription :: Lens' UpdateFleet (Maybe Text)
ufDescription = lens _ufDescription (\s a -> s {_ufDescription = a})

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
ufStreamView :: Lens' UpdateFleet (Maybe StreamView)
ufStreamView = lens _ufStreamView (\s a -> s {_ufStreamView = a})

-- | The desired capacity for the fleet.
ufComputeCapacity :: Lens' UpdateFleet (Maybe ComputeCapacity)
ufComputeCapacity = lens _ufComputeCapacity (\s a -> s {_ufComputeCapacity = a})

instance AWSRequest UpdateFleet where
  type Rs UpdateFleet = UpdateFleetResponse
  request = postJSON appStream
  response =
    receiveJSON
      ( \s h x ->
          UpdateFleetResponse' <$> (x .?> "Fleet") <*> (pure (fromEnum s))
      )

instance Hashable UpdateFleet

instance NFData UpdateFleet

instance ToHeaders UpdateFleet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.UpdateFleet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateFleet where
  toJSON UpdateFleet' {..} =
    object
      ( catMaybes
          [ ("DomainJoinInfo" .=) <$> _ufDomainJoinInfo,
            ("IamRoleArn" .=) <$> _ufIAMRoleARN,
            ("DisconnectTimeoutInSeconds" .=)
              <$> _ufDisconnectTimeoutInSeconds,
            ("MaxUserDurationInSeconds" .=) <$> _ufMaxUserDurationInSeconds,
            ("AttributesToDelete" .=) <$> _ufAttributesToDelete,
            ("IdleDisconnectTimeoutInSeconds" .=)
              <$> _ufIdleDisconnectTimeoutInSeconds,
            ("DeleteVpcConfig" .=) <$> _ufDeleteVPCConfig,
            ("InstanceType" .=) <$> _ufInstanceType,
            ("VpcConfig" .=) <$> _ufVPCConfig,
            ("Name" .=) <$> _ufName,
            ("ImageArn" .=) <$> _ufImageARN,
            ("DisplayName" .=) <$> _ufDisplayName,
            ("EnableDefaultInternetAccess" .=)
              <$> _ufEnableDefaultInternetAccess,
            ("ImageName" .=) <$> _ufImageName,
            ("Description" .=) <$> _ufDescription,
            ("StreamView" .=) <$> _ufStreamView,
            ("ComputeCapacity" .=) <$> _ufComputeCapacity
          ]
      )

instance ToPath UpdateFleet where
  toPath = const "/"

instance ToQuery UpdateFleet where
  toQuery = const mempty

-- | /See:/ 'updateFleetResponse' smart constructor.
data UpdateFleetResponse = UpdateFleetResponse'
  { _ufrsFleet ::
      !(Maybe Fleet),
    _ufrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsFleet' - Information about the fleet.
--
-- * 'ufrsResponseStatus' - -- | The response status code.
updateFleetResponse ::
  -- | 'ufrsResponseStatus'
  Int ->
  UpdateFleetResponse
updateFleetResponse pResponseStatus_ =
  UpdateFleetResponse'
    { _ufrsFleet = Nothing,
      _ufrsResponseStatus = pResponseStatus_
    }

-- | Information about the fleet.
ufrsFleet :: Lens' UpdateFleetResponse (Maybe Fleet)
ufrsFleet = lens _ufrsFleet (\s a -> s {_ufrsFleet = a})

-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFleetResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\s a -> s {_ufrsResponseStatus = a})

instance NFData UpdateFleetResponse
