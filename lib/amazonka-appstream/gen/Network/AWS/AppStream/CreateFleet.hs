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
-- Module      : Network.AWS.AppStream.CreateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet. A fleet consists of streaming instances that run a specified image.
module Network.AWS.AppStream.CreateFleet
  ( -- * Creating a Request
    createFleet,
    CreateFleet,

    -- * Request Lenses
    cfDomainJoinInfo,
    cfIAMRoleARN,
    cfDisconnectTimeoutInSeconds,
    cfMaxUserDurationInSeconds,
    cfIdleDisconnectTimeoutInSeconds,
    cfFleetType,
    cfVPCConfig,
    cfImageARN,
    cfDisplayName,
    cfEnableDefaultInternetAccess,
    cfImageName,
    cfDescription,
    cfStreamView,
    cfTags,
    cfName,
    cfInstanceType,
    cfComputeCapacity,

    -- * Destructuring the Response
    createFleetResponse,
    CreateFleetResponse,

    -- * Response Lenses
    cfrsFleet,
    cfrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
  { _cfDomainJoinInfo ::
      !(Maybe DomainJoinInfo),
    _cfIAMRoleARN :: !(Maybe Text),
    _cfDisconnectTimeoutInSeconds :: !(Maybe Int),
    _cfMaxUserDurationInSeconds :: !(Maybe Int),
    _cfIdleDisconnectTimeoutInSeconds :: !(Maybe Int),
    _cfFleetType :: !(Maybe FleetType),
    _cfVPCConfig :: !(Maybe VPCConfig),
    _cfImageARN :: !(Maybe Text),
    _cfDisplayName :: !(Maybe Text),
    _cfEnableDefaultInternetAccess :: !(Maybe Bool),
    _cfImageName :: !(Maybe Text),
    _cfDescription :: !(Maybe Text),
    _cfStreamView :: !(Maybe StreamView),
    _cfTags :: !(Maybe (Map Text (Text))),
    _cfName :: !Text,
    _cfInstanceType :: !Text,
    _cfComputeCapacity :: !ComputeCapacity
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- * 'cfIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'cfDisconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.  Specify a value between 60 and 360000.
--
-- * 'cfMaxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance. Specify a value between 600 and 360000.
--
-- * 'cfIdleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected. To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- * 'cfFleetType' - The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
-- * 'cfVPCConfig' - The VPC configuration for the fleet.
--
-- * 'cfImageARN' - The ARN of the public, private, or shared image to use.
--
-- * 'cfDisplayName' - The fleet name to display.
--
-- * 'cfEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- * 'cfImageName' - The name of the image used to create the fleet.
--
-- * 'cfDescription' - The description to display.
--
-- * 'cfStreamView' - The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
--
-- * 'cfTags' - The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.  If you do not specify a value, the value is set to an empty string. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:  _ . : / = + \ - @ For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'cfName' - A unique name for the fleet.
--
-- * 'cfInstanceType' - The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
--
-- * 'cfComputeCapacity' - The desired capacity for the fleet.
createFleet ::
  -- | 'cfName'
  Text ->
  -- | 'cfInstanceType'
  Text ->
  -- | 'cfComputeCapacity'
  ComputeCapacity ->
  CreateFleet
createFleet pName_ pInstanceType_ pComputeCapacity_ =
  CreateFleet'
    { _cfDomainJoinInfo = Nothing,
      _cfIAMRoleARN = Nothing,
      _cfDisconnectTimeoutInSeconds = Nothing,
      _cfMaxUserDurationInSeconds = Nothing,
      _cfIdleDisconnectTimeoutInSeconds = Nothing,
      _cfFleetType = Nothing,
      _cfVPCConfig = Nothing,
      _cfImageARN = Nothing,
      _cfDisplayName = Nothing,
      _cfEnableDefaultInternetAccess = Nothing,
      _cfImageName = Nothing,
      _cfDescription = Nothing,
      _cfStreamView = Nothing,
      _cfTags = Nothing,
      _cfName = pName_,
      _cfInstanceType = pInstanceType_,
      _cfComputeCapacity = pComputeCapacity_
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
cfDomainJoinInfo :: Lens' CreateFleet (Maybe DomainJoinInfo)
cfDomainJoinInfo = lens _cfDomainJoinInfo (\s a -> s {_cfDomainJoinInfo = a})

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
cfIAMRoleARN :: Lens' CreateFleet (Maybe Text)
cfIAMRoleARN = lens _cfIAMRoleARN (\s a -> s {_cfIAMRoleARN = a})

-- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.  Specify a value between 60 and 360000.
cfDisconnectTimeoutInSeconds :: Lens' CreateFleet (Maybe Int)
cfDisconnectTimeoutInSeconds = lens _cfDisconnectTimeoutInSeconds (\s a -> s {_cfDisconnectTimeoutInSeconds = a})

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance. Specify a value between 600 and 360000.
cfMaxUserDurationInSeconds :: Lens' CreateFleet (Maybe Int)
cfMaxUserDurationInSeconds = lens _cfMaxUserDurationInSeconds (\s a -> s {_cfMaxUserDurationInSeconds = a})

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected. To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
cfIdleDisconnectTimeoutInSeconds :: Lens' CreateFleet (Maybe Int)
cfIdleDisconnectTimeoutInSeconds = lens _cfIdleDisconnectTimeoutInSeconds (\s a -> s {_cfIdleDisconnectTimeoutInSeconds = a})

-- | The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
cfFleetType :: Lens' CreateFleet (Maybe FleetType)
cfFleetType = lens _cfFleetType (\s a -> s {_cfFleetType = a})

-- | The VPC configuration for the fleet.
cfVPCConfig :: Lens' CreateFleet (Maybe VPCConfig)
cfVPCConfig = lens _cfVPCConfig (\s a -> s {_cfVPCConfig = a})

-- | The ARN of the public, private, or shared image to use.
cfImageARN :: Lens' CreateFleet (Maybe Text)
cfImageARN = lens _cfImageARN (\s a -> s {_cfImageARN = a})

-- | The fleet name to display.
cfDisplayName :: Lens' CreateFleet (Maybe Text)
cfDisplayName = lens _cfDisplayName (\s a -> s {_cfDisplayName = a})

-- | Enables or disables default internet access for the fleet.
cfEnableDefaultInternetAccess :: Lens' CreateFleet (Maybe Bool)
cfEnableDefaultInternetAccess = lens _cfEnableDefaultInternetAccess (\s a -> s {_cfEnableDefaultInternetAccess = a})

-- | The name of the image used to create the fleet.
cfImageName :: Lens' CreateFleet (Maybe Text)
cfImageName = lens _cfImageName (\s a -> s {_cfImageName = a})

-- | The description to display.
cfDescription :: Lens' CreateFleet (Maybe Text)
cfDescription = lens _cfDescription (\s a -> s {_cfDescription = a})

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays. The default value is @APP@ .
cfStreamView :: Lens' CreateFleet (Maybe StreamView)
cfStreamView = lens _cfStreamView (\s a -> s {_cfStreamView = a})

-- | The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.  If you do not specify a value, the value is set to an empty string. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:  _ . : / = + \ - @ For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
cfTags :: Lens' CreateFleet (HashMap Text (Text))
cfTags = lens _cfTags (\s a -> s {_cfTags = a}) . _Default . _Map

-- | A unique name for the fleet.
cfName :: Lens' CreateFleet Text
cfName = lens _cfName (\s a -> s {_cfName = a})

-- | The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
cfInstanceType :: Lens' CreateFleet Text
cfInstanceType = lens _cfInstanceType (\s a -> s {_cfInstanceType = a})

-- | The desired capacity for the fleet.
cfComputeCapacity :: Lens' CreateFleet ComputeCapacity
cfComputeCapacity = lens _cfComputeCapacity (\s a -> s {_cfComputeCapacity = a})

instance AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request = postJSON appStream
  response =
    receiveJSON
      ( \s h x ->
          CreateFleetResponse' <$> (x .?> "Fleet") <*> (pure (fromEnum s))
      )

instance Hashable CreateFleet

instance NFData CreateFleet

instance ToHeaders CreateFleet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.CreateFleet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    object
      ( catMaybes
          [ ("DomainJoinInfo" .=) <$> _cfDomainJoinInfo,
            ("IamRoleArn" .=) <$> _cfIAMRoleARN,
            ("DisconnectTimeoutInSeconds" .=)
              <$> _cfDisconnectTimeoutInSeconds,
            ("MaxUserDurationInSeconds" .=) <$> _cfMaxUserDurationInSeconds,
            ("IdleDisconnectTimeoutInSeconds" .=)
              <$> _cfIdleDisconnectTimeoutInSeconds,
            ("FleetType" .=) <$> _cfFleetType,
            ("VpcConfig" .=) <$> _cfVPCConfig,
            ("ImageArn" .=) <$> _cfImageARN,
            ("DisplayName" .=) <$> _cfDisplayName,
            ("EnableDefaultInternetAccess" .=)
              <$> _cfEnableDefaultInternetAccess,
            ("ImageName" .=) <$> _cfImageName,
            ("Description" .=) <$> _cfDescription,
            ("StreamView" .=) <$> _cfStreamView,
            ("Tags" .=) <$> _cfTags,
            Just ("Name" .= _cfName),
            Just ("InstanceType" .= _cfInstanceType),
            Just ("ComputeCapacity" .= _cfComputeCapacity)
          ]
      )

instance ToPath CreateFleet where
  toPath = const "/"

instance ToQuery CreateFleet where
  toQuery = const mempty

-- | /See:/ 'createFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { _cfrsFleet ::
      !(Maybe Fleet),
    _cfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleet' - Information about the fleet.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse ::
  -- | 'cfrsResponseStatus'
  Int ->
  CreateFleetResponse
createFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    { _cfrsFleet = Nothing,
      _cfrsResponseStatus = pResponseStatus_
    }

-- | Information about the fleet.
cfrsFleet :: Lens' CreateFleetResponse (Maybe Fleet)
cfrsFleet = lens _cfrsFleet (\s a -> s {_cfrsFleet = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\s a -> s {_cfrsResponseStatus = a})

instance NFData CreateFleetResponse
