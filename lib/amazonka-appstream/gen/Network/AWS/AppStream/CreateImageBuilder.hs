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
-- Module      : Network.AWS.AppStream.CreateImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an image builder. An image builder is a virtual machine that is used to create an image.
--
--
-- The initial state of the builder is @PENDING@ . When it is ready, the state is @RUNNING@ .
module Network.AWS.AppStream.CreateImageBuilder
  ( -- * Creating a Request
    createImageBuilder,
    CreateImageBuilder,

    -- * Request Lenses
    cibDomainJoinInfo,
    cibIAMRoleARN,
    cibAccessEndpoints,
    cibVPCConfig,
    cibImageARN,
    cibDisplayName,
    cibEnableDefaultInternetAccess,
    cibImageName,
    cibDescription,
    cibAppstreamAgentVersion,
    cibTags,
    cibName,
    cibInstanceType,

    -- * Destructuring the Response
    createImageBuilderResponse,
    CreateImageBuilderResponse,

    -- * Response Lenses
    cibrsImageBuilder,
    cibrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { _cibDomainJoinInfo ::
      !(Maybe DomainJoinInfo),
    _cibIAMRoleARN :: !(Maybe Text),
    _cibAccessEndpoints ::
      !(Maybe (List1 AccessEndpoint)),
    _cibVPCConfig :: !(Maybe VPCConfig),
    _cibImageARN :: !(Maybe Text),
    _cibDisplayName :: !(Maybe Text),
    _cibEnableDefaultInternetAccess :: !(Maybe Bool),
    _cibImageName :: !(Maybe Text),
    _cibDescription :: !(Maybe Text),
    _cibAppstreamAgentVersion :: !(Maybe Text),
    _cibTags :: !(Maybe (Map Text (Text))),
    _cibName :: !Text,
    _cibInstanceType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
--
-- * 'cibIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'cibAccessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- * 'cibVPCConfig' - The VPC configuration for the image builder. You can specify only one subnet.
--
-- * 'cibImageARN' - The ARN of the public, private, or shared image to use.
--
-- * 'cibDisplayName' - The image builder name to display.
--
-- * 'cibEnableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- * 'cibImageName' - The name of the image used to create the image builder.
--
-- * 'cibDescription' - The description to display.
--
-- * 'cibAppstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- * 'cibTags' - The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.  Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:  _ . : / = + \ - @ If you do not specify a value, the value is set to an empty string. For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- * 'cibName' - A unique name for the image builder.
--
-- * 'cibInstanceType' - The instance type to use when launching the image builder. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
createImageBuilder ::
  -- | 'cibName'
  Text ->
  -- | 'cibInstanceType'
  Text ->
  CreateImageBuilder
createImageBuilder pName_ pInstanceType_ =
  CreateImageBuilder'
    { _cibDomainJoinInfo = Nothing,
      _cibIAMRoleARN = Nothing,
      _cibAccessEndpoints = Nothing,
      _cibVPCConfig = Nothing,
      _cibImageARN = Nothing,
      _cibDisplayName = Nothing,
      _cibEnableDefaultInternetAccess = Nothing,
      _cibImageName = Nothing,
      _cibDescription = Nothing,
      _cibAppstreamAgentVersion = Nothing,
      _cibTags = Nothing,
      _cibName = pName_,
      _cibInstanceType = pInstanceType_
    }

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
cibDomainJoinInfo :: Lens' CreateImageBuilder (Maybe DomainJoinInfo)
cibDomainJoinInfo = lens _cibDomainJoinInfo (\s a -> s {_cibDomainJoinInfo = a})

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
cibIAMRoleARN :: Lens' CreateImageBuilder (Maybe Text)
cibIAMRoleARN = lens _cibIAMRoleARN (\s a -> s {_cibIAMRoleARN = a})

-- | The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
cibAccessEndpoints :: Lens' CreateImageBuilder (Maybe (NonEmpty AccessEndpoint))
cibAccessEndpoints = lens _cibAccessEndpoints (\s a -> s {_cibAccessEndpoints = a}) . mapping _List1

-- | The VPC configuration for the image builder. You can specify only one subnet.
cibVPCConfig :: Lens' CreateImageBuilder (Maybe VPCConfig)
cibVPCConfig = lens _cibVPCConfig (\s a -> s {_cibVPCConfig = a})

-- | The ARN of the public, private, or shared image to use.
cibImageARN :: Lens' CreateImageBuilder (Maybe Text)
cibImageARN = lens _cibImageARN (\s a -> s {_cibImageARN = a})

-- | The image builder name to display.
cibDisplayName :: Lens' CreateImageBuilder (Maybe Text)
cibDisplayName = lens _cibDisplayName (\s a -> s {_cibDisplayName = a})

-- | Enables or disables default internet access for the image builder.
cibEnableDefaultInternetAccess :: Lens' CreateImageBuilder (Maybe Bool)
cibEnableDefaultInternetAccess = lens _cibEnableDefaultInternetAccess (\s a -> s {_cibEnableDefaultInternetAccess = a})

-- | The name of the image used to create the image builder.
cibImageName :: Lens' CreateImageBuilder (Maybe Text)
cibImageName = lens _cibImageName (\s a -> s {_cibImageName = a})

-- | The description to display.
cibDescription :: Lens' CreateImageBuilder (Maybe Text)
cibDescription = lens _cibDescription (\s a -> s {_cibDescription = a})

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
cibAppstreamAgentVersion :: Lens' CreateImageBuilder (Maybe Text)
cibAppstreamAgentVersion = lens _cibAppstreamAgentVersion (\s a -> s {_cibAppstreamAgentVersion = a})

-- | The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.  Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:  _ . : / = + \ - @ If you do not specify a value, the value is set to an empty string. For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
cibTags :: Lens' CreateImageBuilder (HashMap Text (Text))
cibTags = lens _cibTags (\s a -> s {_cibTags = a}) . _Default . _Map

-- | A unique name for the image builder.
cibName :: Lens' CreateImageBuilder Text
cibName = lens _cibName (\s a -> s {_cibName = a})

-- | The instance type to use when launching the image builder. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.memory.z1d.large     * stream.memory.z1d.xlarge     * stream.memory.z1d.2xlarge     * stream.memory.z1d.3xlarge     * stream.memory.z1d.6xlarge     * stream.memory.z1d.12xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics.g4dn.xlarge     * stream.graphics.g4dn.2xlarge     * stream.graphics.g4dn.4xlarge     * stream.graphics.g4dn.8xlarge     * stream.graphics.g4dn.12xlarge     * stream.graphics.g4dn.16xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
cibInstanceType :: Lens' CreateImageBuilder Text
cibInstanceType = lens _cibInstanceType (\s a -> s {_cibInstanceType = a})

instance AWSRequest CreateImageBuilder where
  type Rs CreateImageBuilder = CreateImageBuilderResponse
  request = postJSON appStream
  response =
    receiveJSON
      ( \s h x ->
          CreateImageBuilderResponse'
            <$> (x .?> "ImageBuilder") <*> (pure (fromEnum s))
      )

instance Hashable CreateImageBuilder

instance NFData CreateImageBuilder

instance ToHeaders CreateImageBuilder where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.CreateImageBuilder" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateImageBuilder where
  toJSON CreateImageBuilder' {..} =
    object
      ( catMaybes
          [ ("DomainJoinInfo" .=) <$> _cibDomainJoinInfo,
            ("IamRoleArn" .=) <$> _cibIAMRoleARN,
            ("AccessEndpoints" .=) <$> _cibAccessEndpoints,
            ("VpcConfig" .=) <$> _cibVPCConfig,
            ("ImageArn" .=) <$> _cibImageARN,
            ("DisplayName" .=) <$> _cibDisplayName,
            ("EnableDefaultInternetAccess" .=)
              <$> _cibEnableDefaultInternetAccess,
            ("ImageName" .=) <$> _cibImageName,
            ("Description" .=) <$> _cibDescription,
            ("AppstreamAgentVersion" .=) <$> _cibAppstreamAgentVersion,
            ("Tags" .=) <$> _cibTags,
            Just ("Name" .= _cibName),
            Just ("InstanceType" .= _cibInstanceType)
          ]
      )

instance ToPath CreateImageBuilder where
  toPath = const "/"

instance ToQuery CreateImageBuilder where
  toQuery = const mempty

-- | /See:/ 'createImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { _cibrsImageBuilder ::
      !(Maybe ImageBuilder),
    _cibrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImageBuilderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibrsImageBuilder' - Information about the image builder.
--
-- * 'cibrsResponseStatus' - -- | The response status code.
createImageBuilderResponse ::
  -- | 'cibrsResponseStatus'
  Int ->
  CreateImageBuilderResponse
createImageBuilderResponse pResponseStatus_ =
  CreateImageBuilderResponse'
    { _cibrsImageBuilder = Nothing,
      _cibrsResponseStatus = pResponseStatus_
    }

-- | Information about the image builder.
cibrsImageBuilder :: Lens' CreateImageBuilderResponse (Maybe ImageBuilder)
cibrsImageBuilder = lens _cibrsImageBuilder (\s a -> s {_cibrsImageBuilder = a})

-- | -- | The response status code.
cibrsResponseStatus :: Lens' CreateImageBuilderResponse Int
cibrsResponseStatus = lens _cibrsResponseStatus (\s a -> s {_cibrsResponseStatus = a})

instance NFData CreateImageBuilderResponse
