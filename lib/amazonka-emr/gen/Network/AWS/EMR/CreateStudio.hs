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
-- Module      : Network.AWS.EMR.CreateStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon EMR Studio.
module Network.AWS.EMR.CreateStudio
  ( -- * Creating a Request
    createStudio,
    CreateStudio,

    -- * Request Lenses
    csDefaultS3Location,
    csDescription,
    csTags,
    csName,
    csAuthMode,
    csVPCId,
    csSubnetIds,
    csServiceRole,
    csUserRole,
    csWorkspaceSecurityGroupId,
    csEngineSecurityGroupId,

    -- * Destructuring the Response
    createStudioResponse,
    CreateStudioResponse,

    -- * Response Lenses
    crsStudioId,
    crsURL,
    crsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStudio' smart constructor.
data CreateStudio = CreateStudio'
  { _csDefaultS3Location ::
      !(Maybe Text),
    _csDescription :: !(Maybe Text),
    _csTags :: !(Maybe [Tag]),
    _csName :: !Text,
    _csAuthMode :: !AuthMode,
    _csVPCId :: !Text,
    _csSubnetIds :: ![Text],
    _csServiceRole :: !Text,
    _csUserRole :: !Text,
    _csWorkspaceSecurityGroupId :: !Text,
    _csEngineSecurityGroupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStudio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csDefaultS3Location' - The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
--
-- * 'csDescription' - A detailed description of the Studio.
--
-- * 'csTags' - A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- * 'csName' - A descriptive name for the Amazon EMR Studio.
--
-- * 'csAuthMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
--
-- * 'csVPCId' - The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
--
-- * 'csSubnetIds' - A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
--
-- * 'csServiceRole' - The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
--
-- * 'csUserRole' - The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
--
-- * 'csWorkspaceSecurityGroupId' - The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
--
-- * 'csEngineSecurityGroupId' - The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
createStudio ::
  -- | 'csName'
  Text ->
  -- | 'csAuthMode'
  AuthMode ->
  -- | 'csVPCId'
  Text ->
  -- | 'csServiceRole'
  Text ->
  -- | 'csUserRole'
  Text ->
  -- | 'csWorkspaceSecurityGroupId'
  Text ->
  -- | 'csEngineSecurityGroupId'
  Text ->
  CreateStudio
createStudio
  pName_
  pAuthMode_
  pVPCId_
  pServiceRole_
  pUserRole_
  pWorkspaceSecurityGroupId_
  pEngineSecurityGroupId_ =
    CreateStudio'
      { _csDefaultS3Location = Nothing,
        _csDescription = Nothing,
        _csTags = Nothing,
        _csName = pName_,
        _csAuthMode = pAuthMode_,
        _csVPCId = pVPCId_,
        _csSubnetIds = mempty,
        _csServiceRole = pServiceRole_,
        _csUserRole = pUserRole_,
        _csWorkspaceSecurityGroupId = pWorkspaceSecurityGroupId_,
        _csEngineSecurityGroupId = pEngineSecurityGroupId_
      }

-- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
csDefaultS3Location :: Lens' CreateStudio (Maybe Text)
csDefaultS3Location = lens _csDefaultS3Location (\s a -> s {_csDefaultS3Location = a})

-- | A detailed description of the Studio.
csDescription :: Lens' CreateStudio (Maybe Text)
csDescription = lens _csDescription (\s a -> s {_csDescription = a})

-- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
csTags :: Lens' CreateStudio [Tag]
csTags = lens _csTags (\s a -> s {_csTags = a}) . _Default . _Coerce

-- | A descriptive name for the Amazon EMR Studio.
csName :: Lens' CreateStudio Text
csName = lens _csName (\s a -> s {_csName = a})

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
csAuthMode :: Lens' CreateStudio AuthMode
csAuthMode = lens _csAuthMode (\s a -> s {_csAuthMode = a})

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
csVPCId :: Lens' CreateStudio Text
csVPCId = lens _csVPCId (\s a -> s {_csVPCId = a})

-- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
csSubnetIds :: Lens' CreateStudio [Text]
csSubnetIds = lens _csSubnetIds (\s a -> s {_csSubnetIds = a}) . _Coerce

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
csServiceRole :: Lens' CreateStudio Text
csServiceRole = lens _csServiceRole (\s a -> s {_csServiceRole = a})

-- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
csUserRole :: Lens' CreateStudio Text
csUserRole = lens _csUserRole (\s a -> s {_csUserRole = a})

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
csWorkspaceSecurityGroupId :: Lens' CreateStudio Text
csWorkspaceSecurityGroupId = lens _csWorkspaceSecurityGroupId (\s a -> s {_csWorkspaceSecurityGroupId = a})

-- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
csEngineSecurityGroupId :: Lens' CreateStudio Text
csEngineSecurityGroupId = lens _csEngineSecurityGroupId (\s a -> s {_csEngineSecurityGroupId = a})

instance AWSRequest CreateStudio where
  type Rs CreateStudio = CreateStudioResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            <$> (x .?> "StudioId") <*> (x .?> "Url") <*> (pure (fromEnum s))
      )

instance Hashable CreateStudio

instance NFData CreateStudio

instance ToHeaders CreateStudio where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ElasticMapReduce.CreateStudio" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    object
      ( catMaybes
          [ ("DefaultS3Location" .=) <$> _csDefaultS3Location,
            ("Description" .=) <$> _csDescription,
            ("Tags" .=) <$> _csTags,
            Just ("Name" .= _csName),
            Just ("AuthMode" .= _csAuthMode),
            Just ("VpcId" .= _csVPCId),
            Just ("SubnetIds" .= _csSubnetIds),
            Just ("ServiceRole" .= _csServiceRole),
            Just ("UserRole" .= _csUserRole),
            Just ("WorkspaceSecurityGroupId" .= _csWorkspaceSecurityGroupId),
            Just ("EngineSecurityGroupId" .= _csEngineSecurityGroupId)
          ]
      )

instance ToPath CreateStudio where
  toPath = const "/"

instance ToQuery CreateStudio where
  toQuery = const mempty

-- | /See:/ 'createStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { _crsStudioId ::
      !(Maybe Text),
    _crsURL :: !(Maybe Text),
    _crsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStudioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'crsURL' - The unique Studio access URL.
--
-- * 'crsResponseStatus' - -- | The response status code.
createStudioResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CreateStudioResponse
createStudioResponse pResponseStatus_ =
  CreateStudioResponse'
    { _crsStudioId = Nothing,
      _crsURL = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | The ID of the Amazon EMR Studio.
crsStudioId :: Lens' CreateStudioResponse (Maybe Text)
crsStudioId = lens _crsStudioId (\s a -> s {_crsStudioId = a})

-- | The unique Studio access URL.
crsURL :: Lens' CreateStudioResponse (Maybe Text)
crsURL = lens _crsURL (\s a -> s {_crsURL = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateStudioResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CreateStudioResponse
