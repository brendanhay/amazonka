{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Studio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Studio where

import Network.AWS.EMR.Types.AuthMode
import Network.AWS.EMR.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for an Amazon EMR Studio including ID, creation time, name, and so on.
--
--
--
-- /See:/ 'studio' smart constructor.
data Studio = Studio'
  { _stuCreationTime :: !(Maybe POSIX),
    _stuEngineSecurityGroupId :: !(Maybe Text),
    _stuSubnetIds :: !(Maybe [Text]),
    _stuStudioId :: !(Maybe Text),
    _stuVPCId :: !(Maybe Text),
    _stuURL :: !(Maybe Text),
    _stuAuthMode :: !(Maybe AuthMode),
    _stuDefaultS3Location :: !(Maybe Text),
    _stuWorkspaceSecurityGroupId :: !(Maybe Text),
    _stuName :: !(Maybe Text),
    _stuStudioARN :: !(Maybe Text),
    _stuUserRole :: !(Maybe Text),
    _stuDescription :: !(Maybe Text),
    _stuTags :: !(Maybe [Tag]),
    _stuServiceRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Studio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stuCreationTime' - The time the Amazon EMR Studio was created.
--
-- * 'stuEngineSecurityGroupId' - The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
--
-- * 'stuSubnetIds' - The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- * 'stuStudioId' - The ID of the EMR Studio.
--
-- * 'stuVPCId' - The ID of the VPC associated with the EMR Studio.
--
-- * 'stuURL' - The unique access URL of the Amazon EMR Studio.
--
-- * 'stuAuthMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
--
-- * 'stuDefaultS3Location' - The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
--
-- * 'stuWorkspaceSecurityGroupId' - The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
--
-- * 'stuName' - The name of the EMR Studio.
--
-- * 'stuStudioARN' - The Amazon Resource Name (ARN) of the EMR Studio.
--
-- * 'stuUserRole' - The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
--
-- * 'stuDescription' - The detailed description of the EMR Studio.
--
-- * 'stuTags' - A list of tags associated with the Amazon EMR Studio.
--
-- * 'stuServiceRole' - The name of the IAM role assumed by the Amazon EMR Studio.
studio ::
  Studio
studio =
  Studio'
    { _stuCreationTime = Nothing,
      _stuEngineSecurityGroupId = Nothing,
      _stuSubnetIds = Nothing,
      _stuStudioId = Nothing,
      _stuVPCId = Nothing,
      _stuURL = Nothing,
      _stuAuthMode = Nothing,
      _stuDefaultS3Location = Nothing,
      _stuWorkspaceSecurityGroupId = Nothing,
      _stuName = Nothing,
      _stuStudioARN = Nothing,
      _stuUserRole = Nothing,
      _stuDescription = Nothing,
      _stuTags = Nothing,
      _stuServiceRole = Nothing
    }

-- | The time the Amazon EMR Studio was created.
stuCreationTime :: Lens' Studio (Maybe UTCTime)
stuCreationTime = lens _stuCreationTime (\s a -> s {_stuCreationTime = a}) . mapping _Time

-- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
stuEngineSecurityGroupId :: Lens' Studio (Maybe Text)
stuEngineSecurityGroupId = lens _stuEngineSecurityGroupId (\s a -> s {_stuEngineSecurityGroupId = a})

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
stuSubnetIds :: Lens' Studio [Text]
stuSubnetIds = lens _stuSubnetIds (\s a -> s {_stuSubnetIds = a}) . _Default . _Coerce

-- | The ID of the EMR Studio.
stuStudioId :: Lens' Studio (Maybe Text)
stuStudioId = lens _stuStudioId (\s a -> s {_stuStudioId = a})

-- | The ID of the VPC associated with the EMR Studio.
stuVPCId :: Lens' Studio (Maybe Text)
stuVPCId = lens _stuVPCId (\s a -> s {_stuVPCId = a})

-- | The unique access URL of the Amazon EMR Studio.
stuURL :: Lens' Studio (Maybe Text)
stuURL = lens _stuURL (\s a -> s {_stuURL = a})

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
stuAuthMode :: Lens' Studio (Maybe AuthMode)
stuAuthMode = lens _stuAuthMode (\s a -> s {_stuAuthMode = a})

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
stuDefaultS3Location :: Lens' Studio (Maybe Text)
stuDefaultS3Location = lens _stuDefaultS3Location (\s a -> s {_stuDefaultS3Location = a})

-- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
stuWorkspaceSecurityGroupId :: Lens' Studio (Maybe Text)
stuWorkspaceSecurityGroupId = lens _stuWorkspaceSecurityGroupId (\s a -> s {_stuWorkspaceSecurityGroupId = a})

-- | The name of the EMR Studio.
stuName :: Lens' Studio (Maybe Text)
stuName = lens _stuName (\s a -> s {_stuName = a})

-- | The Amazon Resource Name (ARN) of the EMR Studio.
stuStudioARN :: Lens' Studio (Maybe Text)
stuStudioARN = lens _stuStudioARN (\s a -> s {_stuStudioARN = a})

-- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
stuUserRole :: Lens' Studio (Maybe Text)
stuUserRole = lens _stuUserRole (\s a -> s {_stuUserRole = a})

-- | The detailed description of the EMR Studio.
stuDescription :: Lens' Studio (Maybe Text)
stuDescription = lens _stuDescription (\s a -> s {_stuDescription = a})

-- | A list of tags associated with the Amazon EMR Studio.
stuTags :: Lens' Studio [Tag]
stuTags = lens _stuTags (\s a -> s {_stuTags = a}) . _Default . _Coerce

-- | The name of the IAM role assumed by the Amazon EMR Studio.
stuServiceRole :: Lens' Studio (Maybe Text)
stuServiceRole = lens _stuServiceRole (\s a -> s {_stuServiceRole = a})

instance FromJSON Studio where
  parseJSON =
    withObject
      "Studio"
      ( \x ->
          Studio'
            <$> (x .:? "CreationTime")
            <*> (x .:? "EngineSecurityGroupId")
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "StudioId")
            <*> (x .:? "VpcId")
            <*> (x .:? "Url")
            <*> (x .:? "AuthMode")
            <*> (x .:? "DefaultS3Location")
            <*> (x .:? "WorkspaceSecurityGroupId")
            <*> (x .:? "Name")
            <*> (x .:? "StudioArn")
            <*> (x .:? "UserRole")
            <*> (x .:? "Description")
            <*> (x .:? "Tags" .!= mempty)
            <*> (x .:? "ServiceRole")
      )

instance Hashable Studio

instance NFData Studio
