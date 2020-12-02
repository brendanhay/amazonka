{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceRequest where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.WorkspaceProperties

-- | Describes the information used to create a WorkSpace.
--
--
--
-- /See:/ 'workspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { _wrWorkspaceProperties ::
      !(Maybe WorkspaceProperties),
    _wrRootVolumeEncryptionEnabled :: !(Maybe Bool),
    _wrVolumeEncryptionKey :: !(Maybe Text),
    _wrUserVolumeEncryptionEnabled :: !(Maybe Bool),
    _wrTags :: !(Maybe [Tag]),
    _wrDirectoryId :: !Text,
    _wrUserName :: !Text,
    _wrBundleId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrWorkspaceProperties' - The WorkSpace properties.
--
-- * 'wrRootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- * 'wrVolumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- * 'wrUserVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- * 'wrTags' - The tags for the WorkSpace.
--
-- * 'wrDirectoryId' - The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
--
-- * 'wrUserName' - The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
--
-- * 'wrBundleId' - The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
workspaceRequest ::
  -- | 'wrDirectoryId'
  Text ->
  -- | 'wrUserName'
  Text ->
  -- | 'wrBundleId'
  Text ->
  WorkspaceRequest
workspaceRequest pDirectoryId_ pUserName_ pBundleId_ =
  WorkspaceRequest'
    { _wrWorkspaceProperties = Nothing,
      _wrRootVolumeEncryptionEnabled = Nothing,
      _wrVolumeEncryptionKey = Nothing,
      _wrUserVolumeEncryptionEnabled = Nothing,
      _wrTags = Nothing,
      _wrDirectoryId = pDirectoryId_,
      _wrUserName = pUserName_,
      _wrBundleId = pBundleId_
    }

-- | The WorkSpace properties.
wrWorkspaceProperties :: Lens' WorkspaceRequest (Maybe WorkspaceProperties)
wrWorkspaceProperties = lens _wrWorkspaceProperties (\s a -> s {_wrWorkspaceProperties = a})

-- | Indicates whether the data stored on the root volume is encrypted.
wrRootVolumeEncryptionEnabled :: Lens' WorkspaceRequest (Maybe Bool)
wrRootVolumeEncryptionEnabled = lens _wrRootVolumeEncryptionEnabled (\s a -> s {_wrRootVolumeEncryptionEnabled = a})

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
wrVolumeEncryptionKey :: Lens' WorkspaceRequest (Maybe Text)
wrVolumeEncryptionKey = lens _wrVolumeEncryptionKey (\s a -> s {_wrVolumeEncryptionKey = a})

-- | Indicates whether the data stored on the user volume is encrypted.
wrUserVolumeEncryptionEnabled :: Lens' WorkspaceRequest (Maybe Bool)
wrUserVolumeEncryptionEnabled = lens _wrUserVolumeEncryptionEnabled (\s a -> s {_wrUserVolumeEncryptionEnabled = a})

-- | The tags for the WorkSpace.
wrTags :: Lens' WorkspaceRequest [Tag]
wrTags = lens _wrTags (\s a -> s {_wrTags = a}) . _Default . _Coerce

-- | The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
wrDirectoryId :: Lens' WorkspaceRequest Text
wrDirectoryId = lens _wrDirectoryId (\s a -> s {_wrDirectoryId = a})

-- | The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
wrUserName :: Lens' WorkspaceRequest Text
wrUserName = lens _wrUserName (\s a -> s {_wrUserName = a})

-- | The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
wrBundleId :: Lens' WorkspaceRequest Text
wrBundleId = lens _wrBundleId (\s a -> s {_wrBundleId = a})

instance FromJSON WorkspaceRequest where
  parseJSON =
    withObject
      "WorkspaceRequest"
      ( \x ->
          WorkspaceRequest'
            <$> (x .:? "WorkspaceProperties")
            <*> (x .:? "RootVolumeEncryptionEnabled")
            <*> (x .:? "VolumeEncryptionKey")
            <*> (x .:? "UserVolumeEncryptionEnabled")
            <*> (x .:? "Tags" .!= mempty)
            <*> (x .: "DirectoryId")
            <*> (x .: "UserName")
            <*> (x .: "BundleId")
      )

instance Hashable WorkspaceRequest

instance NFData WorkspaceRequest

instance ToJSON WorkspaceRequest where
  toJSON WorkspaceRequest' {..} =
    object
      ( catMaybes
          [ ("WorkspaceProperties" .=) <$> _wrWorkspaceProperties,
            ("RootVolumeEncryptionEnabled" .=)
              <$> _wrRootVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" .=) <$> _wrVolumeEncryptionKey,
            ("UserVolumeEncryptionEnabled" .=)
              <$> _wrUserVolumeEncryptionEnabled,
            ("Tags" .=) <$> _wrTags,
            Just ("DirectoryId" .= _wrDirectoryId),
            Just ("UserName" .= _wrUserName),
            Just ("BundleId" .= _wrBundleId)
          ]
      )
