{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Workspace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Workspace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceState

-- | Describes a WorkSpace.
--
--
--
-- /See:/ 'workspace' smart constructor.
data Workspace = Workspace'
  { _wDirectoryId :: !(Maybe Text),
    _wState :: !(Maybe WorkspaceState),
    _wIPAddress :: !(Maybe Text),
    _wModificationStates :: !(Maybe [ModificationState]),
    _wUserName :: !(Maybe Text),
    _wSubnetId :: !(Maybe Text),
    _wBundleId :: !(Maybe Text),
    _wWorkspaceProperties :: !(Maybe WorkspaceProperties),
    _wRootVolumeEncryptionEnabled :: !(Maybe Bool),
    _wErrorCode :: !(Maybe Text),
    _wVolumeEncryptionKey :: !(Maybe Text),
    _wComputerName :: !(Maybe Text),
    _wWorkspaceId :: !(Maybe Text),
    _wUserVolumeEncryptionEnabled :: !(Maybe Bool),
    _wErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Workspace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wDirectoryId' - The identifier of the AWS Directory Service directory for the WorkSpace.
--
-- * 'wState' - The operational state of the WorkSpace.
--
-- * 'wIPAddress' - The IP address of the WorkSpace.
--
-- * 'wModificationStates' - The modification states of the WorkSpace.
--
-- * 'wUserName' - The user for the WorkSpace.
--
-- * 'wSubnetId' - The identifier of the subnet for the WorkSpace.
--
-- * 'wBundleId' - The identifier of the bundle used to create the WorkSpace.
--
-- * 'wWorkspaceProperties' - The properties of the WorkSpace.
--
-- * 'wRootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- * 'wErrorCode' - The error code that is returned if the WorkSpace cannot be created.
--
-- * 'wVolumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- * 'wComputerName' - The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> .
--
-- * 'wWorkspaceId' - The identifier of the WorkSpace.
--
-- * 'wUserVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- * 'wErrorMessage' - The text of the error message that is returned if the WorkSpace cannot be created.
workspace ::
  Workspace
workspace =
  Workspace'
    { _wDirectoryId = Nothing,
      _wState = Nothing,
      _wIPAddress = Nothing,
      _wModificationStates = Nothing,
      _wUserName = Nothing,
      _wSubnetId = Nothing,
      _wBundleId = Nothing,
      _wWorkspaceProperties = Nothing,
      _wRootVolumeEncryptionEnabled = Nothing,
      _wErrorCode = Nothing,
      _wVolumeEncryptionKey = Nothing,
      _wComputerName = Nothing,
      _wWorkspaceId = Nothing,
      _wUserVolumeEncryptionEnabled = Nothing,
      _wErrorMessage = Nothing
    }

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
wDirectoryId :: Lens' Workspace (Maybe Text)
wDirectoryId = lens _wDirectoryId (\s a -> s {_wDirectoryId = a})

-- | The operational state of the WorkSpace.
wState :: Lens' Workspace (Maybe WorkspaceState)
wState = lens _wState (\s a -> s {_wState = a})

-- | The IP address of the WorkSpace.
wIPAddress :: Lens' Workspace (Maybe Text)
wIPAddress = lens _wIPAddress (\s a -> s {_wIPAddress = a})

-- | The modification states of the WorkSpace.
wModificationStates :: Lens' Workspace [ModificationState]
wModificationStates = lens _wModificationStates (\s a -> s {_wModificationStates = a}) . _Default . _Coerce

-- | The user for the WorkSpace.
wUserName :: Lens' Workspace (Maybe Text)
wUserName = lens _wUserName (\s a -> s {_wUserName = a})

-- | The identifier of the subnet for the WorkSpace.
wSubnetId :: Lens' Workspace (Maybe Text)
wSubnetId = lens _wSubnetId (\s a -> s {_wSubnetId = a})

-- | The identifier of the bundle used to create the WorkSpace.
wBundleId :: Lens' Workspace (Maybe Text)
wBundleId = lens _wBundleId (\s a -> s {_wBundleId = a})

-- | The properties of the WorkSpace.
wWorkspaceProperties :: Lens' Workspace (Maybe WorkspaceProperties)
wWorkspaceProperties = lens _wWorkspaceProperties (\s a -> s {_wWorkspaceProperties = a})

-- | Indicates whether the data stored on the root volume is encrypted.
wRootVolumeEncryptionEnabled :: Lens' Workspace (Maybe Bool)
wRootVolumeEncryptionEnabled = lens _wRootVolumeEncryptionEnabled (\s a -> s {_wRootVolumeEncryptionEnabled = a})

-- | The error code that is returned if the WorkSpace cannot be created.
wErrorCode :: Lens' Workspace (Maybe Text)
wErrorCode = lens _wErrorCode (\s a -> s {_wErrorCode = a})

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
wVolumeEncryptionKey :: Lens' Workspace (Maybe Text)
wVolumeEncryptionKey = lens _wVolumeEncryptionKey (\s a -> s {_wVolumeEncryptionKey = a})

-- | The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> .
wComputerName :: Lens' Workspace (Maybe Text)
wComputerName = lens _wComputerName (\s a -> s {_wComputerName = a})

-- | The identifier of the WorkSpace.
wWorkspaceId :: Lens' Workspace (Maybe Text)
wWorkspaceId = lens _wWorkspaceId (\s a -> s {_wWorkspaceId = a})

-- | Indicates whether the data stored on the user volume is encrypted.
wUserVolumeEncryptionEnabled :: Lens' Workspace (Maybe Bool)
wUserVolumeEncryptionEnabled = lens _wUserVolumeEncryptionEnabled (\s a -> s {_wUserVolumeEncryptionEnabled = a})

-- | The text of the error message that is returned if the WorkSpace cannot be created.
wErrorMessage :: Lens' Workspace (Maybe Text)
wErrorMessage = lens _wErrorMessage (\s a -> s {_wErrorMessage = a})

instance FromJSON Workspace where
  parseJSON =
    withObject
      "Workspace"
      ( \x ->
          Workspace'
            <$> (x .:? "DirectoryId")
            <*> (x .:? "State")
            <*> (x .:? "IpAddress")
            <*> (x .:? "ModificationStates" .!= mempty)
            <*> (x .:? "UserName")
            <*> (x .:? "SubnetId")
            <*> (x .:? "BundleId")
            <*> (x .:? "WorkspaceProperties")
            <*> (x .:? "RootVolumeEncryptionEnabled")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "VolumeEncryptionKey")
            <*> (x .:? "ComputerName")
            <*> (x .:? "WorkspaceId")
            <*> (x .:? "UserVolumeEncryptionEnabled")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable Workspace

instance NFData Workspace
