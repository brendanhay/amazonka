{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.Backup where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType
import Network.AWS.Prelude

-- | Describes a single backup.
--
--
--
-- /See:/ 'backup' smart constructor.
data Backup = Backup'
  { _bEngineVersion :: !(Maybe Text),
    _bServiceRoleARN :: !(Maybe Text),
    _bStatus :: !(Maybe BackupStatus),
    _bInstanceProfileARN :: !(Maybe Text),
    _bSecurityGroupIds :: !(Maybe [Text]),
    _bStatusDescription :: !(Maybe Text),
    _bServerName :: !(Maybe Text),
    _bSubnetIds :: !(Maybe [Text]),
    _bKeyPair :: !(Maybe Text),
    _bCreatedAt :: !(Maybe POSIX),
    _bBackupId :: !(Maybe Text),
    _bEngine :: !(Maybe Text),
    _bInstanceType :: !(Maybe Text),
    _bEngineModel :: !(Maybe Text),
    _bPreferredMaintenanceWindow :: !(Maybe Text),
    _bUserARN :: !(Maybe Text),
    _bPreferredBackupWindow :: !(Maybe Text),
    _bS3LogURL :: !(Maybe Text),
    _bS3DataSize :: !(Maybe Int),
    _bBackupARN :: !(Maybe Text),
    _bS3DataURL :: !(Maybe Text),
    _bDescription :: !(Maybe Text),
    _bBackupType :: !(Maybe BackupType),
    _bToolsVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bEngineVersion' - The engine version that is obtained from the server when the backup is created.
--
-- * 'bServiceRoleARN' - The service role ARN that is obtained from the server when the backup is created.
--
-- * 'bStatus' - The status of a backup while in progress.
--
-- * 'bInstanceProfileARN' - The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
--
-- * 'bSecurityGroupIds' - The security group IDs that are obtained from the server when the backup is created.
--
-- * 'bStatusDescription' - An informational message about backup status.
--
-- * 'bServerName' - The name of the server from which the backup was made.
--
-- * 'bSubnetIds' - The subnet IDs that are obtained from the server when the backup is created.
--
-- * 'bKeyPair' - The key pair that is obtained from the server when the backup is created.
--
-- * 'bCreatedAt' - The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
--
-- * 'bBackupId' - The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
--
-- * 'bEngine' - The engine type that is obtained from the server when the backup is created.
--
-- * 'bInstanceType' - The instance type that is obtained from the server when the backup is created.
--
-- * 'bEngineModel' - The engine model that is obtained from the server when the backup is created.
--
-- * 'bPreferredMaintenanceWindow' - The preferred maintenance period that is obtained from the server when the backup is created.
--
-- * 'bUserARN' - The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
--
-- * 'bPreferredBackupWindow' - The preferred backup period that is obtained from the server when the backup is created.
--
-- * 'bS3LogURL' - The Amazon S3 URL of the backup's log file.
--
-- * 'bS3DataSize' - This field is deprecated and is no longer used.
--
-- * 'bBackupARN' - The ARN of the backup.
--
-- * 'bS3DataURL' - This field is deprecated and is no longer used.
--
-- * 'bDescription' - A user-provided description for a manual backup. This field is empty for automated backups.
--
-- * 'bBackupType' - The backup type. Valid values are @automated@ or @manual@ .
--
-- * 'bToolsVersion' - The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
backup ::
  Backup
backup =
  Backup'
    { _bEngineVersion = Nothing,
      _bServiceRoleARN = Nothing,
      _bStatus = Nothing,
      _bInstanceProfileARN = Nothing,
      _bSecurityGroupIds = Nothing,
      _bStatusDescription = Nothing,
      _bServerName = Nothing,
      _bSubnetIds = Nothing,
      _bKeyPair = Nothing,
      _bCreatedAt = Nothing,
      _bBackupId = Nothing,
      _bEngine = Nothing,
      _bInstanceType = Nothing,
      _bEngineModel = Nothing,
      _bPreferredMaintenanceWindow = Nothing,
      _bUserARN = Nothing,
      _bPreferredBackupWindow = Nothing,
      _bS3LogURL = Nothing,
      _bS3DataSize = Nothing,
      _bBackupARN = Nothing,
      _bS3DataURL = Nothing,
      _bDescription = Nothing,
      _bBackupType = Nothing,
      _bToolsVersion = Nothing
    }

-- | The engine version that is obtained from the server when the backup is created.
bEngineVersion :: Lens' Backup (Maybe Text)
bEngineVersion = lens _bEngineVersion (\s a -> s {_bEngineVersion = a})

-- | The service role ARN that is obtained from the server when the backup is created.
bServiceRoleARN :: Lens' Backup (Maybe Text)
bServiceRoleARN = lens _bServiceRoleARN (\s a -> s {_bServiceRoleARN = a})

-- | The status of a backup while in progress.
bStatus :: Lens' Backup (Maybe BackupStatus)
bStatus = lens _bStatus (\s a -> s {_bStatus = a})

-- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
bInstanceProfileARN :: Lens' Backup (Maybe Text)
bInstanceProfileARN = lens _bInstanceProfileARN (\s a -> s {_bInstanceProfileARN = a})

-- | The security group IDs that are obtained from the server when the backup is created.
bSecurityGroupIds :: Lens' Backup [Text]
bSecurityGroupIds = lens _bSecurityGroupIds (\s a -> s {_bSecurityGroupIds = a}) . _Default . _Coerce

-- | An informational message about backup status.
bStatusDescription :: Lens' Backup (Maybe Text)
bStatusDescription = lens _bStatusDescription (\s a -> s {_bStatusDescription = a})

-- | The name of the server from which the backup was made.
bServerName :: Lens' Backup (Maybe Text)
bServerName = lens _bServerName (\s a -> s {_bServerName = a})

-- | The subnet IDs that are obtained from the server when the backup is created.
bSubnetIds :: Lens' Backup [Text]
bSubnetIds = lens _bSubnetIds (\s a -> s {_bSubnetIds = a}) . _Default . _Coerce

-- | The key pair that is obtained from the server when the backup is created.
bKeyPair :: Lens' Backup (Maybe Text)
bKeyPair = lens _bKeyPair (\s a -> s {_bKeyPair = a})

-- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
bCreatedAt :: Lens' Backup (Maybe UTCTime)
bCreatedAt = lens _bCreatedAt (\s a -> s {_bCreatedAt = a}) . mapping _Time

-- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
bBackupId :: Lens' Backup (Maybe Text)
bBackupId = lens _bBackupId (\s a -> s {_bBackupId = a})

-- | The engine type that is obtained from the server when the backup is created.
bEngine :: Lens' Backup (Maybe Text)
bEngine = lens _bEngine (\s a -> s {_bEngine = a})

-- | The instance type that is obtained from the server when the backup is created.
bInstanceType :: Lens' Backup (Maybe Text)
bInstanceType = lens _bInstanceType (\s a -> s {_bInstanceType = a})

-- | The engine model that is obtained from the server when the backup is created.
bEngineModel :: Lens' Backup (Maybe Text)
bEngineModel = lens _bEngineModel (\s a -> s {_bEngineModel = a})

-- | The preferred maintenance period that is obtained from the server when the backup is created.
bPreferredMaintenanceWindow :: Lens' Backup (Maybe Text)
bPreferredMaintenanceWindow = lens _bPreferredMaintenanceWindow (\s a -> s {_bPreferredMaintenanceWindow = a})

-- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
bUserARN :: Lens' Backup (Maybe Text)
bUserARN = lens _bUserARN (\s a -> s {_bUserARN = a})

-- | The preferred backup period that is obtained from the server when the backup is created.
bPreferredBackupWindow :: Lens' Backup (Maybe Text)
bPreferredBackupWindow = lens _bPreferredBackupWindow (\s a -> s {_bPreferredBackupWindow = a})

-- | The Amazon S3 URL of the backup's log file.
bS3LogURL :: Lens' Backup (Maybe Text)
bS3LogURL = lens _bS3LogURL (\s a -> s {_bS3LogURL = a})

-- | This field is deprecated and is no longer used.
bS3DataSize :: Lens' Backup (Maybe Int)
bS3DataSize = lens _bS3DataSize (\s a -> s {_bS3DataSize = a})

-- | The ARN of the backup.
bBackupARN :: Lens' Backup (Maybe Text)
bBackupARN = lens _bBackupARN (\s a -> s {_bBackupARN = a})

-- | This field is deprecated and is no longer used.
bS3DataURL :: Lens' Backup (Maybe Text)
bS3DataURL = lens _bS3DataURL (\s a -> s {_bS3DataURL = a})

-- | A user-provided description for a manual backup. This field is empty for automated backups.
bDescription :: Lens' Backup (Maybe Text)
bDescription = lens _bDescription (\s a -> s {_bDescription = a})

-- | The backup type. Valid values are @automated@ or @manual@ .
bBackupType :: Lens' Backup (Maybe BackupType)
bBackupType = lens _bBackupType (\s a -> s {_bBackupType = a})

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
bToolsVersion :: Lens' Backup (Maybe Text)
bToolsVersion = lens _bToolsVersion (\s a -> s {_bToolsVersion = a})

instance FromJSON Backup where
  parseJSON =
    withObject
      "Backup"
      ( \x ->
          Backup'
            <$> (x .:? "EngineVersion")
            <*> (x .:? "ServiceRoleArn")
            <*> (x .:? "Status")
            <*> (x .:? "InstanceProfileArn")
            <*> (x .:? "SecurityGroupIds" .!= mempty)
            <*> (x .:? "StatusDescription")
            <*> (x .:? "ServerName")
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "KeyPair")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "BackupId")
            <*> (x .:? "Engine")
            <*> (x .:? "InstanceType")
            <*> (x .:? "EngineModel")
            <*> (x .:? "PreferredMaintenanceWindow")
            <*> (x .:? "UserArn")
            <*> (x .:? "PreferredBackupWindow")
            <*> (x .:? "S3LogUrl")
            <*> (x .:? "S3DataSize")
            <*> (x .:? "BackupArn")
            <*> (x .:? "S3DataUrl")
            <*> (x .:? "Description")
            <*> (x .:? "BackupType")
            <*> (x .:? "ToolsVersion")
      )

instance Hashable Backup

instance NFData Backup
