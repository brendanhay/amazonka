{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.Backup
  ( Backup (..),

    -- * Smart constructor
    mkBackup,

    -- * Lenses
    bEngineVersion,
    bServiceRoleARN,
    bStatus,
    bInstanceProfileARN,
    bSecurityGroupIds,
    bStatusDescription,
    bServerName,
    bSubnetIds,
    bKeyPair,
    bCreatedAt,
    bBackupId,
    bEngine,
    bInstanceType,
    bEngineModel,
    bPreferredMaintenanceWindow,
    bUserARN,
    bPreferredBackupWindow,
    bS3LogURL,
    bS3DataSize,
    bBackupARN,
    bS3DataURL,
    bDescription,
    bBackupType,
    bToolsVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType
import qualified Network.AWS.Prelude as Lude

-- | Describes a single backup.
--
-- /See:/ 'mkBackup' smart constructor.
data Backup = Backup'
  { engineVersion :: Lude.Maybe Lude.Text,
    serviceRoleARN :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe BackupStatus,
    instanceProfileARN :: Lude.Maybe Lude.Text,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    statusDescription :: Lude.Maybe Lude.Text,
    serverName :: Lude.Maybe Lude.Text,
    subnetIds :: Lude.Maybe [Lude.Text],
    keyPair :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    backupId :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    engineModel :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    s3LogURL :: Lude.Maybe Lude.Text,
    s3DataSize :: Lude.Maybe Lude.Int,
    backupARN :: Lude.Maybe Lude.Text,
    s3DataURL :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    backupType :: Lude.Maybe BackupType,
    toolsVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- * 'backupARN' - The ARN of the backup.
-- * 'backupId' - The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
-- * 'backupType' - The backup type. Valid values are @automated@ or @manual@ .
-- * 'createdAt' - The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
-- * 'description' - A user-provided description for a manual backup. This field is empty for automated backups.
-- * 'engine' - The engine type that is obtained from the server when the backup is created.
-- * 'engineModel' - The engine model that is obtained from the server when the backup is created.
-- * 'engineVersion' - The engine version that is obtained from the server when the backup is created.
-- * 'instanceProfileARN' - The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
-- * 'instanceType' - The instance type that is obtained from the server when the backup is created.
-- * 'keyPair' - The key pair that is obtained from the server when the backup is created.
-- * 'preferredBackupWindow' - The preferred backup period that is obtained from the server when the backup is created.
-- * 'preferredMaintenanceWindow' - The preferred maintenance period that is obtained from the server when the backup is created.
-- * 's3DataSize' - This field is deprecated and is no longer used.
-- * 's3DataURL' - This field is deprecated and is no longer used.
-- * 's3LogURL' - The Amazon S3 URL of the backup's log file.
-- * 'securityGroupIds' - The security group IDs that are obtained from the server when the backup is created.
-- * 'serverName' - The name of the server from which the backup was made.
-- * 'serviceRoleARN' - The service role ARN that is obtained from the server when the backup is created.
-- * 'status' - The status of a backup while in progress.
-- * 'statusDescription' - An informational message about backup status.
-- * 'subnetIds' - The subnet IDs that are obtained from the server when the backup is created.
-- * 'toolsVersion' - The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
-- * 'userARN' - The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
mkBackup ::
  Backup
mkBackup =
  Backup'
    { engineVersion = Lude.Nothing,
      serviceRoleARN = Lude.Nothing,
      status = Lude.Nothing,
      instanceProfileARN = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      statusDescription = Lude.Nothing,
      serverName = Lude.Nothing,
      subnetIds = Lude.Nothing,
      keyPair = Lude.Nothing,
      createdAt = Lude.Nothing,
      backupId = Lude.Nothing,
      engine = Lude.Nothing,
      instanceType = Lude.Nothing,
      engineModel = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      userARN = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      s3LogURL = Lude.Nothing,
      s3DataSize = Lude.Nothing,
      backupARN = Lude.Nothing,
      s3DataURL = Lude.Nothing,
      description = Lude.Nothing,
      backupType = Lude.Nothing,
      toolsVersion = Lude.Nothing
    }

-- | The engine version that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineVersion :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bEngineVersion = Lens.lens (engineVersion :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: Backup)
{-# DEPRECATED bEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The service role ARN that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRoleARN :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bServiceRoleARN = Lens.lens (serviceRoleARN :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: Backup)
{-# DEPRECATED bServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The status of a backup while in progress.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatus :: Lens.Lens' Backup (Lude.Maybe BackupStatus)
bStatus = Lens.lens (status :: Backup -> Lude.Maybe BackupStatus) (\s a -> s {status = a} :: Backup)
{-# DEPRECATED bStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
--
-- /Note:/ Consider using 'instanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceProfileARN :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bInstanceProfileARN = Lens.lens (instanceProfileARN :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {instanceProfileARN = a} :: Backup)
{-# DEPRECATED bInstanceProfileARN "Use generic-lens or generic-optics with 'instanceProfileARN' instead." #-}

-- | The security group IDs that are obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecurityGroupIds :: Lens.Lens' Backup (Lude.Maybe [Lude.Text])
bSecurityGroupIds = Lens.lens (securityGroupIds :: Backup -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: Backup)
{-# DEPRECATED bSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | An informational message about backup status.
--
-- /Note:/ Consider using 'statusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatusDescription :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bStatusDescription = Lens.lens (statusDescription :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {statusDescription = a} :: Backup)
{-# DEPRECATED bStatusDescription "Use generic-lens or generic-optics with 'statusDescription' instead." #-}

-- | The name of the server from which the backup was made.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServerName :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bServerName = Lens.lens (serverName :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: Backup)
{-# DEPRECATED bServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The subnet IDs that are obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSubnetIds :: Lens.Lens' Backup (Lude.Maybe [Lude.Text])
bSubnetIds = Lens.lens (subnetIds :: Backup -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: Backup)
{-# DEPRECATED bSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The key pair that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bKeyPair :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bKeyPair = Lens.lens (keyPair :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {keyPair = a} :: Backup)
{-# DEPRECATED bKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreatedAt :: Lens.Lens' Backup (Lude.Maybe Lude.Timestamp)
bCreatedAt = Lens.lens (createdAt :: Backup -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Backup)
{-# DEPRECATED bCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupId :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bBackupId = Lens.lens (backupId :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {backupId = a} :: Backup)
{-# DEPRECATED bBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | The engine type that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngine :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bEngine = Lens.lens (engine :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: Backup)
{-# DEPRECATED bEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The instance type that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceType :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bInstanceType = Lens.lens (instanceType :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: Backup)
{-# DEPRECATED bInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The engine model that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engineModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineModel :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bEngineModel = Lens.lens (engineModel :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {engineModel = a} :: Backup)
{-# DEPRECATED bEngineModel "Use generic-lens or generic-optics with 'engineModel' instead." #-}

-- | The preferred maintenance period that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredMaintenanceWindow :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: Backup)
{-# DEPRECATED bPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bUserARN :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bUserARN = Lens.lens (userARN :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: Backup)
{-# DEPRECATED bUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The preferred backup period that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredBackupWindow :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bPreferredBackupWindow = Lens.lens (preferredBackupWindow :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: Backup)
{-# DEPRECATED bPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The Amazon S3 URL of the backup's log file.
--
-- /Note:/ Consider using 's3LogURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3LogURL :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bS3LogURL = Lens.lens (s3LogURL :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {s3LogURL = a} :: Backup)
{-# DEPRECATED bS3LogURL "Use generic-lens or generic-optics with 's3LogURL' instead." #-}

-- | This field is deprecated and is no longer used.
--
-- /Note:/ Consider using 's3DataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataSize :: Lens.Lens' Backup (Lude.Maybe Lude.Int)
bS3DataSize = Lens.lens (s3DataSize :: Backup -> Lude.Maybe Lude.Int) (\s a -> s {s3DataSize = a} :: Backup)
{-# DEPRECATED bS3DataSize "Use generic-lens or generic-optics with 's3DataSize' instead." #-}

-- | The ARN of the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupARN :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bBackupARN = Lens.lens (backupARN :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {backupARN = a} :: Backup)
{-# DEPRECATED bBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

-- | This field is deprecated and is no longer used.
--
-- /Note:/ Consider using 's3DataURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataURL :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bS3DataURL = Lens.lens (s3DataURL :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {s3DataURL = a} :: Backup)
{-# DEPRECATED bS3DataURL "Use generic-lens or generic-optics with 's3DataURL' instead." #-}

-- | A user-provided description for a manual backup. This field is empty for automated backups.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDescription :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bDescription = Lens.lens (description :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Backup)
{-# DEPRECATED bDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The backup type. Valid values are @automated@ or @manual@ .
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupType :: Lens.Lens' Backup (Lude.Maybe BackupType)
bBackupType = Lens.lens (backupType :: Backup -> Lude.Maybe BackupType) (\s a -> s {backupType = a} :: Backup)
{-# DEPRECATED bBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'toolsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bToolsVersion :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bToolsVersion = Lens.lens (toolsVersion :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {toolsVersion = a} :: Backup)
{-# DEPRECATED bToolsVersion "Use generic-lens or generic-optics with 'toolsVersion' instead." #-}

instance Lude.FromJSON Backup where
  parseJSON =
    Lude.withObject
      "Backup"
      ( \x ->
          Backup'
            Lude.<$> (x Lude..:? "EngineVersion")
            Lude.<*> (x Lude..:? "ServiceRoleArn")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "InstanceProfileArn")
            Lude.<*> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StatusDescription")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "KeyPair")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "BackupId")
            Lude.<*> (x Lude..:? "Engine")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "EngineModel")
            Lude.<*> (x Lude..:? "PreferredMaintenanceWindow")
            Lude.<*> (x Lude..:? "UserArn")
            Lude.<*> (x Lude..:? "PreferredBackupWindow")
            Lude.<*> (x Lude..:? "S3LogUrl")
            Lude.<*> (x Lude..:? "S3DataSize")
            Lude.<*> (x Lude..:? "BackupArn")
            Lude.<*> (x Lude..:? "S3DataUrl")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "BackupType")
            Lude.<*> (x Lude..:? "ToolsVersion")
      )
