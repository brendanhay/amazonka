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
    bBackupArn,
    bBackupId,
    bBackupType,
    bCreatedAt,
    bDescription,
    bEngine,
    bEngineModel,
    bEngineVersion,
    bInstanceProfileArn,
    bInstanceType,
    bKeyPair,
    bPreferredBackupWindow,
    bPreferredMaintenanceWindow,
    bS3DataSize,
    bS3DataUrl,
    bS3LogUrl,
    bSecurityGroupIds,
    bServerName,
    bServiceRoleArn,
    bStatus,
    bStatusDescription,
    bSubnetIds,
    bToolsVersion,
    bUserArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types.BackupId as Types
import qualified Network.AWS.OpsWorksCM.Types.BackupStatus as Types
import qualified Network.AWS.OpsWorksCM.Types.BackupType as Types
import qualified Network.AWS.OpsWorksCM.Types.ServerName as Types
import qualified Network.AWS.OpsWorksCM.Types.String as Types
import qualified Network.AWS.OpsWorksCM.Types.TimeWindowDefinition as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a single backup.
--
-- /See:/ 'mkBackup' smart constructor.
data Backup = Backup'
  { -- | The ARN of the backup.
    backupArn :: Core.Maybe Types.String,
    -- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
    backupId :: Core.Maybe Types.BackupId,
    -- | The backup type. Valid values are @automated@ or @manual@ .
    backupType :: Core.Maybe Types.BackupType,
    -- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | A user-provided description for a manual backup. This field is empty for automated backups.
    description :: Core.Maybe Types.String,
    -- | The engine type that is obtained from the server when the backup is created.
    engine :: Core.Maybe Types.String,
    -- | The engine model that is obtained from the server when the backup is created.
    engineModel :: Core.Maybe Types.String,
    -- | The engine version that is obtained from the server when the backup is created.
    engineVersion :: Core.Maybe Types.String,
    -- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
    instanceProfileArn :: Core.Maybe Types.String,
    -- | The instance type that is obtained from the server when the backup is created.
    instanceType :: Core.Maybe Types.String,
    -- | The key pair that is obtained from the server when the backup is created.
    keyPair :: Core.Maybe Types.String,
    -- | The preferred backup period that is obtained from the server when the backup is created.
    preferredBackupWindow :: Core.Maybe Types.TimeWindowDefinition,
    -- | The preferred maintenance period that is obtained from the server when the backup is created.
    preferredMaintenanceWindow :: Core.Maybe Types.TimeWindowDefinition,
    -- | This field is deprecated and is no longer used.
    s3DataSize :: Core.Maybe Core.Int,
    -- | This field is deprecated and is no longer used.
    s3DataUrl :: Core.Maybe Types.String,
    -- | The Amazon S3 URL of the backup's log file.
    s3LogUrl :: Core.Maybe Types.String,
    -- | The security group IDs that are obtained from the server when the backup is created.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | The name of the server from which the backup was made.
    serverName :: Core.Maybe Types.ServerName,
    -- | The service role ARN that is obtained from the server when the backup is created.
    serviceRoleArn :: Core.Maybe Types.String,
    -- | The status of a backup while in progress.
    status :: Core.Maybe Types.BackupStatus,
    -- | An informational message about backup status.
    statusDescription :: Core.Maybe Types.String,
    -- | The subnet IDs that are obtained from the server when the backup is created.
    subnetIds :: Core.Maybe [Types.String],
    -- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
    toolsVersion :: Core.Maybe Types.String,
    -- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
    userArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Backup' value with any optional fields omitted.
mkBackup ::
  Backup
mkBackup =
  Backup'
    { backupArn = Core.Nothing,
      backupId = Core.Nothing,
      backupType = Core.Nothing,
      createdAt = Core.Nothing,
      description = Core.Nothing,
      engine = Core.Nothing,
      engineModel = Core.Nothing,
      engineVersion = Core.Nothing,
      instanceProfileArn = Core.Nothing,
      instanceType = Core.Nothing,
      keyPair = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      s3DataSize = Core.Nothing,
      s3DataUrl = Core.Nothing,
      s3LogUrl = Core.Nothing,
      securityGroupIds = Core.Nothing,
      serverName = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      status = Core.Nothing,
      statusDescription = Core.Nothing,
      subnetIds = Core.Nothing,
      toolsVersion = Core.Nothing,
      userArn = Core.Nothing
    }

-- | The ARN of the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupArn :: Lens.Lens' Backup (Core.Maybe Types.String)
bBackupArn = Lens.field @"backupArn"
{-# DEPRECATED bBackupArn "Use generic-lens or generic-optics with 'backupArn' instead." #-}

-- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupId :: Lens.Lens' Backup (Core.Maybe Types.BackupId)
bBackupId = Lens.field @"backupId"
{-# DEPRECATED bBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | The backup type. Valid values are @automated@ or @manual@ .
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupType :: Lens.Lens' Backup (Core.Maybe Types.BackupType)
bBackupType = Lens.field @"backupType"
{-# DEPRECATED bBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreatedAt :: Lens.Lens' Backup (Core.Maybe Core.NominalDiffTime)
bCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED bCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A user-provided description for a manual backup. This field is empty for automated backups.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDescription :: Lens.Lens' Backup (Core.Maybe Types.String)
bDescription = Lens.field @"description"
{-# DEPRECATED bDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The engine type that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngine :: Lens.Lens' Backup (Core.Maybe Types.String)
bEngine = Lens.field @"engine"
{-# DEPRECATED bEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The engine model that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engineModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineModel :: Lens.Lens' Backup (Core.Maybe Types.String)
bEngineModel = Lens.field @"engineModel"
{-# DEPRECATED bEngineModel "Use generic-lens or generic-optics with 'engineModel' instead." #-}

-- | The engine version that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineVersion :: Lens.Lens' Backup (Core.Maybe Types.String)
bEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED bEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
--
-- /Note:/ Consider using 'instanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceProfileArn :: Lens.Lens' Backup (Core.Maybe Types.String)
bInstanceProfileArn = Lens.field @"instanceProfileArn"
{-# DEPRECATED bInstanceProfileArn "Use generic-lens or generic-optics with 'instanceProfileArn' instead." #-}

-- | The instance type that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceType :: Lens.Lens' Backup (Core.Maybe Types.String)
bInstanceType = Lens.field @"instanceType"
{-# DEPRECATED bInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The key pair that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bKeyPair :: Lens.Lens' Backup (Core.Maybe Types.String)
bKeyPair = Lens.field @"keyPair"
{-# DEPRECATED bKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The preferred backup period that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredBackupWindow :: Lens.Lens' Backup (Core.Maybe Types.TimeWindowDefinition)
bPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED bPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The preferred maintenance period that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredMaintenanceWindow :: Lens.Lens' Backup (Core.Maybe Types.TimeWindowDefinition)
bPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED bPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | This field is deprecated and is no longer used.
--
-- /Note:/ Consider using 's3DataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataSize :: Lens.Lens' Backup (Core.Maybe Core.Int)
bS3DataSize = Lens.field @"s3DataSize"
{-# DEPRECATED bS3DataSize "Use generic-lens or generic-optics with 's3DataSize' instead." #-}

-- | This field is deprecated and is no longer used.
--
-- /Note:/ Consider using 's3DataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataUrl :: Lens.Lens' Backup (Core.Maybe Types.String)
bS3DataUrl = Lens.field @"s3DataUrl"
{-# DEPRECATED bS3DataUrl "Use generic-lens or generic-optics with 's3DataUrl' instead." #-}

-- | The Amazon S3 URL of the backup's log file.
--
-- /Note:/ Consider using 's3LogUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3LogUrl :: Lens.Lens' Backup (Core.Maybe Types.String)
bS3LogUrl = Lens.field @"s3LogUrl"
{-# DEPRECATED bS3LogUrl "Use generic-lens or generic-optics with 's3LogUrl' instead." #-}

-- | The security group IDs that are obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecurityGroupIds :: Lens.Lens' Backup (Core.Maybe [Types.String])
bSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED bSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The name of the server from which the backup was made.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServerName :: Lens.Lens' Backup (Core.Maybe Types.ServerName)
bServerName = Lens.field @"serverName"
{-# DEPRECATED bServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The service role ARN that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRoleArn :: Lens.Lens' Backup (Core.Maybe Types.String)
bServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED bServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The status of a backup while in progress.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatus :: Lens.Lens' Backup (Core.Maybe Types.BackupStatus)
bStatus = Lens.field @"status"
{-# DEPRECATED bStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An informational message about backup status.
--
-- /Note:/ Consider using 'statusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatusDescription :: Lens.Lens' Backup (Core.Maybe Types.String)
bStatusDescription = Lens.field @"statusDescription"
{-# DEPRECATED bStatusDescription "Use generic-lens or generic-optics with 'statusDescription' instead." #-}

-- | The subnet IDs that are obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSubnetIds :: Lens.Lens' Backup (Core.Maybe [Types.String])
bSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED bSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
--
-- /Note:/ Consider using 'toolsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bToolsVersion :: Lens.Lens' Backup (Core.Maybe Types.String)
bToolsVersion = Lens.field @"toolsVersion"
{-# DEPRECATED bToolsVersion "Use generic-lens or generic-optics with 'toolsVersion' instead." #-}

-- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bUserArn :: Lens.Lens' Backup (Core.Maybe Types.String)
bUserArn = Lens.field @"userArn"
{-# DEPRECATED bUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON Backup where
  parseJSON =
    Core.withObject "Backup" Core.$
      \x ->
        Backup'
          Core.<$> (x Core..:? "BackupArn")
          Core.<*> (x Core..:? "BackupId")
          Core.<*> (x Core..:? "BackupType")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Engine")
          Core.<*> (x Core..:? "EngineModel")
          Core.<*> (x Core..:? "EngineVersion")
          Core.<*> (x Core..:? "InstanceProfileArn")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "KeyPair")
          Core.<*> (x Core..:? "PreferredBackupWindow")
          Core.<*> (x Core..:? "PreferredMaintenanceWindow")
          Core.<*> (x Core..:? "S3DataSize")
          Core.<*> (x Core..:? "S3DataUrl")
          Core.<*> (x Core..:? "S3LogUrl")
          Core.<*> (x Core..:? "SecurityGroupIds")
          Core.<*> (x Core..:? "ServerName")
          Core.<*> (x Core..:? "ServiceRoleArn")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDescription")
          Core.<*> (x Core..:? "SubnetIds")
          Core.<*> (x Core..:? "ToolsVersion")
          Core.<*> (x Core..:? "UserArn")
