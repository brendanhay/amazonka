{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.Backup
  ( Backup (..)
  -- * Smart constructor
  , mkBackup
  -- * Lenses
  , bBackupArn
  , bBackupId
  , bBackupType
  , bCreatedAt
  , bDescription
  , bEngine
  , bEngineModel
  , bEngineVersion
  , bInstanceProfileArn
  , bInstanceType
  , bKeyPair
  , bPreferredBackupWindow
  , bPreferredMaintenanceWindow
  , bS3DataSize
  , bS3DataUrl
  , bS3LogUrl
  , bSecurityGroupIds
  , bServerName
  , bServiceRoleArn
  , bStatus
  , bStatusDescription
  , bSubnetIds
  , bToolsVersion
  , bUserArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types.BackupId as Types
import qualified Network.AWS.OpsWorksCM.Types.BackupStatus as Types
import qualified Network.AWS.OpsWorksCM.Types.BackupType as Types
import qualified Network.AWS.OpsWorksCM.Types.ServerName as Types
import qualified Network.AWS.OpsWorksCM.Types.TimeWindowDefinition as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a single backup. 
--
-- /See:/ 'mkBackup' smart constructor.
data Backup = Backup'
  { backupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the backup. 
  , backupId :: Core.Maybe Types.BackupId
    -- ^ The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@ 
  , backupType :: Core.Maybe Types.BackupType
    -- ^ The backup type. Valid values are @automated@ or @manual@ . 
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@ 
  , description :: Core.Maybe Core.Text
    -- ^ A user-provided description for a manual backup. This field is empty for automated backups. 
  , engine :: Core.Maybe Core.Text
    -- ^ The engine type that is obtained from the server when the backup is created. 
  , engineModel :: Core.Maybe Core.Text
    -- ^ The engine model that is obtained from the server when the backup is created. 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version that is obtained from the server when the backup is created. 
  , instanceProfileArn :: Core.Maybe Core.Text
    -- ^ The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup. 
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type that is obtained from the server when the backup is created. 
  , keyPair :: Core.Maybe Core.Text
    -- ^ The key pair that is obtained from the server when the backup is created. 
  , preferredBackupWindow :: Core.Maybe Types.TimeWindowDefinition
    -- ^ The preferred backup period that is obtained from the server when the backup is created. 
  , preferredMaintenanceWindow :: Core.Maybe Types.TimeWindowDefinition
    -- ^ The preferred maintenance period that is obtained from the server when the backup is created. 
  , s3DataSize :: Core.Maybe Core.Int
    -- ^ This field is deprecated and is no longer used. 
  , s3DataUrl :: Core.Maybe Core.Text
    -- ^ This field is deprecated and is no longer used. 
  , s3LogUrl :: Core.Maybe Core.Text
    -- ^ The Amazon S3 URL of the backup's log file. 
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ The security group IDs that are obtained from the server when the backup is created. 
  , serverName :: Core.Maybe Types.ServerName
    -- ^ The name of the server from which the backup was made. 
  , serviceRoleArn :: Core.Maybe Core.Text
    -- ^ The service role ARN that is obtained from the server when the backup is created. 
  , status :: Core.Maybe Types.BackupStatus
    -- ^ The status of a backup while in progress. 
  , statusDescription :: Core.Maybe Core.Text
    -- ^ An informational message about backup status. 
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The subnet IDs that are obtained from the server when the backup is created. 
  , toolsVersion :: Core.Maybe Core.Text
    -- ^ The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created. 
  , userArn :: Core.Maybe Core.Text
    -- ^ The IAM user ARN of the requester for manual backups. This field is empty for automated backups. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Backup' value with any optional fields omitted.
mkBackup
    :: Backup
mkBackup
  = Backup'{backupArn = Core.Nothing, backupId = Core.Nothing,
            backupType = Core.Nothing, createdAt = Core.Nothing,
            description = Core.Nothing, engine = Core.Nothing,
            engineModel = Core.Nothing, engineVersion = Core.Nothing,
            instanceProfileArn = Core.Nothing, instanceType = Core.Nothing,
            keyPair = Core.Nothing, preferredBackupWindow = Core.Nothing,
            preferredMaintenanceWindow = Core.Nothing,
            s3DataSize = Core.Nothing, s3DataUrl = Core.Nothing,
            s3LogUrl = Core.Nothing, securityGroupIds = Core.Nothing,
            serverName = Core.Nothing, serviceRoleArn = Core.Nothing,
            status = Core.Nothing, statusDescription = Core.Nothing,
            subnetIds = Core.Nothing, toolsVersion = Core.Nothing,
            userArn = Core.Nothing}

-- | The ARN of the backup. 
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
bBackupArn = Lens.field @"backupArn"
{-# INLINEABLE bBackupArn #-}
{-# DEPRECATED backupArn "Use generic-lens or generic-optics with 'backupArn' instead"  #-}

-- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@ 
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupId :: Lens.Lens' Backup (Core.Maybe Types.BackupId)
bBackupId = Lens.field @"backupId"
{-# INLINEABLE bBackupId #-}
{-# DEPRECATED backupId "Use generic-lens or generic-optics with 'backupId' instead"  #-}

-- | The backup type. Valid values are @automated@ or @manual@ . 
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupType :: Lens.Lens' Backup (Core.Maybe Types.BackupType)
bBackupType = Lens.field @"backupType"
{-# INLINEABLE bBackupType #-}
{-# DEPRECATED backupType "Use generic-lens or generic-optics with 'backupType' instead"  #-}

-- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@ 
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreatedAt :: Lens.Lens' Backup (Core.Maybe Core.NominalDiffTime)
bCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE bCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | A user-provided description for a manual backup. This field is empty for automated backups. 
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDescription :: Lens.Lens' Backup (Core.Maybe Core.Text)
bDescription = Lens.field @"description"
{-# INLINEABLE bDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The engine type that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngine :: Lens.Lens' Backup (Core.Maybe Core.Text)
bEngine = Lens.field @"engine"
{-# INLINEABLE bEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The engine model that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'engineModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineModel :: Lens.Lens' Backup (Core.Maybe Core.Text)
bEngineModel = Lens.field @"engineModel"
{-# INLINEABLE bEngineModel #-}
{-# DEPRECATED engineModel "Use generic-lens or generic-optics with 'engineModel' instead"  #-}

-- | The engine version that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEngineVersion :: Lens.Lens' Backup (Core.Maybe Core.Text)
bEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE bEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup. 
--
-- /Note:/ Consider using 'instanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceProfileArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
bInstanceProfileArn = Lens.field @"instanceProfileArn"
{-# INLINEABLE bInstanceProfileArn #-}
{-# DEPRECATED instanceProfileArn "Use generic-lens or generic-optics with 'instanceProfileArn' instead"  #-}

-- | The instance type that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInstanceType :: Lens.Lens' Backup (Core.Maybe Core.Text)
bInstanceType = Lens.field @"instanceType"
{-# INLINEABLE bInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The key pair that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bKeyPair :: Lens.Lens' Backup (Core.Maybe Core.Text)
bKeyPair = Lens.field @"keyPair"
{-# INLINEABLE bKeyPair #-}
{-# DEPRECATED keyPair "Use generic-lens or generic-optics with 'keyPair' instead"  #-}

-- | The preferred backup period that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredBackupWindow :: Lens.Lens' Backup (Core.Maybe Types.TimeWindowDefinition)
bPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE bPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

-- | The preferred maintenance period that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPreferredMaintenanceWindow :: Lens.Lens' Backup (Core.Maybe Types.TimeWindowDefinition)
bPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE bPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | This field is deprecated and is no longer used. 
--
-- /Note:/ Consider using 's3DataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataSize :: Lens.Lens' Backup (Core.Maybe Core.Int)
bS3DataSize = Lens.field @"s3DataSize"
{-# INLINEABLE bS3DataSize #-}
{-# DEPRECATED s3DataSize "Use generic-lens or generic-optics with 's3DataSize' instead"  #-}

-- | This field is deprecated and is no longer used. 
--
-- /Note:/ Consider using 's3DataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3DataUrl :: Lens.Lens' Backup (Core.Maybe Core.Text)
bS3DataUrl = Lens.field @"s3DataUrl"
{-# INLINEABLE bS3DataUrl #-}
{-# DEPRECATED s3DataUrl "Use generic-lens or generic-optics with 's3DataUrl' instead"  #-}

-- | The Amazon S3 URL of the backup's log file. 
--
-- /Note:/ Consider using 's3LogUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bS3LogUrl :: Lens.Lens' Backup (Core.Maybe Core.Text)
bS3LogUrl = Lens.field @"s3LogUrl"
{-# INLINEABLE bS3LogUrl #-}
{-# DEPRECATED s3LogUrl "Use generic-lens or generic-optics with 's3LogUrl' instead"  #-}

-- | The security group IDs that are obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecurityGroupIds :: Lens.Lens' Backup (Core.Maybe [Core.Text])
bSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE bSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The name of the server from which the backup was made. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServerName :: Lens.Lens' Backup (Core.Maybe Types.ServerName)
bServerName = Lens.field @"serverName"
{-# INLINEABLE bServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The service role ARN that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRoleArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
bServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE bServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | The status of a backup while in progress. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatus :: Lens.Lens' Backup (Core.Maybe Types.BackupStatus)
bStatus = Lens.field @"status"
{-# INLINEABLE bStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An informational message about backup status. 
--
-- /Note:/ Consider using 'statusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatusDescription :: Lens.Lens' Backup (Core.Maybe Core.Text)
bStatusDescription = Lens.field @"statusDescription"
{-# INLINEABLE bStatusDescription #-}
{-# DEPRECATED statusDescription "Use generic-lens or generic-optics with 'statusDescription' instead"  #-}

-- | The subnet IDs that are obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSubnetIds :: Lens.Lens' Backup (Core.Maybe [Core.Text])
bSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE bSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created. 
--
-- /Note:/ Consider using 'toolsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bToolsVersion :: Lens.Lens' Backup (Core.Maybe Core.Text)
bToolsVersion = Lens.field @"toolsVersion"
{-# INLINEABLE bToolsVersion #-}
{-# DEPRECATED toolsVersion "Use generic-lens or generic-optics with 'toolsVersion' instead"  #-}

-- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups. 
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bUserArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
bUserArn = Lens.field @"userArn"
{-# INLINEABLE bUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.FromJSON Backup where
        parseJSON
          = Core.withObject "Backup" Core.$
              \ x ->
                Backup' Core.<$>
                  (x Core..:? "BackupArn") Core.<*> x Core..:? "BackupId" Core.<*>
                    x Core..:? "BackupType"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Engine"
                    Core.<*> x Core..:? "EngineModel"
                    Core.<*> x Core..:? "EngineVersion"
                    Core.<*> x Core..:? "InstanceProfileArn"
                    Core.<*> x Core..:? "InstanceType"
                    Core.<*> x Core..:? "KeyPair"
                    Core.<*> x Core..:? "PreferredBackupWindow"
                    Core.<*> x Core..:? "PreferredMaintenanceWindow"
                    Core.<*> x Core..:? "S3DataSize"
                    Core.<*> x Core..:? "S3DataUrl"
                    Core.<*> x Core..:? "S3LogUrl"
                    Core.<*> x Core..:? "SecurityGroupIds"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "ServiceRoleArn"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusDescription"
                    Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "ToolsVersion"
                    Core.<*> x Core..:? "UserArn"
