{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.CreateBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup for an existing table.
--
-- Each time you create an on-demand backup, the entire table data is backed up. There is no limit to the number of on-demand backups that can be taken. 
-- When you create an on-demand backup, a time marker of the request is cataloged, and the backup is created asynchronously, by applying all changes until the time of the request to the last full table snapshot. Backup requests are processed instantaneously and become available for restore within minutes. 
-- You can call @CreateBackup@ at a maximum rate of 50 times per second.
-- All backups in DynamoDB work without consuming any provisioned throughput on the table.
-- If you submit a backup request on 2018-12-14 at 14:25:00, the backup is guaranteed to contain all data committed to the table up to 14:24:00, and data committed after 14:26:00 will not be. The backup might contain data modifications made between 14:24:00 and 14:26:00. On-demand backup does not support causal consistency. 
-- Along with data, the following are also included on the backups: 
--
--     * Global secondary indexes (GSIs)
--
--
--     * Local secondary indexes (LSIs)
--
--
--     * Streams
--
--
--     * Provisioned read and write capacity
--
--
module Network.AWS.DynamoDB.CreateBackup
    (
    -- * Creating a request
      CreateBackup (..)
    , mkCreateBackup
    -- ** Request lenses
    , cbTableName
    , cbBackupName

    -- * Destructuring the response
    , CreateBackupResponse (..)
    , mkCreateBackupResponse
    -- ** Response lenses
    , cbrrsBackupDetails
    , cbrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { tableName :: Types.TableName
    -- ^ The name of the table.
  , backupName :: Types.BackupName
    -- ^ Specified name for the backup.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBackup' value with any optional fields omitted.
mkCreateBackup
    :: Types.TableName -- ^ 'tableName'
    -> Types.BackupName -- ^ 'backupName'
    -> CreateBackup
mkCreateBackup tableName backupName
  = CreateBackup'{tableName, backupName}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTableName :: Lens.Lens' CreateBackup Types.TableName
cbTableName = Lens.field @"tableName"
{-# INLINEABLE cbTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Specified name for the backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBackupName :: Lens.Lens' CreateBackup Types.BackupName
cbBackupName = Lens.field @"backupName"
{-# INLINEABLE cbBackupName #-}
{-# DEPRECATED backupName "Use generic-lens or generic-optics with 'backupName' instead"  #-}

instance Core.ToQuery CreateBackup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBackup where
        toHeaders CreateBackup{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.CreateBackup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON CreateBackup where
        toJSON CreateBackup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just ("BackupName" Core..= backupName)])

instance Core.AWSRequest CreateBackup where
        type Rs CreateBackup = CreateBackupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBackupResponse' Core.<$>
                   (x Core..:? "BackupDetails") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { backupDetails :: Core.Maybe Types.BackupDetails
    -- ^ Contains the details of the backup created for the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateBackupResponse' value with any optional fields omitted.
mkCreateBackupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBackupResponse
mkCreateBackupResponse responseStatus
  = CreateBackupResponse'{backupDetails = Core.Nothing,
                          responseStatus}

-- | Contains the details of the backup created for the table.
--
-- /Note:/ Consider using 'backupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsBackupDetails :: Lens.Lens' CreateBackupResponse (Core.Maybe Types.BackupDetails)
cbrrsBackupDetails = Lens.field @"backupDetails"
{-# INLINEABLE cbrrsBackupDetails #-}
{-# DEPRECATED backupDetails "Use generic-lens or generic-optics with 'backupDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBackupResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
