{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing backup of a table.
--
-- You can call @DeleteBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DeleteBackup
    (
    -- * Creating a request
      DeleteBackup (..)
    , mkDeleteBackup
    -- ** Request lenses
    , dbBackupArn

    -- * Destructuring the response
    , DeleteBackupResponse (..)
    , mkDeleteBackupResponse
    -- ** Response lenses
    , dbrrsBackupDescription
    , dbrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup'
  { backupArn :: Types.BackupArn
    -- ^ The ARN associated with the backup.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBackup' value with any optional fields omitted.
mkDeleteBackup
    :: Types.BackupArn -- ^ 'backupArn'
    -> DeleteBackup
mkDeleteBackup backupArn = DeleteBackup'{backupArn}

-- | The ARN associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupArn :: Lens.Lens' DeleteBackup Types.BackupArn
dbBackupArn = Lens.field @"backupArn"
{-# INLINEABLE dbBackupArn #-}
{-# DEPRECATED backupArn "Use generic-lens or generic-optics with 'backupArn' instead"  #-}

instance Core.ToQuery DeleteBackup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBackup where
        toHeaders DeleteBackup{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.DeleteBackup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DeleteBackup where
        toJSON DeleteBackup{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BackupArn" Core..= backupArn)])

instance Core.AWSRequest DeleteBackup where
        type Rs DeleteBackup = DeleteBackupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBackupResponse' Core.<$>
                   (x Core..:? "BackupDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { backupDescription :: Core.Maybe Types.BackupDescription
    -- ^ Contains the description of the backup created for the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteBackupResponse' value with any optional fields omitted.
mkDeleteBackupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBackupResponse
mkDeleteBackupResponse responseStatus
  = DeleteBackupResponse'{backupDescription = Core.Nothing,
                          responseStatus}

-- | Contains the description of the backup created for the table.
--
-- /Note:/ Consider using 'backupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBackupDescription :: Lens.Lens' DeleteBackupResponse (Core.Maybe Types.BackupDescription)
dbrrsBackupDescription = Lens.field @"backupDescription"
{-# INLINEABLE dbrrsBackupDescription #-}
{-# DEPRECATED backupDescription "Use generic-lens or generic-optics with 'backupDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DeleteBackupResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
