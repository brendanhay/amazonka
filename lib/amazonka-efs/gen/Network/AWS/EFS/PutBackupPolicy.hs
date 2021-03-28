{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file system's backup policy. Use this action to start or stop automatic backups of the file system. 
module Network.AWS.EFS.PutBackupPolicy
    (
    -- * Creating a request
      PutBackupPolicy (..)
    , mkPutBackupPolicy
    -- ** Request lenses
    , pbpFileSystemId
    , pbpBackupPolicy

     -- * Destructuring the response
    , Types.BackupPolicyDescription (..)
    , Types.mkBackupPolicyDescription
    -- ** Response lenses
    , Types.bpdBackupPolicy
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { fileSystemId :: Types.FileSystemId
    -- ^ Specifies which EFS file system to update the backup policy for.
  , backupPolicy :: Types.BackupPolicy
    -- ^ The backup policy included in the @PutBackupPolicy@ request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBackupPolicy' value with any optional fields omitted.
mkPutBackupPolicy
    :: Types.FileSystemId -- ^ 'fileSystemId'
    -> Types.BackupPolicy -- ^ 'backupPolicy'
    -> PutBackupPolicy
mkPutBackupPolicy fileSystemId backupPolicy
  = PutBackupPolicy'{fileSystemId, backupPolicy}

-- | Specifies which EFS file system to update the backup policy for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpFileSystemId :: Lens.Lens' PutBackupPolicy Types.FileSystemId
pbpFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE pbpFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | The backup policy included in the @PutBackupPolicy@ request.
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpBackupPolicy :: Lens.Lens' PutBackupPolicy Types.BackupPolicy
pbpBackupPolicy = Lens.field @"backupPolicy"
{-# INLINEABLE pbpBackupPolicy #-}
{-# DEPRECATED backupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead"  #-}

instance Core.ToQuery PutBackupPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutBackupPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PutBackupPolicy where
        toJSON PutBackupPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BackupPolicy" Core..= backupPolicy)])

instance Core.AWSRequest PutBackupPolicy where
        type Rs PutBackupPolicy = Types.BackupPolicyDescription
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-02-01/file-systems/" Core.<> Core.toText fileSystemId
                             Core.<> "/backup-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
