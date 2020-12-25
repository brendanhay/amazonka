{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutBackupPolicy (..),
    mkPutBackupPolicy,

    -- ** Request lenses
    pbpFileSystemId,
    pbpBackupPolicy,

    -- * Destructuring the response
    Types.BackupPolicyDescription (..),
    Types.mkBackupPolicyDescription,

    -- ** Response lenses
    Types.bpdBackupPolicy,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { -- | Specifies which EFS file system to update the backup policy for.
    fileSystemId :: Types.FileSystemId,
    -- | The backup policy included in the @PutBackupPolicy@ request.
    backupPolicy :: Types.BackupPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBackupPolicy' value with any optional fields omitted.
mkPutBackupPolicy ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  -- | 'backupPolicy'
  Types.BackupPolicy ->
  PutBackupPolicy
mkPutBackupPolicy fileSystemId backupPolicy =
  PutBackupPolicy' {fileSystemId, backupPolicy}

-- | Specifies which EFS file system to update the backup policy for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpFileSystemId :: Lens.Lens' PutBackupPolicy Types.FileSystemId
pbpFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED pbpFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The backup policy included in the @PutBackupPolicy@ request.
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpBackupPolicy :: Lens.Lens' PutBackupPolicy Types.BackupPolicy
pbpBackupPolicy = Lens.field @"backupPolicy"
{-# DEPRECATED pbpBackupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead." #-}

instance Core.FromJSON PutBackupPolicy where
  toJSON PutBackupPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BackupPolicy" Core..= backupPolicy)])

instance Core.AWSRequest PutBackupPolicy where
  type Rs PutBackupPolicy = Types.BackupPolicyDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2015-02-01/file-systems/" Core.<> (Core.toText fileSystemId)
                Core.<> ("/backup-policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
