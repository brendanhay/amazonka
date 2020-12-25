{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup policy for the specified EFS file system.
module Network.AWS.EFS.DescribeBackupPolicy
  ( -- * Creating a request
    DescribeBackupPolicy (..),
    mkDescribeBackupPolicy,

    -- ** Request lenses
    dbpFileSystemId,

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

-- | /See:/ 'mkDescribeBackupPolicy' smart constructor.
newtype DescribeBackupPolicy = DescribeBackupPolicy'
  { -- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
    fileSystemId :: Types.FileSystemId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBackupPolicy' value with any optional fields omitted.
mkDescribeBackupPolicy ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  DescribeBackupPolicy
mkDescribeBackupPolicy fileSystemId =
  DescribeBackupPolicy' {fileSystemId}

-- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpFileSystemId :: Lens.Lens' DescribeBackupPolicy Types.FileSystemId
dbpFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED dbpFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Core.AWSRequest DescribeBackupPolicy where
  type Rs DescribeBackupPolicy = Types.BackupPolicyDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-02-01/file-systems/" Core.<> (Core.toText fileSystemId)
                Core.<> ("/backup-policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
