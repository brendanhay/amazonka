{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing backup of a table.
--
-- You can call @DescribeBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DescribeBackup
  ( -- * Creating a request
    DescribeBackup (..),
    mkDescribeBackup,

    -- ** Request lenses
    dBackupArn,

    -- * Destructuring the response
    DescribeBackupResponse (..),
    mkDescribeBackupResponse,

    -- ** Response lenses
    dbrfrsBackupDescription,
    dbrfrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBackup' smart constructor.
newtype DescribeBackup = DescribeBackup'
  { -- | The Amazon Resource Name (ARN) associated with the backup.
    backupArn :: Types.BackupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBackup' value with any optional fields omitted.
mkDescribeBackup ::
  -- | 'backupArn'
  Types.BackupArn ->
  DescribeBackup
mkDescribeBackup backupArn = DescribeBackup' {backupArn}

-- | The Amazon Resource Name (ARN) associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBackupArn :: Lens.Lens' DescribeBackup Types.BackupArn
dBackupArn = Lens.field @"backupArn"
{-# DEPRECATED dBackupArn "Use generic-lens or generic-optics with 'backupArn' instead." #-}

instance Core.FromJSON DescribeBackup where
  toJSON DescribeBackup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BackupArn" Core..= backupArn)])

instance Core.AWSRequest DescribeBackup where
  type Rs DescribeBackup = DescribeBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeBackup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupResponse'
            Core.<$> (x Core..:? "BackupDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeBackupResponse' smart constructor.
data DescribeBackupResponse = DescribeBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Core.Maybe Types.BackupDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBackupResponse' value with any optional fields omitted.
mkDescribeBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBackupResponse
mkDescribeBackupResponse responseStatus =
  DescribeBackupResponse'
    { backupDescription = Core.Nothing,
      responseStatus
    }

-- | Contains the description of the backup created for the table.
--
-- /Note:/ Consider using 'backupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsBackupDescription :: Lens.Lens' DescribeBackupResponse (Core.Maybe Types.BackupDescription)
dbrfrsBackupDescription = Lens.field @"backupDescription"
{-# DEPRECATED dbrfrsBackupDescription "Use generic-lens or generic-optics with 'backupDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsResponseStatus :: Lens.Lens' DescribeBackupResponse Core.Int
dbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
