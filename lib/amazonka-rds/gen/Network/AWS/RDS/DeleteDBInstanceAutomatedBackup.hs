{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes automated backups based on the source instance's @DbiResourceId@ value or the restorable instance's resource ID.
module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
  ( -- * Creating a request
    DeleteDBInstanceAutomatedBackup (..),
    mkDeleteDBInstanceAutomatedBackup,

    -- ** Request lenses
    ddbiabDbiResourceId,

    -- * Destructuring the response
    DeleteDBInstanceAutomatedBackupResponse (..),
    mkDeleteDBInstanceAutomatedBackupResponse,

    -- ** Response lenses
    ddbiabrrsDBInstanceAutomatedBackup,
    ddbiabrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameter input for the @DeleteDBInstanceAutomatedBackup@ operation.
--
-- /See:/ 'mkDeleteDBInstanceAutomatedBackup' smart constructor.
newtype DeleteDBInstanceAutomatedBackup = DeleteDBInstanceAutomatedBackup'
  { -- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
    dbiResourceId :: Types.DbiResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBInstanceAutomatedBackup' value with any optional fields omitted.
mkDeleteDBInstanceAutomatedBackup ::
  -- | 'dbiResourceId'
  Types.DbiResourceId ->
  DeleteDBInstanceAutomatedBackup
mkDeleteDBInstanceAutomatedBackup dbiResourceId =
  DeleteDBInstanceAutomatedBackup' {dbiResourceId}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabDbiResourceId :: Lens.Lens' DeleteDBInstanceAutomatedBackup Types.DbiResourceId
ddbiabDbiResourceId = Lens.field @"dbiResourceId"
{-# DEPRECATED ddbiabDbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

instance Core.AWSRequest DeleteDBInstanceAutomatedBackup where
  type
    Rs DeleteDBInstanceAutomatedBackup =
      DeleteDBInstanceAutomatedBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteDBInstanceAutomatedBackup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DbiResourceId" dbiResourceId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteDBInstanceAutomatedBackupResult"
      ( \s h x ->
          DeleteDBInstanceAutomatedBackupResponse'
            Core.<$> (x Core..@? "DBInstanceAutomatedBackup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDBInstanceAutomatedBackupResponse' smart constructor.
data DeleteDBInstanceAutomatedBackupResponse = DeleteDBInstanceAutomatedBackupResponse'
  { dBInstanceAutomatedBackup :: Core.Maybe Types.DBInstanceAutomatedBackup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDBInstanceAutomatedBackupResponse' value with any optional fields omitted.
mkDeleteDBInstanceAutomatedBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDBInstanceAutomatedBackupResponse
mkDeleteDBInstanceAutomatedBackupResponse responseStatus =
  DeleteDBInstanceAutomatedBackupResponse'
    { dBInstanceAutomatedBackup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstanceAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrrsDBInstanceAutomatedBackup :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse (Core.Maybe Types.DBInstanceAutomatedBackup)
ddbiabrrsDBInstanceAutomatedBackup = Lens.field @"dBInstanceAutomatedBackup"
{-# DEPRECATED ddbiabrrsDBInstanceAutomatedBackup "Use generic-lens or generic-optics with 'dBInstanceAutomatedBackup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrrsResponseStatus :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse Core.Int
ddbiabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbiabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
