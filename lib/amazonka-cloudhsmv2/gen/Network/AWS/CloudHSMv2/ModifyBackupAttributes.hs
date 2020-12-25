{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ModifyBackupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes for AWS CloudHSM backup.
module Network.AWS.CloudHSMv2.ModifyBackupAttributes
  ( -- * Creating a request
    ModifyBackupAttributes (..),
    mkModifyBackupAttributes,

    -- ** Request lenses
    mbaBackupId,
    mbaNeverExpires,

    -- * Destructuring the response
    ModifyBackupAttributesResponse (..),
    mkModifyBackupAttributesResponse,

    -- ** Response lenses
    mbarrsBackup,
    mbarrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyBackupAttributes' smart constructor.
data ModifyBackupAttributes = ModifyBackupAttributes'
  { -- | The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
    backupId :: Types.BackupId,
    -- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
    neverExpires :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyBackupAttributes' value with any optional fields omitted.
mkModifyBackupAttributes ::
  -- | 'backupId'
  Types.BackupId ->
  -- | 'neverExpires'
  Core.Bool ->
  ModifyBackupAttributes
mkModifyBackupAttributes backupId neverExpires =
  ModifyBackupAttributes' {backupId, neverExpires}

-- | The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbaBackupId :: Lens.Lens' ModifyBackupAttributes Types.BackupId
mbaBackupId = Lens.field @"backupId"
{-# DEPRECATED mbaBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'neverExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbaNeverExpires :: Lens.Lens' ModifyBackupAttributes Core.Bool
mbaNeverExpires = Lens.field @"neverExpires"
{-# DEPRECATED mbaNeverExpires "Use generic-lens or generic-optics with 'neverExpires' instead." #-}

instance Core.FromJSON ModifyBackupAttributes where
  toJSON ModifyBackupAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BackupId" Core..= backupId),
            Core.Just ("NeverExpires" Core..= neverExpires)
          ]
      )

instance Core.AWSRequest ModifyBackupAttributes where
  type Rs ModifyBackupAttributes = ModifyBackupAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "BaldrApiService.ModifyBackupAttributes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyBackupAttributesResponse'
            Core.<$> (x Core..:? "Backup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyBackupAttributesResponse' smart constructor.
data ModifyBackupAttributesResponse = ModifyBackupAttributesResponse'
  { backup :: Core.Maybe Types.Backup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyBackupAttributesResponse' value with any optional fields omitted.
mkModifyBackupAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyBackupAttributesResponse
mkModifyBackupAttributesResponse responseStatus =
  ModifyBackupAttributesResponse'
    { backup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbarrsBackup :: Lens.Lens' ModifyBackupAttributesResponse (Core.Maybe Types.Backup)
mbarrsBackup = Lens.field @"backup"
{-# DEPRECATED mbarrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbarrsResponseStatus :: Lens.Lens' ModifyBackupAttributesResponse Core.Int
mbarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mbarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
