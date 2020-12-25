{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CopyBackupToRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copy an AWS CloudHSM cluster backup to a different region.
module Network.AWS.CloudHSMv2.CopyBackupToRegion
  ( -- * Creating a request
    CopyBackupToRegion (..),
    mkCopyBackupToRegion,

    -- ** Request lenses
    cbtrDestinationRegion,
    cbtrBackupId,
    cbtrTagList,

    -- * Destructuring the response
    CopyBackupToRegionResponse (..),
    mkCopyBackupToRegionResponse,

    -- ** Response lenses
    cbtrrrsDestinationBackup,
    cbtrrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopyBackupToRegion' smart constructor.
data CopyBackupToRegion = CopyBackupToRegion'
  { -- | The AWS region that will contain your copied CloudHSM cluster backup.
    destinationRegion :: Types.Region,
    -- | The ID of the backup that will be copied to the destination region.
    backupId :: Types.BackupId,
    -- | Tags to apply to the destination backup during creation. If you specify tags, only these tags will be applied to the destination backup. If you do not specify tags, the service copies tags from the source backup to the destination backup.
    tagList :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyBackupToRegion' value with any optional fields omitted.
mkCopyBackupToRegion ::
  -- | 'destinationRegion'
  Types.Region ->
  -- | 'backupId'
  Types.BackupId ->
  CopyBackupToRegion
mkCopyBackupToRegion destinationRegion backupId =
  CopyBackupToRegion'
    { destinationRegion,
      backupId,
      tagList = Core.Nothing
    }

-- | The AWS region that will contain your copied CloudHSM cluster backup.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrDestinationRegion :: Lens.Lens' CopyBackupToRegion Types.Region
cbtrDestinationRegion = Lens.field @"destinationRegion"
{-# DEPRECATED cbtrDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The ID of the backup that will be copied to the destination region.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrBackupId :: Lens.Lens' CopyBackupToRegion Types.BackupId
cbtrBackupId = Lens.field @"backupId"
{-# DEPRECATED cbtrBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | Tags to apply to the destination backup during creation. If you specify tags, only these tags will be applied to the destination backup. If you do not specify tags, the service copies tags from the source backup to the destination backup.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrTagList :: Lens.Lens' CopyBackupToRegion (Core.Maybe [Types.Tag])
cbtrTagList = Lens.field @"tagList"
{-# DEPRECATED cbtrTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Core.FromJSON CopyBackupToRegion where
  toJSON CopyBackupToRegion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DestinationRegion" Core..= destinationRegion),
            Core.Just ("BackupId" Core..= backupId),
            ("TagList" Core..=) Core.<$> tagList
          ]
      )

instance Core.AWSRequest CopyBackupToRegion where
  type Rs CopyBackupToRegion = CopyBackupToRegionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "BaldrApiService.CopyBackupToRegion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyBackupToRegionResponse'
            Core.<$> (x Core..:? "DestinationBackup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyBackupToRegionResponse' smart constructor.
data CopyBackupToRegionResponse = CopyBackupToRegionResponse'
  { -- | Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup.
    --
    -- You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
    destinationBackup :: Core.Maybe Types.DestinationBackup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyBackupToRegionResponse' value with any optional fields omitted.
mkCopyBackupToRegionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyBackupToRegionResponse
mkCopyBackupToRegionResponse responseStatus =
  CopyBackupToRegionResponse'
    { destinationBackup = Core.Nothing,
      responseStatus
    }

-- | Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup.
--
-- You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
--
-- /Note:/ Consider using 'destinationBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrrsDestinationBackup :: Lens.Lens' CopyBackupToRegionResponse (Core.Maybe Types.DestinationBackup)
cbtrrrsDestinationBackup = Lens.field @"destinationBackup"
{-# DEPRECATED cbtrrrsDestinationBackup "Use generic-lens or generic-optics with 'destinationBackup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrrsResponseStatus :: Lens.Lens' CopyBackupToRegionResponse Core.Int
cbtrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbtrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
