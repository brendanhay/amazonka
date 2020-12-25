{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SharedDirectory
  ( SharedDirectory (..),

    -- * Smart constructor
    mkSharedDirectory,

    -- * Lenses
    sdCreatedDateTime,
    sdLastUpdatedDateTime,
    sdOwnerAccountId,
    sdOwnerDirectoryId,
    sdShareMethod,
    sdShareNotes,
    sdShareStatus,
    sdSharedAccountId,
    sdSharedDirectoryId,
  )
where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.OwnerAccountId as Types
import qualified Network.AWS.DirectoryService.Types.ShareMethod as Types
import qualified Network.AWS.DirectoryService.Types.ShareNotes as Types
import qualified Network.AWS.DirectoryService.Types.ShareStatus as Types
import qualified Network.AWS.DirectoryService.Types.SharedAccountId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the shared directory in the directory owner account for which the share request in the directory consumer account has been accepted.
--
-- /See:/ 'mkSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | The date and time that the shared directory was created.
    createdDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
    ownerAccountId :: Core.Maybe Types.OwnerAccountId,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Core.Maybe Types.DirectoryId,
    -- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
    shareMethod :: Core.Maybe Types.ShareMethod,
    -- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
    shareNotes :: Core.Maybe Types.ShareNotes,
    -- | Current directory status of the shared AWS Managed Microsoft AD directory.
    shareStatus :: Core.Maybe Types.ShareStatus,
    -- | Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
    sharedAccountId :: Core.Maybe Types.SharedAccountId,
    -- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
    sharedDirectoryId :: Core.Maybe Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SharedDirectory' value with any optional fields omitted.
mkSharedDirectory ::
  SharedDirectory
mkSharedDirectory =
  SharedDirectory'
    { createdDateTime = Core.Nothing,
      lastUpdatedDateTime = Core.Nothing,
      ownerAccountId = Core.Nothing,
      ownerDirectoryId = Core.Nothing,
      shareMethod = Core.Nothing,
      shareNotes = Core.Nothing,
      shareStatus = Core.Nothing,
      sharedAccountId = Core.Nothing,
      sharedDirectoryId = Core.Nothing
    }

-- | The date and time that the shared directory was created.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCreatedDateTime :: Lens.Lens' SharedDirectory (Core.Maybe Core.NominalDiffTime)
sdCreatedDateTime = Lens.field @"createdDateTime"
{-# DEPRECATED sdCreatedDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead." #-}

-- | The date and time that the shared directory was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLastUpdatedDateTime :: Lens.Lens' SharedDirectory (Core.Maybe Core.NominalDiffTime)
sdLastUpdatedDateTime = Lens.field @"lastUpdatedDateTime"
{-# DEPRECATED sdLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwnerAccountId :: Lens.Lens' SharedDirectory (Core.Maybe Types.OwnerAccountId)
sdOwnerAccountId = Lens.field @"ownerAccountId"
{-# DEPRECATED sdOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | Identifier of the directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwnerDirectoryId :: Lens.Lens' SharedDirectory (Core.Maybe Types.DirectoryId)
sdOwnerDirectoryId = Lens.field @"ownerDirectoryId"
{-# DEPRECATED sdOwnerDirectoryId "Use generic-lens or generic-optics with 'ownerDirectoryId' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareMethod :: Lens.Lens' SharedDirectory (Core.Maybe Types.ShareMethod)
sdShareMethod = Lens.field @"shareMethod"
{-# DEPRECATED sdShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareNotes :: Lens.Lens' SharedDirectory (Core.Maybe Types.ShareNotes)
sdShareNotes = Lens.field @"shareNotes"
{-# DEPRECATED sdShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'shareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareStatus :: Lens.Lens' SharedDirectory (Core.Maybe Types.ShareStatus)
sdShareStatus = Lens.field @"shareStatus"
{-# DEPRECATED sdShareStatus "Use generic-lens or generic-optics with 'shareStatus' instead." #-}

-- | Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSharedAccountId :: Lens.Lens' SharedDirectory (Core.Maybe Types.SharedAccountId)
sdSharedAccountId = Lens.field @"sharedAccountId"
{-# DEPRECATED sdSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSharedDirectoryId :: Lens.Lens' SharedDirectory (Core.Maybe Types.DirectoryId)
sdSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED sdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

instance Core.FromJSON SharedDirectory where
  parseJSON =
    Core.withObject "SharedDirectory" Core.$
      \x ->
        SharedDirectory'
          Core.<$> (x Core..:? "CreatedDateTime")
          Core.<*> (x Core..:? "LastUpdatedDateTime")
          Core.<*> (x Core..:? "OwnerAccountId")
          Core.<*> (x Core..:? "OwnerDirectoryId")
          Core.<*> (x Core..:? "ShareMethod")
          Core.<*> (x Core..:? "ShareNotes")
          Core.<*> (x Core..:? "ShareStatus")
          Core.<*> (x Core..:? "SharedAccountId")
          Core.<*> (x Core..:? "SharedDirectoryId")
