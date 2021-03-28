{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.NFSFileShareDefaults
  ( NFSFileShareDefaults (..)
  -- * Smart constructor
  , mkNFSFileShareDefaults
  -- * Lenses
  , nfsfsdDirectoryMode
  , nfsfsdFileMode
  , nfsfsdGroupId
  , nfsfsdOwnerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.DirectoryMode as Types
import qualified Network.AWS.StorageGateway.Types.FileMode as Types

-- | Describes Network File System (NFS) file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported for file gateways.
--
-- /See:/ 'mkNFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { directoryMode :: Core.Maybe Types.DirectoryMode
    -- ^ The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
  , fileMode :: Core.Maybe Types.FileMode
    -- ^ The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
  , groupId :: Core.Maybe Core.Natural
    -- ^ The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
  , ownerId :: Core.Maybe Core.Natural
    -- ^ The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NFSFileShareDefaults' value with any optional fields omitted.
mkNFSFileShareDefaults
    :: NFSFileShareDefaults
mkNFSFileShareDefaults
  = NFSFileShareDefaults'{directoryMode = Core.Nothing,
                          fileMode = Core.Nothing, groupId = Core.Nothing,
                          ownerId = Core.Nothing}

-- | The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
--
-- /Note:/ Consider using 'directoryMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdDirectoryMode :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Types.DirectoryMode)
nfsfsdDirectoryMode = Lens.field @"directoryMode"
{-# INLINEABLE nfsfsdDirectoryMode #-}
{-# DEPRECATED directoryMode "Use generic-lens or generic-optics with 'directoryMode' instead"  #-}

-- | The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdFileMode :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Types.FileMode)
nfsfsdFileMode = Lens.field @"fileMode"
{-# INLINEABLE nfsfsdFileMode #-}
{-# DEPRECATED fileMode "Use generic-lens or generic-optics with 'fileMode' instead"  #-}

-- | The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdGroupId :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Natural)
nfsfsdGroupId = Lens.field @"groupId"
{-# INLINEABLE nfsfsdGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdOwnerId :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Natural)
nfsfsdOwnerId = Lens.field @"ownerId"
{-# INLINEABLE nfsfsdOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

instance Core.FromJSON NFSFileShareDefaults where
        toJSON NFSFileShareDefaults{..}
          = Core.object
              (Core.catMaybes
                 [("DirectoryMode" Core..=) Core.<$> directoryMode,
                  ("FileMode" Core..=) Core.<$> fileMode,
                  ("GroupId" Core..=) Core.<$> groupId,
                  ("OwnerId" Core..=) Core.<$> ownerId])

instance Core.FromJSON NFSFileShareDefaults where
        parseJSON
          = Core.withObject "NFSFileShareDefaults" Core.$
              \ x ->
                NFSFileShareDefaults' Core.<$>
                  (x Core..:? "DirectoryMode") Core.<*> x Core..:? "FileMode"
                    Core.<*> x Core..:? "GroupId"
                    Core.<*> x Core..:? "OwnerId"
