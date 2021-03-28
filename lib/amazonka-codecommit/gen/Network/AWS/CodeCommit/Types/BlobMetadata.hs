{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BlobMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.BlobMetadata
  ( BlobMetadata (..)
  -- * Smart constructor
  , mkBlobMetadata
  -- * Lenses
  , bmBlobId
  , bmMode
  , bmPath
  ) where

import qualified Network.AWS.CodeCommit.Types.Mode as Types
import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a specific Git blob object.
--
-- /See:/ 'mkBlobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { blobId :: Core.Maybe Types.ObjectId
    -- ^ The full ID of the blob.
  , mode :: Core.Maybe Types.Mode
    -- ^ The file mode permissions of the blob. File mode permission codes include:
--
--
--     * @100644@ indicates read/write
--
--
--     * @100755@ indicates read/write/execute
--
--
--     * @160000@ indicates a submodule
--
--
--     * @120000@ indicates a symlink
--
--
  , path :: Core.Maybe Types.Path
    -- ^ The path to the blob and associated file name, if any.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlobMetadata' value with any optional fields omitted.
mkBlobMetadata
    :: BlobMetadata
mkBlobMetadata
  = BlobMetadata'{blobId = Core.Nothing, mode = Core.Nothing,
                  path = Core.Nothing}

-- | The full ID of the blob.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmBlobId :: Lens.Lens' BlobMetadata (Core.Maybe Types.ObjectId)
bmBlobId = Lens.field @"blobId"
{-# INLINEABLE bmBlobId #-}
{-# DEPRECATED blobId "Use generic-lens or generic-optics with 'blobId' instead"  #-}

-- | The file mode permissions of the blob. File mode permission codes include:
--
--
--     * @100644@ indicates read/write
--
--
--     * @100755@ indicates read/write/execute
--
--
--     * @160000@ indicates a submodule
--
--
--     * @120000@ indicates a symlink
--
--
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmMode :: Lens.Lens' BlobMetadata (Core.Maybe Types.Mode)
bmMode = Lens.field @"mode"
{-# INLINEABLE bmMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The path to the blob and associated file name, if any.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmPath :: Lens.Lens' BlobMetadata (Core.Maybe Types.Path)
bmPath = Lens.field @"path"
{-# INLINEABLE bmPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromJSON BlobMetadata where
        parseJSON
          = Core.withObject "BlobMetadata" Core.$
              \ x ->
                BlobMetadata' Core.<$>
                  (x Core..:? "blobId") Core.<*> x Core..:? "mode" Core.<*>
                    x Core..:? "path"
