{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ArchiveCreationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.ArchiveCreationOutput
  ( ArchiveCreationOutput (..)
  -- * Smart constructor
  , mkArchiveCreationOutput
  -- * Lenses
  , acoArchiveId
  , acoChecksum
  , acoLocation
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the Amazon S3 Glacier response to your request.
--
-- For information about the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> . For conceptual information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> .
--
-- /See:/ 'mkArchiveCreationOutput' smart constructor.
data ArchiveCreationOutput = ArchiveCreationOutput'
  { archiveId :: Core.Maybe Core.Text
    -- ^ The ID of the archive. This value is also included as part of the location.
  , checksum :: Core.Maybe Core.Text
    -- ^ The checksum of the archive computed by Amazon S3 Glacier.
  , location :: Core.Maybe Core.Text
    -- ^ The relative URI path of the newly added archive resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveCreationOutput' value with any optional fields omitted.
mkArchiveCreationOutput
    :: ArchiveCreationOutput
mkArchiveCreationOutput
  = ArchiveCreationOutput'{archiveId = Core.Nothing,
                           checksum = Core.Nothing, location = Core.Nothing}

-- | The ID of the archive. This value is also included as part of the location.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoArchiveId :: Lens.Lens' ArchiveCreationOutput (Core.Maybe Core.Text)
acoArchiveId = Lens.field @"archiveId"
{-# INLINEABLE acoArchiveId #-}
{-# DEPRECATED archiveId "Use generic-lens or generic-optics with 'archiveId' instead"  #-}

-- | The checksum of the archive computed by Amazon S3 Glacier.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoChecksum :: Lens.Lens' ArchiveCreationOutput (Core.Maybe Core.Text)
acoChecksum = Lens.field @"checksum"
{-# INLINEABLE acoChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | The relative URI path of the newly added archive resource.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoLocation :: Lens.Lens' ArchiveCreationOutput (Core.Maybe Core.Text)
acoLocation = Lens.field @"location"
{-# INLINEABLE acoLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.FromJSON ArchiveCreationOutput where
        parseJSON
          = Core.withObject "ArchiveCreationOutput" Core.$
              \ x ->
                ArchiveCreationOutput' Core.<$>
                  (x Core..:? "x-amz-archive-id") Core.<*>
                    x Core..:? "x-amz-sha256-tree-hash"
                    Core.<*> x Core..:? "Location"
