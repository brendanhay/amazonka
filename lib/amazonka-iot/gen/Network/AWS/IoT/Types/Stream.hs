{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Stream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Stream
  ( Stream (..)
  -- * Smart constructor
  , mkStream
  -- * Lenses
  , sFileId
  , sStreamId
  ) where

import qualified Network.AWS.IoT.Types.StreamId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a group of files that can be streamed.
--
-- /See:/ 'mkStream' smart constructor.
data Stream = Stream'
  { fileId :: Core.Maybe Core.Natural
    -- ^ The ID of a file associated with a stream.
  , streamId :: Core.Maybe Types.StreamId
    -- ^ The stream ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Stream' value with any optional fields omitted.
mkStream
    :: Stream
mkStream = Stream'{fileId = Core.Nothing, streamId = Core.Nothing}

-- | The ID of a file associated with a stream.
--
-- /Note:/ Consider using 'fileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFileId :: Lens.Lens' Stream (Core.Maybe Core.Natural)
sFileId = Lens.field @"fileId"
{-# INLINEABLE sFileId #-}
{-# DEPRECATED fileId "Use generic-lens or generic-optics with 'fileId' instead"  #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamId :: Lens.Lens' Stream (Core.Maybe Types.StreamId)
sStreamId = Lens.field @"streamId"
{-# INLINEABLE sStreamId #-}
{-# DEPRECATED streamId "Use generic-lens or generic-optics with 'streamId' instead"  #-}

instance Core.FromJSON Stream where
        toJSON Stream{..}
          = Core.object
              (Core.catMaybes
                 [("fileId" Core..=) Core.<$> fileId,
                  ("streamId" Core..=) Core.<$> streamId])

instance Core.FromJSON Stream where
        parseJSON
          = Core.withObject "Stream" Core.$
              \ x ->
                Stream' Core.<$>
                  (x Core..:? "fileId") Core.<*> x Core..:? "streamId"
