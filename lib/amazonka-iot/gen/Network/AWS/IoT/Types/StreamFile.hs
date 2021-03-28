{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.StreamFile
  ( StreamFile (..)
  -- * Smart constructor
  , mkStreamFile
  -- * Lenses
  , sfFileId
  , sfS3Location
  ) where

import qualified Network.AWS.IoT.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a file to stream.
--
-- /See:/ 'mkStreamFile' smart constructor.
data StreamFile = StreamFile'
  { fileId :: Core.Maybe Core.Natural
    -- ^ The file ID.
  , s3Location :: Core.Maybe Types.S3Location
    -- ^ The location of the file in S3.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamFile' value with any optional fields omitted.
mkStreamFile
    :: StreamFile
mkStreamFile
  = StreamFile'{fileId = Core.Nothing, s3Location = Core.Nothing}

-- | The file ID.
--
-- /Note:/ Consider using 'fileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFileId :: Lens.Lens' StreamFile (Core.Maybe Core.Natural)
sfFileId = Lens.field @"fileId"
{-# INLINEABLE sfFileId #-}
{-# DEPRECATED fileId "Use generic-lens or generic-optics with 'fileId' instead"  #-}

-- | The location of the file in S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfS3Location :: Lens.Lens' StreamFile (Core.Maybe Types.S3Location)
sfS3Location = Lens.field @"s3Location"
{-# INLINEABLE sfS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

instance Core.FromJSON StreamFile where
        toJSON StreamFile{..}
          = Core.object
              (Core.catMaybes
                 [("fileId" Core..=) Core.<$> fileId,
                  ("s3Location" Core..=) Core.<$> s3Location])

instance Core.FromJSON StreamFile where
        parseJSON
          = Core.withObject "StreamFile" Core.$
              \ x ->
                StreamFile' Core.<$>
                  (x Core..:? "fileId") Core.<*> x Core..:? "s3Location"
