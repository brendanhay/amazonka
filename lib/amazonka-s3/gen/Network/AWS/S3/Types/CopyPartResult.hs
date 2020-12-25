{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyPartResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyPartResult
  ( CopyPartResult (..),

    -- * Smart constructor
    mkCopyPartResult,

    -- * Lenses
    cprETag,
    cprLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for all response elements.
--
-- /See:/ 'mkCopyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { -- | Entity tag of the object.
    eTag :: Core.Maybe Types.ETag,
    -- | Date and time at which the object was uploaded.
    lastModified :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyPartResult' value with any optional fields omitted.
mkCopyPartResult ::
  CopyPartResult
mkCopyPartResult =
  CopyPartResult' {eTag = Core.Nothing, lastModified = Core.Nothing}

-- | Entity tag of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprETag :: Lens.Lens' CopyPartResult (Core.Maybe Types.ETag)
cprETag = Lens.field @"eTag"
{-# DEPRECATED cprETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Date and time at which the object was uploaded.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprLastModified :: Lens.Lens' CopyPartResult (Core.Maybe Core.UTCTime)
cprLastModified = Lens.field @"lastModified"
{-# DEPRECATED cprLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Core.FromXML CopyPartResult where
  parseXML x =
    CopyPartResult'
      Core.<$> (x Core..@? "ETag") Core.<*> (x Core..@? "LastModified")
