{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyObjectResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyObjectResult
  ( CopyObjectResult (..),

    -- * Smart constructor
    mkCopyObjectResult,

    -- * Lenses
    corETag,
    corLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for all response elements.
--
-- /See:/ 'mkCopyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { -- | Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
    eTag :: Core.Maybe Types.ETag,
    -- | Returns the date that the object was last modified.
    lastModified :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyObjectResult' value with any optional fields omitted.
mkCopyObjectResult ::
  CopyObjectResult
mkCopyObjectResult =
  CopyObjectResult'
    { eTag = Core.Nothing,
      lastModified = Core.Nothing
    }

-- | Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corETag :: Lens.Lens' CopyObjectResult (Core.Maybe Types.ETag)
corETag = Lens.field @"eTag"
{-# DEPRECATED corETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Returns the date that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corLastModified :: Lens.Lens' CopyObjectResult (Core.Maybe Core.UTCTime)
corLastModified = Lens.field @"lastModified"
{-# DEPRECATED corLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Core.FromXML CopyObjectResult where
  parseXML x =
    CopyObjectResult'
      Core.<$> (x Core..@? "ETag") Core.<*> (x Core..@? "LastModified")
