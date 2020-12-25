{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CompletedPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CompletedPart
  ( CompletedPart (..),

    -- * Smart constructor
    mkCompletedPart,

    -- * Lenses
    cpETag,
    cpPartNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Details of the parts that were uploaded.
--
-- /See:/ 'mkCompletedPart' smart constructor.
data CompletedPart = CompletedPart'
  { -- | Entity tag returned when the part was uploaded.
    eTag :: Types.ETag,
    -- | Part number that identifies the part. This is a positive integer between 1 and 10,000.
    partNumber :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompletedPart' value with any optional fields omitted.
mkCompletedPart ::
  -- | 'eTag'
  Types.ETag ->
  -- | 'partNumber'
  Core.Int ->
  CompletedPart
mkCompletedPart eTag partNumber = CompletedPart' {eTag, partNumber}

-- | Entity tag returned when the part was uploaded.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpETag :: Lens.Lens' CompletedPart Types.ETag
cpETag = Lens.field @"eTag"
{-# DEPRECATED cpETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Part number that identifies the part. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPartNumber :: Lens.Lens' CompletedPart Core.Int
cpPartNumber = Lens.field @"partNumber"
{-# DEPRECATED cpPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

instance Core.ToXML CompletedPart where
  toXML CompletedPart {..} =
    Core.toXMLNode "ETag" eTag
      Core.<> Core.toXMLNode "PartNumber" partNumber
