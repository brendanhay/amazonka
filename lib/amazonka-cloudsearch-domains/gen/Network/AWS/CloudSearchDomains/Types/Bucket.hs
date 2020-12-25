{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Bucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Bucket
  ( Bucket (..),

    -- * Smart constructor
    mkBucket,

    -- * Lenses
    bCount,
    bValue,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for facet information.
--
-- /See:/ 'mkBucket' smart constructor.
data Bucket = Bucket'
  { -- | The number of hits that contain the facet value in the specified facet field.
    count :: Core.Maybe Core.Integer,
    -- | The facet value being counted.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Bucket' value with any optional fields omitted.
mkBucket ::
  Bucket
mkBucket = Bucket' {count = Core.Nothing, value = Core.Nothing}

-- | The number of hits that contain the facet value in the specified facet field.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCount :: Lens.Lens' Bucket (Core.Maybe Core.Integer)
bCount = Lens.field @"count"
{-# DEPRECATED bCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The facet value being counted.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Bucket (Core.Maybe Types.String)
bValue = Lens.field @"value"
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Bucket where
  parseJSON =
    Core.withObject "Bucket" Core.$
      \x ->
        Bucket' Core.<$> (x Core..:? "count") Core.<*> (x Core..:? "value")
