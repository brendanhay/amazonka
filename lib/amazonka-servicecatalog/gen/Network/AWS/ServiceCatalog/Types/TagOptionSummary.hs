{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.TagOptionSummary
  ( TagOptionSummary (..),

    -- * Smart constructor
    mkTagOptionSummary,

    -- * Lenses
    tosKey,
    tosValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Key as Types
import qualified Network.AWS.ServiceCatalog.Types.TagOptionValue as Types

-- | Summary information about a TagOption.
--
-- /See:/ 'mkTagOptionSummary' smart constructor.
data TagOptionSummary = TagOptionSummary'
  { -- | The TagOption key.
    key :: Core.Maybe Types.Key,
    -- | The TagOption value.
    values :: Core.Maybe [Types.TagOptionValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagOptionSummary' value with any optional fields omitted.
mkTagOptionSummary ::
  TagOptionSummary
mkTagOptionSummary =
  TagOptionSummary' {key = Core.Nothing, values = Core.Nothing}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tosKey :: Lens.Lens' TagOptionSummary (Core.Maybe Types.Key)
tosKey = Lens.field @"key"
{-# DEPRECATED tosKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The TagOption value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tosValues :: Lens.Lens' TagOptionSummary (Core.Maybe [Types.TagOptionValue])
tosValues = Lens.field @"values"
{-# DEPRECATED tosValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON TagOptionSummary where
  parseJSON =
    Core.withObject "TagOptionSummary" Core.$
      \x ->
        TagOptionSummary'
          Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Values")
