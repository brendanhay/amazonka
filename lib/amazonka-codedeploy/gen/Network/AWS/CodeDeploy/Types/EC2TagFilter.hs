{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilter
  ( EC2TagFilter (..),

    -- * Smart constructor
    mkEC2TagFilter,

    -- * Lenses
    ectfKey,
    ectfType,
    ectfValue,
  )
where

import qualified Network.AWS.CodeDeploy.Types.EC2TagFilterType as Types
import qualified Network.AWS.CodeDeploy.Types.Key as Types
import qualified Network.AWS.CodeDeploy.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an EC2 tag filter.
--
-- /See:/ 'mkEC2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { -- | The tag filter key.
    key :: Core.Maybe Types.Key,
    -- | The tag filter type:
    --
    --
    --     * @KEY_ONLY@ : Key only.
    --
    --
    --     * @VALUE_ONLY@ : Value only.
    --
    --
    --     * @KEY_AND_VALUE@ : Key and value.
    type' :: Core.Maybe Types.EC2TagFilterType,
    -- | The tag filter value.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2TagFilter' value with any optional fields omitted.
mkEC2TagFilter ::
  EC2TagFilter
mkEC2TagFilter =
  EC2TagFilter'
    { key = Core.Nothing,
      type' = Core.Nothing,
      value = Core.Nothing
    }

-- | The tag filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectfKey :: Lens.Lens' EC2TagFilter (Core.Maybe Types.Key)
ectfKey = Lens.field @"key"
{-# DEPRECATED ectfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The tag filter type:
--
--
--     * @KEY_ONLY@ : Key only.
--
--
--     * @VALUE_ONLY@ : Value only.
--
--
--     * @KEY_AND_VALUE@ : Key and value.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectfType :: Lens.Lens' EC2TagFilter (Core.Maybe Types.EC2TagFilterType)
ectfType = Lens.field @"type'"
{-# DEPRECATED ectfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The tag filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectfValue :: Lens.Lens' EC2TagFilter (Core.Maybe Types.Value)
ectfValue = Lens.field @"value"
{-# DEPRECATED ectfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON EC2TagFilter where
  toJSON EC2TagFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Type" Core..=) Core.<$> type',
            ("Value" Core..=) Core.<$> value
          ]
      )

instance Core.FromJSON EC2TagFilter where
  parseJSON =
    Core.withObject "EC2TagFilter" Core.$
      \x ->
        EC2TagFilter'
          Core.<$> (x Core..:? "Key")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "Value")
