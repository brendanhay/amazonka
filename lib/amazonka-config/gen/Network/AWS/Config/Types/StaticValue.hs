{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.StaticValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StaticValue
  ( StaticValue (..),

    -- * Smart constructor
    mkStaticValue,

    -- * Lenses
    svValues,
  )
where

import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The static value of the resource.
--
-- /See:/ 'mkStaticValue' smart constructor.
newtype StaticValue = StaticValue'
  { -- | A list of values. For example, the ARN of the assumed role.
    values :: [Types.StringWithCharLimit256]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StaticValue' value with any optional fields omitted.
mkStaticValue ::
  StaticValue
mkStaticValue = StaticValue' {values = Core.mempty}

-- | A list of values. For example, the ARN of the assumed role.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svValues :: Lens.Lens' StaticValue [Types.StringWithCharLimit256]
svValues = Lens.field @"values"
{-# DEPRECATED svValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON StaticValue where
  toJSON StaticValue {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Values" Core..= values)])

instance Core.FromJSON StaticValue where
  parseJSON =
    Core.withObject "StaticValue" Core.$
      \x ->
        StaticValue' Core.<$> (x Core..:? "Values" Core..!= Core.mempty)
