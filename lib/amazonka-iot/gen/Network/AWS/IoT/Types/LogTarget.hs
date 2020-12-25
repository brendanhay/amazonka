{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTarget
  ( LogTarget (..),

    -- * Smart constructor
    mkLogTarget,

    -- * Lenses
    ltTargetType,
    ltTargetName,
  )
where

import qualified Network.AWS.IoT.Types.LogTargetName as Types
import qualified Network.AWS.IoT.Types.LogTargetType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A log target.
--
-- /See:/ 'mkLogTarget' smart constructor.
data LogTarget = LogTarget'
  { -- | The target type.
    targetType :: Types.LogTargetType,
    -- | The target name.
    targetName :: Core.Maybe Types.LogTargetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogTarget' value with any optional fields omitted.
mkLogTarget ::
  -- | 'targetType'
  Types.LogTargetType ->
  LogTarget
mkLogTarget targetType =
  LogTarget' {targetType, targetName = Core.Nothing}

-- | The target type.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetType :: Lens.Lens' LogTarget Types.LogTargetType
ltTargetType = Lens.field @"targetType"
{-# DEPRECATED ltTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetName :: Lens.Lens' LogTarget (Core.Maybe Types.LogTargetName)
ltTargetName = Lens.field @"targetName"
{-# DEPRECATED ltTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

instance Core.FromJSON LogTarget where
  toJSON LogTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targetType" Core..= targetType),
            ("targetName" Core..=) Core.<$> targetName
          ]
      )

instance Core.FromJSON LogTarget where
  parseJSON =
    Core.withObject "LogTarget" Core.$
      \x ->
        LogTarget'
          Core.<$> (x Core..: "targetType") Core.<*> (x Core..:? "targetName")
