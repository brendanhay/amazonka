{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityType
  ( ActivityType (..),

    -- * Smart constructor
    mkActivityType,

    -- * Lenses
    atName,
    atVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Name as Types
import qualified Network.AWS.SWF.Types.Version as Types

-- | Represents an activity type.
--
-- /See:/ 'mkActivityType' smart constructor.
data ActivityType = ActivityType'
  { -- | The name of this activity.
    name :: Types.Name,
    -- | The version of this activity.
    version :: Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityType' value with any optional fields omitted.
mkActivityType ::
  -- | 'name'
  Types.Name ->
  -- | 'version'
  Types.Version ->
  ActivityType
mkActivityType name version = ActivityType' {name, version}

-- | The name of this activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' ActivityType Types.Name
atName = Lens.field @"name"
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of this activity.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atVersion :: Lens.Lens' ActivityType Types.Version
atVersion = Lens.field @"version"
{-# DEPRECATED atVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON ActivityType where
  toJSON ActivityType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("version" Core..= version)
          ]
      )

instance Core.FromJSON ActivityType where
  parseJSON =
    Core.withObject "ActivityType" Core.$
      \x ->
        ActivityType'
          Core.<$> (x Core..: "name") Core.<*> (x Core..: "version")
