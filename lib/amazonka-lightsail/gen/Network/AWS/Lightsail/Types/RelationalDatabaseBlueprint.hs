{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
  ( RelationalDatabaseBlueprint (..),

    -- * Smart constructor
    mkRelationalDatabaseBlueprint,

    -- * Lenses
    rdbBlueprintId,
    rdbEngine,
    rdbEngineDescription,
    rdbEngineVersion,
    rdbEngineVersionDescription,
    rdbIsEngineDefault,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.RelationalDatabaseEngine as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a database image, or blueprint. A blueprint describes the major engine version of a database.
--
-- /See:/ 'mkRelationalDatabaseBlueprint' smart constructor.
data RelationalDatabaseBlueprint = RelationalDatabaseBlueprint'
  { -- | The ID for the database blueprint.
    blueprintId :: Core.Maybe Types.String,
    -- | The database software of the database blueprint (for example, @MySQL@ ).
    engine :: Core.Maybe Types.RelationalDatabaseEngine,
    -- | The description of the database engine for the database blueprint.
    engineDescription :: Core.Maybe Types.String,
    -- | The database engine version for the database blueprint (for example, @5.7.23@ ).
    engineVersion :: Core.Maybe Types.String,
    -- | The description of the database engine version for the database blueprint.
    engineVersionDescription :: Core.Maybe Types.String,
    -- | A Boolean value indicating whether the engine version is the default for the database blueprint.
    isEngineDefault :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseBlueprint' value with any optional fields omitted.
mkRelationalDatabaseBlueprint ::
  RelationalDatabaseBlueprint
mkRelationalDatabaseBlueprint =
  RelationalDatabaseBlueprint'
    { blueprintId = Core.Nothing,
      engine = Core.Nothing,
      engineDescription = Core.Nothing,
      engineVersion = Core.Nothing,
      engineVersionDescription = Core.Nothing,
      isEngineDefault = Core.Nothing
    }

-- | The ID for the database blueprint.
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbBlueprintId :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Types.String)
rdbBlueprintId = Lens.field @"blueprintId"
{-# DEPRECATED rdbBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The database software of the database blueprint (for example, @MySQL@ ).
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngine :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Types.RelationalDatabaseEngine)
rdbEngine = Lens.field @"engine"
{-# DEPRECATED rdbEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The description of the database engine for the database blueprint.
--
-- /Note:/ Consider using 'engineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineDescription :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Types.String)
rdbEngineDescription = Lens.field @"engineDescription"
{-# DEPRECATED rdbEngineDescription "Use generic-lens or generic-optics with 'engineDescription' instead." #-}

-- | The database engine version for the database blueprint (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineVersion :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Types.String)
rdbEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED rdbEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The description of the database engine version for the database blueprint.
--
-- /Note:/ Consider using 'engineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineVersionDescription :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Types.String)
rdbEngineVersionDescription = Lens.field @"engineVersionDescription"
{-# DEPRECATED rdbEngineVersionDescription "Use generic-lens or generic-optics with 'engineVersionDescription' instead." #-}

-- | A Boolean value indicating whether the engine version is the default for the database blueprint.
--
-- /Note:/ Consider using 'isEngineDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsEngineDefault :: Lens.Lens' RelationalDatabaseBlueprint (Core.Maybe Core.Bool)
rdbIsEngineDefault = Lens.field @"isEngineDefault"
{-# DEPRECATED rdbIsEngineDefault "Use generic-lens or generic-optics with 'isEngineDefault' instead." #-}

instance Core.FromJSON RelationalDatabaseBlueprint where
  parseJSON =
    Core.withObject "RelationalDatabaseBlueprint" Core.$
      \x ->
        RelationalDatabaseBlueprint'
          Core.<$> (x Core..:? "blueprintId")
          Core.<*> (x Core..:? "engine")
          Core.<*> (x Core..:? "engineDescription")
          Core.<*> (x Core..:? "engineVersion")
          Core.<*> (x Core..:? "engineVersionDescription")
          Core.<*> (x Core..:? "isEngineDefault")
