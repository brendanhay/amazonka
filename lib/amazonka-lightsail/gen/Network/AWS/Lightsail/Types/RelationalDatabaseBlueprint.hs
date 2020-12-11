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
    rdbEngineVersion,
    rdbIsEngineDefault,
    rdbEngineVersionDescription,
    rdbEngine,
    rdbBlueprintId,
    rdbEngineDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.RelationalDatabaseEngine
import qualified Network.AWS.Prelude as Lude

-- | Describes a database image, or blueprint. A blueprint describes the major engine version of a database.
--
-- /See:/ 'mkRelationalDatabaseBlueprint' smart constructor.
data RelationalDatabaseBlueprint = RelationalDatabaseBlueprint'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    isEngineDefault ::
      Lude.Maybe Lude.Bool,
    engineVersionDescription ::
      Lude.Maybe Lude.Text,
    engine ::
      Lude.Maybe RelationalDatabaseEngine,
    blueprintId :: Lude.Maybe Lude.Text,
    engineDescription ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseBlueprint' with the minimum fields required to make a request.
--
-- * 'blueprintId' - The ID for the database blueprint.
-- * 'engine' - The database software of the database blueprint (for example, @MySQL@ ).
-- * 'engineDescription' - The description of the database engine for the database blueprint.
-- * 'engineVersion' - The database engine version for the database blueprint (for example, @5.7.23@ ).
-- * 'engineVersionDescription' - The description of the database engine version for the database blueprint.
-- * 'isEngineDefault' - A Boolean value indicating whether the engine version is the default for the database blueprint.
mkRelationalDatabaseBlueprint ::
  RelationalDatabaseBlueprint
mkRelationalDatabaseBlueprint =
  RelationalDatabaseBlueprint'
    { engineVersion = Lude.Nothing,
      isEngineDefault = Lude.Nothing,
      engineVersionDescription = Lude.Nothing,
      engine = Lude.Nothing,
      blueprintId = Lude.Nothing,
      engineDescription = Lude.Nothing
    }

-- | The database engine version for the database blueprint (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineVersion :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe Lude.Text)
rdbEngineVersion = Lens.lens (engineVersion :: RelationalDatabaseBlueprint -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A Boolean value indicating whether the engine version is the default for the database blueprint.
--
-- /Note:/ Consider using 'isEngineDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsEngineDefault :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe Lude.Bool)
rdbIsEngineDefault = Lens.lens (isEngineDefault :: RelationalDatabaseBlueprint -> Lude.Maybe Lude.Bool) (\s a -> s {isEngineDefault = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbIsEngineDefault "Use generic-lens or generic-optics with 'isEngineDefault' instead." #-}

-- | The description of the database engine version for the database blueprint.
--
-- /Note:/ Consider using 'engineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineVersionDescription :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe Lude.Text)
rdbEngineVersionDescription = Lens.lens (engineVersionDescription :: RelationalDatabaseBlueprint -> Lude.Maybe Lude.Text) (\s a -> s {engineVersionDescription = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbEngineVersionDescription "Use generic-lens or generic-optics with 'engineVersionDescription' instead." #-}

-- | The database software of the database blueprint (for example, @MySQL@ ).
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngine :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe RelationalDatabaseEngine)
rdbEngine = Lens.lens (engine :: RelationalDatabaseBlueprint -> Lude.Maybe RelationalDatabaseEngine) (\s a -> s {engine = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The ID for the database blueprint.
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbBlueprintId :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe Lude.Text)
rdbBlueprintId = Lens.lens (blueprintId :: RelationalDatabaseBlueprint -> Lude.Maybe Lude.Text) (\s a -> s {blueprintId = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The description of the database engine for the database blueprint.
--
-- /Note:/ Consider using 'engineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbEngineDescription :: Lens.Lens' RelationalDatabaseBlueprint (Lude.Maybe Lude.Text)
rdbEngineDescription = Lens.lens (engineDescription :: RelationalDatabaseBlueprint -> Lude.Maybe Lude.Text) (\s a -> s {engineDescription = a} :: RelationalDatabaseBlueprint)
{-# DEPRECATED rdbEngineDescription "Use generic-lens or generic-optics with 'engineDescription' instead." #-}

instance Lude.FromJSON RelationalDatabaseBlueprint where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseBlueprint"
      ( \x ->
          RelationalDatabaseBlueprint'
            Lude.<$> (x Lude..:? "engineVersion")
            Lude.<*> (x Lude..:? "isEngineDefault")
            Lude.<*> (x Lude..:? "engineVersionDescription")
            Lude.<*> (x Lude..:? "engine")
            Lude.<*> (x Lude..:? "blueprintId")
            Lude.<*> (x Lude..:? "engineDescription")
      )
