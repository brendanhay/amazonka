{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseInput
  ( DatabaseInput (..),

    -- * Smart constructor
    mkDatabaseInput,

    -- * Lenses
    diName,
    diCreateTableDefaultPermissions,
    diDescription,
    diLocationUri,
    diParameters,
    diTargetDatabase,
  )
where

import qualified Network.AWS.Glue.Types.DatabaseIdentifier as Types
import qualified Network.AWS.Glue.Types.Description as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.LocationUri as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.PrincipalPermissions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure used to create or update a database.
--
-- /See:/ 'mkDatabaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { -- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
    name :: Types.Name,
    -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Core.Maybe [Types.PrincipalPermissions],
    -- | A description of the database.
    description :: Core.Maybe Types.Description,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Core.Maybe Types.LocationUri,
    -- | These key-value pairs define parameters and properties of the database.
    --
    -- These key-value pairs define parameters and properties of the database.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
    targetDatabase :: Core.Maybe Types.DatabaseIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatabaseInput' value with any optional fields omitted.
mkDatabaseInput ::
  -- | 'name'
  Types.Name ->
  DatabaseInput
mkDatabaseInput name =
  DatabaseInput'
    { name,
      createTableDefaultPermissions = Core.Nothing,
      description = Core.Nothing,
      locationUri = Core.Nothing,
      parameters = Core.Nothing,
      targetDatabase = Core.Nothing
    }

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DatabaseInput Types.Name
diName = Lens.field @"name"
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Creates a set of default permissions on the table for principals.
--
-- /Note:/ Consider using 'createTableDefaultPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreateTableDefaultPermissions :: Lens.Lens' DatabaseInput (Core.Maybe [Types.PrincipalPermissions])
diCreateTableDefaultPermissions = Lens.field @"createTableDefaultPermissions"
{-# DEPRECATED diCreateTableDefaultPermissions "Use generic-lens or generic-optics with 'createTableDefaultPermissions' instead." #-}

-- | A description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DatabaseInput (Core.Maybe Types.Description)
diDescription = Lens.field @"description"
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The location of the database (for example, an HDFS path).
--
-- /Note:/ Consider using 'locationUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLocationUri :: Lens.Lens' DatabaseInput (Core.Maybe Types.LocationUri)
diLocationUri = Lens.field @"locationUri"
{-# DEPRECATED diLocationUri "Use generic-lens or generic-optics with 'locationUri' instead." #-}

-- | These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diParameters :: Lens.Lens' DatabaseInput (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
diParameters = Lens.field @"parameters"
{-# DEPRECATED diParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
--
-- /Note:/ Consider using 'targetDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetDatabase :: Lens.Lens' DatabaseInput (Core.Maybe Types.DatabaseIdentifier)
diTargetDatabase = Lens.field @"targetDatabase"
{-# DEPRECATED diTargetDatabase "Use generic-lens or generic-optics with 'targetDatabase' instead." #-}

instance Core.FromJSON DatabaseInput where
  toJSON DatabaseInput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("CreateTableDefaultPermissions" Core..=)
              Core.<$> createTableDefaultPermissions,
            ("Description" Core..=) Core.<$> description,
            ("LocationUri" Core..=) Core.<$> locationUri,
            ("Parameters" Core..=) Core.<$> parameters,
            ("TargetDatabase" Core..=) Core.<$> targetDatabase
          ]
      )
