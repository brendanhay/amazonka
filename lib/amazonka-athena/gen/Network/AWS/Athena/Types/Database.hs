{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Database
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Database
  ( Database (..),

    -- * Smart constructor
    mkDatabase,

    -- * Lenses
    dName,
    dDescription,
    dParameters,
  )
where

import qualified Network.AWS.Athena.Types.Description as Types
import qualified Network.AWS.Athena.Types.KeyString as Types
import qualified Network.AWS.Athena.Types.Name as Types
import qualified Network.AWS.Athena.Types.ParametersMapValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains metadata information for a database in a data catalog.
--
-- /See:/ 'mkDatabase' smart constructor.
data Database = Database'
  { -- | The name of the database.
    name :: Types.Name,
    -- | An optional description of the database.
    description :: Core.Maybe Types.Description,
    -- | A set of custom key/value pairs.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Database' value with any optional fields omitted.
mkDatabase ::
  -- | 'name'
  Types.Name ->
  Database
mkDatabase name =
  Database'
    { name,
      description = Core.Nothing,
      parameters = Core.Nothing
    }

-- | The name of the database.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Database Types.Name
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An optional description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Database (Core.Maybe Types.Description)
dDescription = Lens.field @"description"
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A set of custom key/value pairs.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameters :: Lens.Lens' Database (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
dParameters = Lens.field @"parameters"
{-# DEPRECATED dParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject "Database" Core.$
      \x ->
        Database'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Parameters")
