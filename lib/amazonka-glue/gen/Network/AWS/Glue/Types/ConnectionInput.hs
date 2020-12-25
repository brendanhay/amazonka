{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionInput
  ( ConnectionInput (..),

    -- * Smart constructor
    mkConnectionInput,

    -- * Lenses
    ciName,
    ciConnectionType,
    ciConnectionProperties,
    ciDescription,
    ciMatchCriteria,
    ciPhysicalConnectionRequirements,
  )
where

import qualified Network.AWS.Glue.Types.ConnectionPropertyKey as Types
import qualified Network.AWS.Glue.Types.ConnectionType as Types
import qualified Network.AWS.Glue.Types.Description as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.PhysicalConnectionRequirements as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that is used to specify a connection to create or update.
--
-- /See:/ 'mkConnectionInput' smart constructor.
data ConnectionInput = ConnectionInput'
  { -- | The name of the connection.
    name :: Types.Name,
    -- | The type of the connection. Currently, these types are supported:
    --
    --
    --     * @JDBC@ - Designates a connection to a database through Java Database Connectivity (JDBC).
    --
    --
    --     * @KAFKA@ - Designates a connection to an Apache Kafka streaming platform.
    --
    --
    --     * @MONGODB@ - Designates a connection to a MongoDB document database.
    --
    --
    --     * @NETWORK@ - Designates a network connection to a data source within an Amazon Virtual Private Cloud environment (Amazon VPC).
    --
    --
    -- SFTP is not supported.
    connectionType :: Types.ConnectionType,
    -- | These key-value pairs define parameters for the connection.
    connectionProperties :: Core.HashMap Types.ConnectionPropertyKey Types.ValueString,
    -- | The description of the connection.
    description :: Core.Maybe Types.Description,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Core.Maybe [Types.NameString],
    -- | A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to successfully make this connection.
    physicalConnectionRequirements :: Core.Maybe Types.PhysicalConnectionRequirements
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionInput' value with any optional fields omitted.
mkConnectionInput ::
  -- | 'name'
  Types.Name ->
  -- | 'connectionType'
  Types.ConnectionType ->
  ConnectionInput
mkConnectionInput name connectionType =
  ConnectionInput'
    { name,
      connectionType,
      connectionProperties = Core.mempty,
      description = Core.Nothing,
      matchCriteria = Core.Nothing,
      physicalConnectionRequirements = Core.Nothing
    }

-- | The name of the connection.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' ConnectionInput Types.Name
ciName = Lens.field @"name"
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the connection. Currently, these types are supported:
--
--
--     * @JDBC@ - Designates a connection to a database through Java Database Connectivity (JDBC).
--
--
--     * @KAFKA@ - Designates a connection to an Apache Kafka streaming platform.
--
--
--     * @MONGODB@ - Designates a connection to a MongoDB document database.
--
--
--     * @NETWORK@ - Designates a network connection to a data source within an Amazon Virtual Private Cloud environment (Amazon VPC).
--
--
-- SFTP is not supported.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConnectionType :: Lens.Lens' ConnectionInput Types.ConnectionType
ciConnectionType = Lens.field @"connectionType"
{-# DEPRECATED ciConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | These key-value pairs define parameters for the connection.
--
-- /Note:/ Consider using 'connectionProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConnectionProperties :: Lens.Lens' ConnectionInput (Core.HashMap Types.ConnectionPropertyKey Types.ValueString)
ciConnectionProperties = Lens.field @"connectionProperties"
{-# DEPRECATED ciConnectionProperties "Use generic-lens or generic-optics with 'connectionProperties' instead." #-}

-- | The description of the connection.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDescription :: Lens.Lens' ConnectionInput (Core.Maybe Types.Description)
ciDescription = Lens.field @"description"
{-# DEPRECATED ciDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of criteria that can be used in selecting this connection.
--
-- /Note:/ Consider using 'matchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMatchCriteria :: Lens.Lens' ConnectionInput (Core.Maybe [Types.NameString])
ciMatchCriteria = Lens.field @"matchCriteria"
{-# DEPRECATED ciMatchCriteria "Use generic-lens or generic-optics with 'matchCriteria' instead." #-}

-- | A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to successfully make this connection.
--
-- /Note:/ Consider using 'physicalConnectionRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPhysicalConnectionRequirements :: Lens.Lens' ConnectionInput (Core.Maybe Types.PhysicalConnectionRequirements)
ciPhysicalConnectionRequirements = Lens.field @"physicalConnectionRequirements"
{-# DEPRECATED ciPhysicalConnectionRequirements "Use generic-lens or generic-optics with 'physicalConnectionRequirements' instead." #-}

instance Core.FromJSON ConnectionInput where
  toJSON ConnectionInput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ConnectionType" Core..= connectionType),
            Core.Just ("ConnectionProperties" Core..= connectionProperties),
            ("Description" Core..=) Core.<$> description,
            ("MatchCriteria" Core..=) Core.<$> matchCriteria,
            ("PhysicalConnectionRequirements" Core..=)
              Core.<$> physicalConnectionRequirements
          ]
      )
