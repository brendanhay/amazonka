{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterEndpoint
  ( DBClusterEndpoint (..),

    -- * Smart constructor
    mkDBClusterEndpoint,

    -- * Lenses
    dbceCustomEndpointType,
    dbceDBClusterEndpointArn,
    dbceDBClusterEndpointIdentifier,
    dbceDBClusterEndpointResourceIdentifier,
    dbceDBClusterIdentifier,
    dbceEndpoint,
    dbceEndpointType,
    dbceExcludedMembers,
    dbceStaticMembers,
    dbceStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type represents the information you need to connect to an Amazon Aurora DB cluster. This data type is used as a response element in the following actions:
--
--
--     * @CreateDBClusterEndpoint@
--
--
--     * @DescribeDBClusterEndpoints@
--
--
--     * @ModifyDBClusterEndpoint@
--
--
--     * @DeleteDBClusterEndpoint@
--
--
-- For the data structure that represents Amazon RDS DB instance endpoints, see @Endpoint@ .
--
-- /See:/ 'mkDBClusterEndpoint' smart constructor.
data DBClusterEndpoint = DBClusterEndpoint'
  { -- | The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
    customEndpointType :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the endpoint.
    dBClusterEndpointArn :: Core.Maybe Types.String,
    -- | The identifier associated with the endpoint. This parameter is stored as a lowercase string.
    dBClusterEndpointIdentifier :: Core.Maybe Types.String,
    -- | A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
    dBClusterEndpointResourceIdentifier :: Core.Maybe Types.String,
    -- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
    dBClusterIdentifier :: Core.Maybe Types.String,
    -- | The DNS address of the endpoint.
    endpoint :: Core.Maybe Types.String,
    -- | The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
    endpointType :: Core.Maybe Types.String,
    -- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Core.Maybe [Types.String],
    -- | List of DB instance identifiers that are part of the custom endpoint group.
    staticMembers :: Core.Maybe [Types.String],
    -- | The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterEndpoint' value with any optional fields omitted.
mkDBClusterEndpoint ::
  DBClusterEndpoint
mkDBClusterEndpoint =
  DBClusterEndpoint'
    { customEndpointType = Core.Nothing,
      dBClusterEndpointArn = Core.Nothing,
      dBClusterEndpointIdentifier = Core.Nothing,
      dBClusterEndpointResourceIdentifier = Core.Nothing,
      dBClusterIdentifier = Core.Nothing,
      endpoint = Core.Nothing,
      endpointType = Core.Nothing,
      excludedMembers = Core.Nothing,
      staticMembers = Core.Nothing,
      status = Core.Nothing
    }

-- | The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'customEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceCustomEndpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceCustomEndpointType = Lens.field @"customEndpointType"
{-# DEPRECATED dbceCustomEndpointType "Use generic-lens or generic-optics with 'customEndpointType' instead." #-}

-- | The Amazon Resource Name (ARN) for the endpoint.
--
-- /Note:/ Consider using 'dBClusterEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointArn :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceDBClusterEndpointArn = Lens.field @"dBClusterEndpointArn"
{-# DEPRECATED dbceDBClusterEndpointArn "Use generic-lens or generic-optics with 'dBClusterEndpointArn' instead." #-}

-- | The identifier associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# DEPRECATED dbceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead." #-}

-- | A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
--
-- /Note:/ Consider using 'dBClusterEndpointResourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointResourceIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceDBClusterEndpointResourceIdentifier = Lens.field @"dBClusterEndpointResourceIdentifier"
{-# DEPRECATED dbceDBClusterEndpointResourceIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointResourceIdentifier' instead." #-}

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED dbceDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The DNS address of the endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceEndpoint :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceEndpoint = Lens.field @"endpoint"
{-# DEPRECATED dbceEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceEndpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceEndpointType = Lens.field @"endpointType"
{-# DEPRECATED dbceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceExcludedMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Types.String])
dbceExcludedMembers = Lens.field @"excludedMembers"
{-# DEPRECATED dbceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceStaticMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Types.String])
dbceStaticMembers = Lens.field @"staticMembers"
{-# DEPRECATED dbceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

-- | The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceStatus :: Lens.Lens' DBClusterEndpoint (Core.Maybe Types.String)
dbceStatus = Lens.field @"status"
{-# DEPRECATED dbceStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML DBClusterEndpoint where
  parseXML x =
    DBClusterEndpoint'
      Core.<$> (x Core..@? "CustomEndpointType")
      Core.<*> (x Core..@? "DBClusterEndpointArn")
      Core.<*> (x Core..@? "DBClusterEndpointIdentifier")
      Core.<*> (x Core..@? "DBClusterEndpointResourceIdentifier")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "EndpointType")
      Core.<*> (x Core..@? "ExcludedMembers" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "StaticMembers" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Status")
