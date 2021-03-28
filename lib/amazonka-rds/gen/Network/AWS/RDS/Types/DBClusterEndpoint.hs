{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBClusterEndpoint
  ( DBClusterEndpoint (..)
  -- * Smart constructor
  , mkDBClusterEndpoint
  -- * Lenses
  , dbceCustomEndpointType
  , dbceDBClusterEndpointArn
  , dbceDBClusterEndpointIdentifier
  , dbceDBClusterEndpointResourceIdentifier
  , dbceDBClusterIdentifier
  , dbceEndpoint
  , dbceEndpointType
  , dbceExcludedMembers
  , dbceStaticMembers
  , dbceStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { customEndpointType :: Core.Maybe Core.Text
    -- ^ The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
  , dBClusterEndpointArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the endpoint.
  , dBClusterEndpointIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier associated with the endpoint. This parameter is stored as a lowercase string.
  , dBClusterEndpointResourceIdentifier :: Core.Maybe Core.Text
    -- ^ A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
  , dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
  , endpoint :: Core.Maybe Core.Text
    -- ^ The DNS address of the endpoint.
  , endpointType :: Core.Maybe Core.Text
    -- ^ The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
  , excludedMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
  , staticMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that are part of the custom endpoint group.
  , status :: Core.Maybe Core.Text
    -- ^ The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterEndpoint' value with any optional fields omitted.
mkDBClusterEndpoint
    :: DBClusterEndpoint
mkDBClusterEndpoint
  = DBClusterEndpoint'{customEndpointType = Core.Nothing,
                       dBClusterEndpointArn = Core.Nothing,
                       dBClusterEndpointIdentifier = Core.Nothing,
                       dBClusterEndpointResourceIdentifier = Core.Nothing,
                       dBClusterIdentifier = Core.Nothing, endpoint = Core.Nothing,
                       endpointType = Core.Nothing, excludedMembers = Core.Nothing,
                       staticMembers = Core.Nothing, status = Core.Nothing}

-- | The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'customEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceCustomEndpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceCustomEndpointType = Lens.field @"customEndpointType"
{-# INLINEABLE dbceCustomEndpointType #-}
{-# DEPRECATED customEndpointType "Use generic-lens or generic-optics with 'customEndpointType' instead"  #-}

-- | The Amazon Resource Name (ARN) for the endpoint.
--
-- /Note:/ Consider using 'dBClusterEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointArn :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceDBClusterEndpointArn = Lens.field @"dBClusterEndpointArn"
{-# INLINEABLE dbceDBClusterEndpointArn #-}
{-# DEPRECATED dBClusterEndpointArn "Use generic-lens or generic-optics with 'dBClusterEndpointArn' instead"  #-}

-- | The identifier associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# INLINEABLE dbceDBClusterEndpointIdentifier #-}
{-# DEPRECATED dBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead"  #-}

-- | A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
--
-- /Note:/ Consider using 'dBClusterEndpointResourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterEndpointResourceIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceDBClusterEndpointResourceIdentifier = Lens.field @"dBClusterEndpointResourceIdentifier"
{-# INLINEABLE dbceDBClusterEndpointResourceIdentifier #-}
{-# DEPRECATED dBClusterEndpointResourceIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointResourceIdentifier' instead"  #-}

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceDBClusterIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE dbceDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The DNS address of the endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceEndpoint :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceEndpoint = Lens.field @"endpoint"
{-# INLINEABLE dbceEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceEndpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceEndpointType = Lens.field @"endpointType"
{-# INLINEABLE dbceEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceExcludedMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Core.Text])
dbceExcludedMembers = Lens.field @"excludedMembers"
{-# INLINEABLE dbceExcludedMembers #-}
{-# DEPRECATED excludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead"  #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceStaticMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Core.Text])
dbceStaticMembers = Lens.field @"staticMembers"
{-# INLINEABLE dbceStaticMembers #-}
{-# DEPRECATED staticMembers "Use generic-lens or generic-optics with 'staticMembers' instead"  #-}

-- | The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbceStatus :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbceStatus = Lens.field @"status"
{-# INLINEABLE dbceStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML DBClusterEndpoint where
        parseXML x
          = DBClusterEndpoint' Core.<$>
              (x Core..@? "CustomEndpointType") Core.<*>
                x Core..@? "DBClusterEndpointArn"
                Core.<*> x Core..@? "DBClusterEndpointIdentifier"
                Core.<*> x Core..@? "DBClusterEndpointResourceIdentifier"
                Core.<*> x Core..@? "DBClusterIdentifier"
                Core.<*> x Core..@? "Endpoint"
                Core.<*> x Core..@? "EndpointType"
                Core.<*>
                x Core..@? "ExcludedMembers" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "StaticMembers" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Status"
