{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of an endpoint in an Amazon Aurora DB cluster.
module Network.AWS.RDS.ModifyDBClusterEndpoint
    (
    -- * Creating a request
      ModifyDBClusterEndpoint (..)
    , mkModifyDBClusterEndpoint
    -- ** Request lenses
    , mdbceDBClusterEndpointIdentifier
    , mdbceEndpointType
    , mdbceExcludedMembers
    , mdbceStaticMembers

     -- * Destructuring the response
    , Types.DBClusterEndpoint (..)
    , Types.mkDBClusterEndpoint
    -- ** Response lenses
    , Types.dbceCustomEndpointType
    , Types.dbceDBClusterEndpointArn
    , Types.dbceDBClusterEndpointIdentifier
    , Types.dbceDBClusterEndpointResourceIdentifier
    , Types.dbceDBClusterIdentifier
    , Types.dbceEndpoint
    , Types.dbceEndpointType
    , Types.dbceExcludedMembers
    , Types.dbceStaticMembers
    , Types.dbceStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { dBClusterEndpointIdentifier :: Core.Text
    -- ^ The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
  , endpointType :: Core.Maybe Core.Text
    -- ^ The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
  , excludedMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
  , staticMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that are part of the custom endpoint group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBClusterEndpoint' value with any optional fields omitted.
mkModifyDBClusterEndpoint
    :: Core.Text -- ^ 'dBClusterEndpointIdentifier'
    -> ModifyDBClusterEndpoint
mkModifyDBClusterEndpoint dBClusterEndpointIdentifier
  = ModifyDBClusterEndpoint'{dBClusterEndpointIdentifier,
                             endpointType = Core.Nothing, excludedMembers = Core.Nothing,
                             staticMembers = Core.Nothing}

-- | The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceDBClusterEndpointIdentifier :: Lens.Lens' ModifyDBClusterEndpoint Core.Text
mdbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# INLINEABLE mdbceDBClusterEndpointIdentifier #-}
{-# DEPRECATED dBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead"  #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceEndpointType :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe Core.Text)
mdbceEndpointType = Lens.field @"endpointType"
{-# INLINEABLE mdbceEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceExcludedMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Core.Text])
mdbceExcludedMembers = Lens.field @"excludedMembers"
{-# INLINEABLE mdbceExcludedMembers #-}
{-# DEPRECATED excludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead"  #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceStaticMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Core.Text])
mdbceStaticMembers = Lens.field @"staticMembers"
{-# INLINEABLE mdbceStaticMembers #-}
{-# DEPRECATED staticMembers "Use generic-lens or generic-optics with 'staticMembers' instead"  #-}

instance Core.ToQuery ModifyDBClusterEndpoint where
        toQuery ModifyDBClusterEndpoint{..}
          = Core.toQueryPair "Action"
              ("ModifyDBClusterEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterEndpointIdentifier"
                dBClusterEndpointIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EndpointType")
                endpointType
              Core.<>
              Core.toQueryPair "ExcludedMembers"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   excludedMembers)
              Core.<>
              Core.toQueryPair "StaticMembers"
                (Core.maybe Core.mempty (Core.toQueryList "member") staticMembers)

instance Core.ToHeaders ModifyDBClusterEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBClusterEndpoint where
        type Rs ModifyDBClusterEndpoint = Types.DBClusterEndpoint
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyDBClusterEndpointResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
