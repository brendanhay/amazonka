{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom endpoint and associates it with an Amazon Aurora DB cluster.
module Network.AWS.RDS.CreateDBClusterEndpoint
    (
    -- * Creating a request
      CreateDBClusterEndpoint (..)
    , mkCreateDBClusterEndpoint
    -- ** Request lenses
    , cdbceDBClusterIdentifier
    , cdbceDBClusterEndpointIdentifier
    , cdbceEndpointType
    , cdbceExcludedMembers
    , cdbceStaticMembers
    , cdbceTags

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

-- | /See:/ 'mkCreateDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { dBClusterIdentifier :: Core.Text
    -- ^ The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
  , dBClusterEndpointIdentifier :: Core.Text
    -- ^ The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
  , endpointType :: Core.Text
    -- ^ The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
  , excludedMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
  , staticMembers :: Core.Maybe [Core.Text]
    -- ^ List of DB instance identifiers that are part of the custom endpoint group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be assigned to the Amazon RDS resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterEndpoint' value with any optional fields omitted.
mkCreateDBClusterEndpoint
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> Core.Text -- ^ 'dBClusterEndpointIdentifier'
    -> Core.Text -- ^ 'endpointType'
    -> CreateDBClusterEndpoint
mkCreateDBClusterEndpoint dBClusterIdentifier
  dBClusterEndpointIdentifier endpointType
  = CreateDBClusterEndpoint'{dBClusterIdentifier,
                             dBClusterEndpointIdentifier, endpointType,
                             excludedMembers = Core.Nothing, staticMembers = Core.Nothing,
                             tags = Core.Nothing}

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceDBClusterIdentifier :: Lens.Lens' CreateDBClusterEndpoint Core.Text
cdbceDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE cdbceDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceDBClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpoint Core.Text
cdbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# INLINEABLE cdbceDBClusterEndpointIdentifier #-}
{-# DEPRECATED dBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead"  #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceEndpointType :: Lens.Lens' CreateDBClusterEndpoint Core.Text
cdbceEndpointType = Lens.field @"endpointType"
{-# INLINEABLE cdbceEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceExcludedMembers :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Core.Text])
cdbceExcludedMembers = Lens.field @"excludedMembers"
{-# INLINEABLE cdbceExcludedMembers #-}
{-# DEPRECATED excludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead"  #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceStaticMembers :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Core.Text])
cdbceStaticMembers = Lens.field @"staticMembers"
{-# INLINEABLE cdbceStaticMembers #-}
{-# DEPRECATED staticMembers "Use generic-lens or generic-optics with 'staticMembers' instead"  #-}

-- | The tags to be assigned to the Amazon RDS resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceTags :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Types.Tag])
cdbceTags = Lens.field @"tags"
{-# INLINEABLE cdbceTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBClusterEndpoint where
        toQuery CreateDBClusterEndpoint{..}
          = Core.toQueryPair "Action"
              ("CreateDBClusterEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.toQueryPair "DBClusterEndpointIdentifier"
                dBClusterEndpointIdentifier
              Core.<> Core.toQueryPair "EndpointType" endpointType
              Core.<>
              Core.toQueryPair "ExcludedMembers"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   excludedMembers)
              Core.<>
              Core.toQueryPair "StaticMembers"
                (Core.maybe Core.mempty (Core.toQueryList "member") staticMembers)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBClusterEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBClusterEndpoint where
        type Rs CreateDBClusterEndpoint = Types.DBClusterEndpoint
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
          = Response.receiveXMLWrapper "CreateDBClusterEndpointResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
