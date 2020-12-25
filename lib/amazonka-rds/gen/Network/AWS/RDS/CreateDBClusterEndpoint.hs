{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDBClusterEndpoint (..),
    mkCreateDBClusterEndpoint,

    -- ** Request lenses
    cdbceDBClusterIdentifier,
    cdbceDBClusterEndpointIdentifier,
    cdbceEndpointType,
    cdbceExcludedMembers,
    cdbceStaticMembers,
    cdbceTags,

    -- * Destructuring the response
    Types.DBClusterEndpoint (..),
    Types.mkDBClusterEndpoint,

    -- ** Response lenses
    Types.dbceCustomEndpointType,
    Types.dbceDBClusterEndpointArn,
    Types.dbceDBClusterEndpointIdentifier,
    Types.dbceDBClusterEndpointResourceIdentifier,
    Types.dbceDBClusterIdentifier,
    Types.dbceEndpoint,
    Types.dbceEndpointType,
    Types.dbceExcludedMembers,
    Types.dbceStaticMembers,
    Types.dbceStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { -- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
    dBClusterIdentifier :: Types.String,
    -- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
    dBClusterEndpointIdentifier :: Types.String,
    -- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
    endpointType :: Types.String,
    -- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Core.Maybe [Types.String],
    -- | List of DB instance identifiers that are part of the custom endpoint group.
    staticMembers :: Core.Maybe [Types.String],
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterEndpoint' value with any optional fields omitted.
mkCreateDBClusterEndpoint ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'dBClusterEndpointIdentifier'
  Types.String ->
  -- | 'endpointType'
  Types.String ->
  CreateDBClusterEndpoint
mkCreateDBClusterEndpoint
  dBClusterIdentifier
  dBClusterEndpointIdentifier
  endpointType =
    CreateDBClusterEndpoint'
      { dBClusterIdentifier,
        dBClusterEndpointIdentifier,
        endpointType,
        excludedMembers = Core.Nothing,
        staticMembers = Core.Nothing,
        tags = Core.Nothing
      }

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceDBClusterIdentifier :: Lens.Lens' CreateDBClusterEndpoint Types.String
cdbceDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED cdbceDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceDBClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpoint Types.String
cdbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# DEPRECATED cdbceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceEndpointType :: Lens.Lens' CreateDBClusterEndpoint Types.String
cdbceEndpointType = Lens.field @"endpointType"
{-# DEPRECATED cdbceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceExcludedMembers :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Types.String])
cdbceExcludedMembers = Lens.field @"excludedMembers"
{-# DEPRECATED cdbceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceStaticMembers :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Types.String])
cdbceStaticMembers = Lens.field @"staticMembers"
{-# DEPRECATED cdbceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

-- | The tags to be assigned to the Amazon RDS resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbceTags :: Lens.Lens' CreateDBClusterEndpoint (Core.Maybe [Types.Tag])
cdbceTags = Lens.field @"tags"
{-# DEPRECATED cdbceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateDBClusterEndpoint where
  type Rs CreateDBClusterEndpoint = Types.DBClusterEndpoint
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateDBClusterEndpoint")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> ( Core.toQueryValue
                            "DBClusterEndpointIdentifier"
                            dBClusterEndpointIdentifier
                        )
                Core.<> (Core.toQueryValue "EndpointType" endpointType)
                Core.<> ( Core.toQueryValue
                            "ExcludedMembers"
                            (Core.toQueryList "member" Core.<$> excludedMembers)
                        )
                Core.<> ( Core.toQueryValue
                            "StaticMembers"
                            (Core.toQueryList "member" Core.<$> staticMembers)
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterEndpointResult"
      (\s h x -> Core.parseXML x)
