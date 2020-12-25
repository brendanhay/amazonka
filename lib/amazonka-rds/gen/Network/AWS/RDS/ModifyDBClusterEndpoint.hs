{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyDBClusterEndpoint (..),
    mkModifyDBClusterEndpoint,

    -- ** Request lenses
    mdbceDBClusterEndpointIdentifier,
    mdbceEndpointType,
    mdbceExcludedMembers,
    mdbceStaticMembers,

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

-- | /See:/ 'mkModifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { -- | The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
    dBClusterEndpointIdentifier :: Types.DBClusterEndpointIdentifier,
    -- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
    endpointType :: Core.Maybe Types.EndpointType,
    -- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Core.Maybe [Types.String],
    -- | List of DB instance identifiers that are part of the custom endpoint group.
    staticMembers :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBClusterEndpoint' value with any optional fields omitted.
mkModifyDBClusterEndpoint ::
  -- | 'dBClusterEndpointIdentifier'
  Types.DBClusterEndpointIdentifier ->
  ModifyDBClusterEndpoint
mkModifyDBClusterEndpoint dBClusterEndpointIdentifier =
  ModifyDBClusterEndpoint'
    { dBClusterEndpointIdentifier,
      endpointType = Core.Nothing,
      excludedMembers = Core.Nothing,
      staticMembers = Core.Nothing
    }

-- | The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceDBClusterEndpointIdentifier :: Lens.Lens' ModifyDBClusterEndpoint Types.DBClusterEndpointIdentifier
mdbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# DEPRECATED mdbceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceEndpointType :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe Types.EndpointType)
mdbceEndpointType = Lens.field @"endpointType"
{-# DEPRECATED mdbceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceExcludedMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Types.String])
mdbceExcludedMembers = Lens.field @"excludedMembers"
{-# DEPRECATED mdbceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbceStaticMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Types.String])
mdbceStaticMembers = Lens.field @"staticMembers"
{-# DEPRECATED mdbceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

instance Core.AWSRequest ModifyDBClusterEndpoint where
  type Rs ModifyDBClusterEndpoint = Types.DBClusterEndpoint
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
            ( Core.pure ("Action", "ModifyDBClusterEndpoint")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterEndpointIdentifier"
                            dBClusterEndpointIdentifier
                        )
                Core.<> (Core.toQueryValue "EndpointType" Core.<$> endpointType)
                Core.<> ( Core.toQueryValue
                            "ExcludedMembers"
                            (Core.toQueryList "member" Core.<$> excludedMembers)
                        )
                Core.<> ( Core.toQueryValue
                            "StaticMembers"
                            (Core.toQueryList "member" Core.<$> staticMembers)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterEndpointResult"
      (\s h x -> Core.parseXML x)
