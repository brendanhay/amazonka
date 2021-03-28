{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom endpoint and removes it from an Amazon Aurora DB cluster.
module Network.AWS.RDS.DeleteDBClusterEndpoint
    (
    -- * Creating a request
      DeleteDBClusterEndpoint (..)
    , mkDeleteDBClusterEndpoint
    -- ** Request lenses
    , ddbceDBClusterEndpointIdentifier

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

-- | /See:/ 'mkDeleteDBClusterEndpoint' smart constructor.
newtype DeleteDBClusterEndpoint = DeleteDBClusterEndpoint'
  { dBClusterEndpointIdentifier :: Core.Text
    -- ^ The identifier associated with the custom endpoint. This parameter is stored as a lowercase string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBClusterEndpoint' value with any optional fields omitted.
mkDeleteDBClusterEndpoint
    :: Core.Text -- ^ 'dBClusterEndpointIdentifier'
    -> DeleteDBClusterEndpoint
mkDeleteDBClusterEndpoint dBClusterEndpointIdentifier
  = DeleteDBClusterEndpoint'{dBClusterEndpointIdentifier}

-- | The identifier associated with the custom endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceDBClusterEndpointIdentifier :: Lens.Lens' DeleteDBClusterEndpoint Core.Text
ddbceDBClusterEndpointIdentifier = Lens.field @"dBClusterEndpointIdentifier"
{-# INLINEABLE ddbceDBClusterEndpointIdentifier #-}
{-# DEPRECATED dBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dBClusterEndpointIdentifier' instead"  #-}

instance Core.ToQuery DeleteDBClusterEndpoint where
        toQuery DeleteDBClusterEndpoint{..}
          = Core.toQueryPair "Action"
              ("DeleteDBClusterEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterEndpointIdentifier"
                dBClusterEndpointIdentifier

instance Core.ToHeaders DeleteDBClusterEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBClusterEndpoint where
        type Rs DeleteDBClusterEndpoint = Types.DBClusterEndpoint
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
          = Response.receiveXMLWrapper "DeleteDBClusterEndpointResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
