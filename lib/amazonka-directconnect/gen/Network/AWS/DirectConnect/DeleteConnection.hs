{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection.
--
-- Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. If you are partnering with any third parties to connect with the AWS Direct Connect location, you must cancel your service with them separately.
module Network.AWS.DirectConnect.DeleteConnection
    (
    -- * Creating a request
      DeleteConnection (..)
    , mkDeleteConnection
    -- ** Request lenses
    , dcConnectionId

     -- * Destructuring the response
    , Types.Connection (..)
    , Types.mkConnection
    -- ** Response lenses
    , Types.cAwsDevice
    , Types.cAwsDeviceV2
    , Types.cBandwidth
    , Types.cConnectionId
    , Types.cConnectionName
    , Types.cConnectionState
    , Types.cHasLogicalRedundancy
    , Types.cJumboFrameCapable
    , Types.cLagId
    , Types.cLoaIssueTime
    , Types.cLocation
    , Types.cOwnerAccount
    , Types.cPartnerName
    , Types.cProviderName
    , Types.cRegion
    , Types.cTags
    , Types.cVlan
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnection' value with any optional fields omitted.
mkDeleteConnection
    :: Types.ConnectionId -- ^ 'connectionId'
    -> DeleteConnection
mkDeleteConnection connectionId = DeleteConnection'{connectionId}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionId :: Lens.Lens' DeleteConnection Types.ConnectionId
dcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE dcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery DeleteConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConnection where
        toHeaders DeleteConnection{..}
          = Core.pure ("X-Amz-Target", "OvertureService.DeleteConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConnection where
        toJSON DeleteConnection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest DeleteConnection where
        type Rs DeleteConnection = Types.Connection
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
