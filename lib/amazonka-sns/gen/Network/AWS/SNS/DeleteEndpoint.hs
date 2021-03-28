{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the endpoint for a device and mobile app from Amazon SNS. This action is idempotent. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> . 
--
-- When you delete an endpoint that is also subscribed to a topic, then you must also unsubscribe the endpoint from the topic.
module Network.AWS.SNS.DeleteEndpoint
    (
    -- * Creating a request
      DeleteEndpoint (..)
    , mkDeleteEndpoint
    -- ** Request lenses
    , deEndpointArn

    -- * Destructuring the response
    , DeleteEndpointResponse (..)
    , mkDeleteEndpointResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for DeleteEndpoint action.
--
-- /See:/ 'mkDeleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
  { endpointArn :: Core.Text
    -- ^ EndpointArn of endpoint to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpoint' value with any optional fields omitted.
mkDeleteEndpoint
    :: Core.Text -- ^ 'endpointArn'
    -> DeleteEndpoint
mkDeleteEndpoint endpointArn = DeleteEndpoint'{endpointArn}

-- | EndpointArn of endpoint to delete.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointArn :: Lens.Lens' DeleteEndpoint Core.Text
deEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE deEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.ToQuery DeleteEndpoint where
        toQuery DeleteEndpoint{..}
          = Core.toQueryPair "Action" ("DeleteEndpoint" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "EndpointArn" endpointArn

instance Core.ToHeaders DeleteEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteEndpoint where
        type Rs DeleteEndpoint = DeleteEndpointResponse
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
        parseResponse = Response.receiveNull DeleteEndpointResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpointResponse' value with any optional fields omitted.
mkDeleteEndpointResponse
    :: DeleteEndpointResponse
mkDeleteEndpointResponse = DeleteEndpointResponse'
