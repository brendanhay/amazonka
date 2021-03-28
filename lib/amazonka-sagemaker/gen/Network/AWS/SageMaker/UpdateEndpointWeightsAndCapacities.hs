{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates variant weight of one or more variants associated with an existing endpoint, or capacity of one variant associated with an existing endpoint. When it receives the request, Amazon SageMaker sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the 'DescribeEndpoint' API. 
module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
    (
    -- * Creating a request
      UpdateEndpointWeightsAndCapacities (..)
    , mkUpdateEndpointWeightsAndCapacities
    -- ** Request lenses
    , uewacEndpointName
    , uewacDesiredWeightsAndCapacities

    -- * Destructuring the response
    , UpdateEndpointWeightsAndCapacitiesResponse (..)
    , mkUpdateEndpointWeightsAndCapacitiesResponse
    -- ** Response lenses
    , uewacrrsEndpointArn
    , uewacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateEndpointWeightsAndCapacities' smart constructor.
data UpdateEndpointWeightsAndCapacities = UpdateEndpointWeightsAndCapacities'
  { endpointName :: Types.EndpointName
    -- ^ The name of an existing Amazon SageMaker endpoint.
  , desiredWeightsAndCapacities :: Core.NonEmpty Types.DesiredWeightAndCapacity
    -- ^ An object that provides new capacity and weight values for a variant.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointWeightsAndCapacities' value with any optional fields omitted.
mkUpdateEndpointWeightsAndCapacities
    :: Types.EndpointName -- ^ 'endpointName'
    -> Core.NonEmpty Types.DesiredWeightAndCapacity -- ^ 'desiredWeightsAndCapacities'
    -> UpdateEndpointWeightsAndCapacities
mkUpdateEndpointWeightsAndCapacities endpointName
  desiredWeightsAndCapacities
  = UpdateEndpointWeightsAndCapacities'{endpointName,
                                        desiredWeightsAndCapacities}

-- | The name of an existing Amazon SageMaker endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacEndpointName :: Lens.Lens' UpdateEndpointWeightsAndCapacities Types.EndpointName
uewacEndpointName = Lens.field @"endpointName"
{-# INLINEABLE uewacEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | An object that provides new capacity and weight values for a variant.
--
-- /Note:/ Consider using 'desiredWeightsAndCapacities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacDesiredWeightsAndCapacities :: Lens.Lens' UpdateEndpointWeightsAndCapacities (Core.NonEmpty Types.DesiredWeightAndCapacity)
uewacDesiredWeightsAndCapacities = Lens.field @"desiredWeightsAndCapacities"
{-# INLINEABLE uewacDesiredWeightsAndCapacities #-}
{-# DEPRECATED desiredWeightsAndCapacities "Use generic-lens or generic-optics with 'desiredWeightsAndCapacities' instead"  #-}

instance Core.ToQuery UpdateEndpointWeightsAndCapacities where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEndpointWeightsAndCapacities where
        toHeaders UpdateEndpointWeightsAndCapacities{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.UpdateEndpointWeightsAndCapacities")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEndpointWeightsAndCapacities where
        toJSON UpdateEndpointWeightsAndCapacities{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointName" Core..= endpointName),
                  Core.Just
                    ("DesiredWeightsAndCapacities" Core..=
                       desiredWeightsAndCapacities)])

instance Core.AWSRequest UpdateEndpointWeightsAndCapacities where
        type Rs UpdateEndpointWeightsAndCapacities =
             UpdateEndpointWeightsAndCapacitiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEndpointWeightsAndCapacitiesResponse' Core.<$>
                   (x Core..: "EndpointArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEndpointWeightsAndCapacitiesResponse' smart constructor.
data UpdateEndpointWeightsAndCapacitiesResponse = UpdateEndpointWeightsAndCapacitiesResponse'
  { endpointArn :: Types.EndpointArn
    -- ^ The Amazon Resource Name (ARN) of the updated endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointWeightsAndCapacitiesResponse' value with any optional fields omitted.
mkUpdateEndpointWeightsAndCapacitiesResponse
    :: Types.EndpointArn -- ^ 'endpointArn'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateEndpointWeightsAndCapacitiesResponse
mkUpdateEndpointWeightsAndCapacitiesResponse endpointArn
  responseStatus
  = UpdateEndpointWeightsAndCapacitiesResponse'{endpointArn,
                                                responseStatus}

-- | The Amazon Resource Name (ARN) of the updated endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacrrsEndpointArn :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Types.EndpointArn
uewacrrsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE uewacrrsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacrrsResponseStatus :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Core.Int
uewacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uewacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
