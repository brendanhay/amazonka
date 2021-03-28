{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a job's @JobState@ value is @New@ , you can update some of the information associated with a job. Once the job changes to a different job state, usually within 60 minutes of the job being created, this action is no longer available.
module Network.AWS.Snowball.UpdateJob
    (
    -- * Creating a request
      UpdateJob (..)
    , mkUpdateJob
    -- ** Request lenses
    , ujJobId
    , ujAddressId
    , ujDescription
    , ujForwardingAddressId
    , ujNotification
    , ujResources
    , ujRoleARN
    , ujShippingOption
    , ujSnowballCapacityPreference

    -- * Destructuring the response
    , UpdateJobResponse (..)
    , mkUpdateJobResponse
    -- ** Response lenses
    , ujrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { jobId :: Types.JobId
    -- ^ The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  , addressId :: Core.Maybe Types.AddressId
    -- ^ The ID of the updated 'Address' object.
  , description :: Core.Maybe Core.Text
    -- ^ The updated description of this job's 'JobMetadata' object.
  , forwardingAddressId :: Core.Maybe Types.ForwardingAddressId
    -- ^ The updated ID for the forwarding address for a job. This field is not supported in most regions.
  , notification :: Core.Maybe Types.Notification
    -- ^ The new or updated 'Notification' object.
  , resources :: Core.Maybe Types.JobResource
    -- ^ The updated @JobResource@ object, or the updated 'JobResource' object. 
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
  , shippingOption :: Core.Maybe Types.ShippingOption
    -- ^ The updated shipping option value of this job's 'ShippingDetails' object.
  , snowballCapacityPreference :: Core.Maybe Types.SnowballCapacity
    -- ^ The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob
    :: Types.JobId -- ^ 'jobId'
    -> UpdateJob
mkUpdateJob jobId
  = UpdateJob'{jobId, addressId = Core.Nothing,
               description = Core.Nothing, forwardingAddressId = Core.Nothing,
               notification = Core.Nothing, resources = Core.Nothing,
               roleARN = Core.Nothing, shippingOption = Core.Nothing,
               snowballCapacityPreference = Core.Nothing}

-- | The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Types.JobId
ujJobId = Lens.field @"jobId"
{-# INLINEABLE ujJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAddressId :: Lens.Lens' UpdateJob (Core.Maybe Types.AddressId)
ujAddressId = Lens.field @"addressId"
{-# INLINEABLE ujAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The updated description of this job's 'JobMetadata' object.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujDescription :: Lens.Lens' UpdateJob (Core.Maybe Core.Text)
ujDescription = Lens.field @"description"
{-# INLINEABLE ujDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated ID for the forwarding address for a job. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujForwardingAddressId :: Lens.Lens' UpdateJob (Core.Maybe Types.ForwardingAddressId)
ujForwardingAddressId = Lens.field @"forwardingAddressId"
{-# INLINEABLE ujForwardingAddressId #-}
{-# DEPRECATED forwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead"  #-}

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujNotification :: Lens.Lens' UpdateJob (Core.Maybe Types.Notification)
ujNotification = Lens.field @"notification"
{-# INLINEABLE ujNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The updated @JobResource@ object, or the updated 'JobResource' object. 
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujResources :: Lens.Lens' UpdateJob (Core.Maybe Types.JobResource)
ujResources = Lens.field @"resources"
{-# INLINEABLE ujResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujRoleARN :: Lens.Lens' UpdateJob (Core.Maybe Types.RoleARN)
ujRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ujRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The updated shipping option value of this job's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujShippingOption :: Lens.Lens' UpdateJob (Core.Maybe Types.ShippingOption)
ujShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE ujShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

-- | The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujSnowballCapacityPreference :: Lens.Lens' UpdateJob (Core.Maybe Types.SnowballCapacity)
ujSnowballCapacityPreference = Lens.field @"snowballCapacityPreference"
{-# INLINEABLE ujSnowballCapacityPreference #-}
{-# DEPRECATED snowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead"  #-}

instance Core.ToQuery UpdateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateJob where
        toHeaders UpdateJob{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.UpdateJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateJob where
        toJSON UpdateJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("AddressId" Core..=) Core.<$> addressId,
                  ("Description" Core..=) Core.<$> description,
                  ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
                  ("Notification" Core..=) Core.<$> notification,
                  ("Resources" Core..=) Core.<$> resources,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("ShippingOption" Core..=) Core.<$> shippingOption,
                  ("SnowballCapacityPreference" Core..=) Core.<$>
                    snowballCapacityPreference])

instance Core.AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateJobResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
newtype UpdateJobResponse = UpdateJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateJobResponse
mkUpdateJobResponse responseStatus
  = UpdateJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJobResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ujrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
