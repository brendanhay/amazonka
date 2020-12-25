{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujJobId,
    ujAddressId,
    ujDescription,
    ujForwardingAddressId,
    ujNotification,
    ujResources,
    ujRoleARN,
    ujShippingOption,
    ujSnowballCapacityPreference,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Types.JobId,
    -- | The ID of the updated 'Address' object.
    addressId :: Core.Maybe Types.AddressId,
    -- | The updated description of this job's 'JobMetadata' object.
    description :: Core.Maybe Types.Description,
    -- | The updated ID for the forwarding address for a job. This field is not supported in most regions.
    forwardingAddressId :: Core.Maybe Types.ForwardingAddressId,
    -- | The new or updated 'Notification' object.
    notification :: Core.Maybe Types.Notification,
    -- | The updated @JobResource@ object, or the updated 'JobResource' object.
    resources :: Core.Maybe Types.JobResource,
    -- | The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The updated shipping option value of this job's 'ShippingDetails' object.
    shippingOption :: Core.Maybe Types.ShippingOption,
    -- | The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
    snowballCapacityPreference :: Core.Maybe Types.SnowballCapacity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob ::
  -- | 'jobId'
  Types.JobId ->
  UpdateJob
mkUpdateJob jobId =
  UpdateJob'
    { jobId,
      addressId = Core.Nothing,
      description = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      notification = Core.Nothing,
      resources = Core.Nothing,
      roleARN = Core.Nothing,
      shippingOption = Core.Nothing,
      snowballCapacityPreference = Core.Nothing
    }

-- | The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Types.JobId
ujJobId = Lens.field @"jobId"
{-# DEPRECATED ujJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAddressId :: Lens.Lens' UpdateJob (Core.Maybe Types.AddressId)
ujAddressId = Lens.field @"addressId"
{-# DEPRECATED ujAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The updated description of this job's 'JobMetadata' object.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujDescription :: Lens.Lens' UpdateJob (Core.Maybe Types.Description)
ujDescription = Lens.field @"description"
{-# DEPRECATED ujDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated ID for the forwarding address for a job. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujForwardingAddressId :: Lens.Lens' UpdateJob (Core.Maybe Types.ForwardingAddressId)
ujForwardingAddressId = Lens.field @"forwardingAddressId"
{-# DEPRECATED ujForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujNotification :: Lens.Lens' UpdateJob (Core.Maybe Types.Notification)
ujNotification = Lens.field @"notification"
{-# DEPRECATED ujNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The updated @JobResource@ object, or the updated 'JobResource' object.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujResources :: Lens.Lens' UpdateJob (Core.Maybe Types.JobResource)
ujResources = Lens.field @"resources"
{-# DEPRECATED ujResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujRoleARN :: Lens.Lens' UpdateJob (Core.Maybe Types.RoleARN)
ujRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ujRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The updated shipping option value of this job's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujShippingOption :: Lens.Lens' UpdateJob (Core.Maybe Types.ShippingOption)
ujShippingOption = Lens.field @"shippingOption"
{-# DEPRECATED ujShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujSnowballCapacityPreference :: Lens.Lens' UpdateJob (Core.Maybe Types.SnowballCapacity)
ujSnowballCapacityPreference = Lens.field @"snowballCapacityPreference"
{-# DEPRECATED ujSnowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead." #-}

instance Core.FromJSON UpdateJob where
  toJSON UpdateJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            ("AddressId" Core..=) Core.<$> addressId,
            ("Description" Core..=) Core.<$> description,
            ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
            ("Notification" Core..=) Core.<$> notification,
            ("Resources" Core..=) Core.<$> resources,
            ("RoleARN" Core..=) Core.<$> roleARN,
            ("ShippingOption" Core..=) Core.<$> shippingOption,
            ("SnowballCapacityPreference" Core..=)
              Core.<$> snowballCapacityPreference
          ]
      )

instance Core.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSIESnowballJobManagementService.UpdateJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateJobResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
newtype UpdateJobResponse = UpdateJobResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateJobResponse
mkUpdateJobResponse responseStatus =
  UpdateJobResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJobResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
