{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ujNotification,
    ujForwardingAddressId,
    ujAddressId,
    ujShippingOption,
    ujResources,
    ujDescription,
    ujRoleARN,
    ujSnowballCapacityPreference,
    ujJobId,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { notification ::
      Lude.Maybe Notification,
    forwardingAddressId :: Lude.Maybe Lude.Text,
    addressId :: Lude.Maybe Lude.Text,
    shippingOption :: Lude.Maybe ShippingOption,
    resources :: Lude.Maybe JobResource,
    description :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    snowballCapacityPreference :: Lude.Maybe SnowballCapacity,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- * 'addressId' - The ID of the updated 'Address' object.
-- * 'description' - The updated description of this job's 'JobMetadata' object.
-- * 'forwardingAddressId' - The updated ID for the forwarding address for a job. This field is not supported in most regions.
-- * 'jobId' - The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'notification' - The new or updated 'Notification' object.
-- * 'resources' - The updated @JobResource@ object, or the updated 'JobResource' object.
-- * 'roleARN' - The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
-- * 'shippingOption' - The updated shipping option value of this job's 'ShippingDetails' object.
-- * 'snowballCapacityPreference' - The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
mkUpdateJob ::
  -- | 'jobId'
  Lude.Text ->
  UpdateJob
mkUpdateJob pJobId_ =
  UpdateJob'
    { notification = Lude.Nothing,
      forwardingAddressId = Lude.Nothing,
      addressId = Lude.Nothing,
      shippingOption = Lude.Nothing,
      resources = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing,
      snowballCapacityPreference = Lude.Nothing,
      jobId = pJobId_
    }

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujNotification :: Lens.Lens' UpdateJob (Lude.Maybe Notification)
ujNotification = Lens.lens (notification :: UpdateJob -> Lude.Maybe Notification) (\s a -> s {notification = a} :: UpdateJob)
{-# DEPRECATED ujNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The updated ID for the forwarding address for a job. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujForwardingAddressId :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujForwardingAddressId = Lens.lens (forwardingAddressId :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: UpdateJob)
{-# DEPRECATED ujForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAddressId :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujAddressId = Lens.lens (addressId :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: UpdateJob)
{-# DEPRECATED ujAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The updated shipping option value of this job's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujShippingOption :: Lens.Lens' UpdateJob (Lude.Maybe ShippingOption)
ujShippingOption = Lens.lens (shippingOption :: UpdateJob -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: UpdateJob)
{-# DEPRECATED ujShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The updated @JobResource@ object, or the updated 'JobResource' object.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujResources :: Lens.Lens' UpdateJob (Lude.Maybe JobResource)
ujResources = Lens.lens (resources :: UpdateJob -> Lude.Maybe JobResource) (\s a -> s {resources = a} :: UpdateJob)
{-# DEPRECATED ujResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The updated description of this job's 'JobMetadata' object.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujDescription :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujDescription = Lens.lens (description :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateJob)
{-# DEPRECATED ujDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujRoleARN :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujRoleARN = Lens.lens (roleARN :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateJob)
{-# DEPRECATED ujRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujSnowballCapacityPreference :: Lens.Lens' UpdateJob (Lude.Maybe SnowballCapacity)
ujSnowballCapacityPreference = Lens.lens (snowballCapacityPreference :: UpdateJob -> Lude.Maybe SnowballCapacity) (\s a -> s {snowballCapacityPreference = a} :: UpdateJob)
{-# DEPRECATED ujSnowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead." #-}

-- | The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Lude.Text
ujJobId = Lens.lens (jobId :: UpdateJob -> Lude.Text) (\s a -> s {jobId = a} :: UpdateJob)
{-# DEPRECATED ujJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSIESnowballJobManagementService.UpdateJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Notification" Lude..=) Lude.<$> notification,
            ("ForwardingAddressId" Lude..=) Lude.<$> forwardingAddressId,
            ("AddressId" Lude..=) Lude.<$> addressId,
            ("ShippingOption" Lude..=) Lude.<$> shippingOption,
            ("Resources" Lude..=) Lude.<$> resources,
            ("Description" Lude..=) Lude.<$> description,
            ("RoleARN" Lude..=) Lude.<$> roleARN,
            ("SnowballCapacityPreference" Lude..=)
              Lude.<$> snowballCapacityPreference,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath UpdateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
newtype UpdateJobResponse = UpdateJobResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobResponse
mkUpdateJobResponse pResponseStatus_ =
  UpdateJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsResponseStatus :: Lens.Lens' UpdateJobResponse Lude.Int
ujrsResponseStatus = Lens.lens (responseStatus :: UpdateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
