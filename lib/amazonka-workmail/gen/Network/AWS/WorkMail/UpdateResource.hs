{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates data for the resource. To have the latest information, it must be preceded by a 'DescribeResource' call. The dataset in the request should be the one expected when performing another @DescribeResource@ call.
module Network.AWS.WorkMail.UpdateResource
  ( -- * Creating a request
    UpdateResource (..),
    mkUpdateResource,

    -- ** Request lenses
    urName,
    urBookingOptions,
    urOrganizationId,
    urResourceId,

    -- * Destructuring the response
    UpdateResourceResponse (..),
    mkUpdateResourceResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { name :: Lude.Maybe Lude.Text,
    bookingOptions :: Lude.Maybe BookingOptions,
    organizationId :: Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResource' with the minimum fields required to make a request.
--
-- * 'bookingOptions' - The resource's booking options to be updated.
-- * 'name' - The name of the resource to be updated.
-- * 'organizationId' - The identifier associated with the organization for which the resource is updated.
-- * 'resourceId' - The identifier of the resource to be updated.
mkUpdateResource ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  UpdateResource
mkUpdateResource pOrganizationId_ pResourceId_ =
  UpdateResource'
    { name = Lude.Nothing,
      bookingOptions = Lude.Nothing,
      organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The name of the resource to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urName :: Lens.Lens' UpdateResource (Lude.Maybe Lude.Text)
urName = Lens.lens (name :: UpdateResource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateResource)
{-# DEPRECATED urName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The resource's booking options to be updated.
--
-- /Note:/ Consider using 'bookingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urBookingOptions :: Lens.Lens' UpdateResource (Lude.Maybe BookingOptions)
urBookingOptions = Lens.lens (bookingOptions :: UpdateResource -> Lude.Maybe BookingOptions) (\s a -> s {bookingOptions = a} :: UpdateResource)
{-# DEPRECATED urBookingOptions "Use generic-lens or generic-optics with 'bookingOptions' instead." #-}

-- | The identifier associated with the organization for which the resource is updated.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urOrganizationId :: Lens.Lens' UpdateResource Lude.Text
urOrganizationId = Lens.lens (organizationId :: UpdateResource -> Lude.Text) (\s a -> s {organizationId = a} :: UpdateResource)
{-# DEPRECATED urOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the resource to be updated.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UpdateResource Lude.Text
urResourceId = Lens.lens (resourceId :: UpdateResource -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateResource)
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest UpdateResource where
  type Rs UpdateResource = UpdateResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.UpdateResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("BookingOptions" Lude..=) Lude.<$> bookingOptions,
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath UpdateResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateResourceResponse' smart constructor.
newtype UpdateResourceResponse = UpdateResourceResponse'
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

-- | Creates a value of 'UpdateResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateResourceResponse
mkUpdateResourceResponse pResponseStatus_ =
  UpdateResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateResourceResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateResourceResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
