{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the resource.
module Network.AWS.WorkMail.DescribeResource
  ( -- * Creating a request
    DescribeResource (..),
    mkDescribeResource,

    -- ** Request lenses
    drOrganizationId,
    drResourceId,

    -- * Destructuring the response
    DescribeResourceResponse (..),
    mkDescribeResourceResponse,

    -- ** Response lenses
    drrsEmail,
    drrsState,
    drrsResourceId,
    drrsDisabledDate,
    drrsName,
    drrsType,
    drrsEnabledDate,
    drrsBookingOptions,
    drrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeResource' smart constructor.
data DescribeResource = DescribeResource'
  { organizationId ::
      Lude.Text,
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

-- | Creates a value of 'DescribeResource' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier associated with the organization for which the resource is described.
-- * 'resourceId' - The identifier of the resource to be described.
mkDescribeResource ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  DescribeResource
mkDescribeResource pOrganizationId_ pResourceId_ =
  DescribeResource'
    { organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The identifier associated with the organization for which the resource is described.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drOrganizationId :: Lens.Lens' DescribeResource Lude.Text
drOrganizationId = Lens.lens (organizationId :: DescribeResource -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeResource)
{-# DEPRECATED drOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the resource to be described.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drResourceId :: Lens.Lens' DescribeResource Lude.Text
drResourceId = Lens.lens (resourceId :: DescribeResource -> Lude.Text) (\s a -> s {resourceId = a} :: DescribeResource)
{-# DEPRECATED drResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DescribeResource where
  type Rs DescribeResource = DescribeResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourceResponse'
            Lude.<$> (x Lude..?> "Email")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ResourceId")
            Lude.<*> (x Lude..?> "DisabledDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Type")
            Lude.<*> (x Lude..?> "EnabledDate")
            Lude.<*> (x Lude..?> "BookingOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DescribeResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeResource where
  toJSON DescribeResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath DescribeResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { email ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe EntityState,
    resourceId :: Lude.Maybe Lude.Text,
    disabledDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ResourceType,
    enabledDate :: Lude.Maybe Lude.Timestamp,
    bookingOptions ::
      Lude.Maybe BookingOptions,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceResponse' with the minimum fields required to make a request.
--
-- * 'bookingOptions' - The booking options for the described resource.
-- * 'disabledDate' - The date and time when a resource was disabled from WorkMail, in UNIX epoch time format.
-- * 'email' - The email of the described resource.
-- * 'enabledDate' - The date and time when a resource was enabled for WorkMail, in UNIX epoch time format.
-- * 'name' - The name of the described resource.
-- * 'resourceId' - The identifier of the described resource.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the resource: enabled (registered to Amazon WorkMail), disabled (deregistered or never registered to WorkMail), or deleted.
-- * 'type'' - The type of the described resource.
mkDescribeResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeResourceResponse
mkDescribeResourceResponse pResponseStatus_ =
  DescribeResourceResponse'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      resourceId = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      enabledDate = Lude.Nothing,
      bookingOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The email of the described resource.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEmail :: Lens.Lens' DescribeResourceResponse (Lude.Maybe Lude.Text)
drrsEmail = Lens.lens (email :: DescribeResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the resource: enabled (registered to Amazon WorkMail), disabled (deregistered or never registered to WorkMail), or deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsState :: Lens.Lens' DescribeResourceResponse (Lude.Maybe EntityState)
drrsState = Lens.lens (state :: DescribeResourceResponse -> Lude.Maybe EntityState) (\s a -> s {state = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier of the described resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResourceId :: Lens.Lens' DescribeResourceResponse (Lude.Maybe Lude.Text)
drrsResourceId = Lens.lens (resourceId :: DescribeResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The date and time when a resource was disabled from WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsDisabledDate :: Lens.Lens' DescribeResourceResponse (Lude.Maybe Lude.Timestamp)
drrsDisabledDate = Lens.lens (disabledDate :: DescribeResourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the described resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsName :: Lens.Lens' DescribeResourceResponse (Lude.Maybe Lude.Text)
drrsName = Lens.lens (name :: DescribeResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the described resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsType :: Lens.Lens' DescribeResourceResponse (Lude.Maybe ResourceType)
drrsType = Lens.lens (type' :: DescribeResourceResponse -> Lude.Maybe ResourceType) (\s a -> s {type' = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date and time when a resource was enabled for WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEnabledDate :: Lens.Lens' DescribeResourceResponse (Lude.Maybe Lude.Timestamp)
drrsEnabledDate = Lens.lens (enabledDate :: DescribeResourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The booking options for the described resource.
--
-- /Note:/ Consider using 'bookingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsBookingOptions :: Lens.Lens' DescribeResourceResponse (Lude.Maybe BookingOptions)
drrsBookingOptions = Lens.lens (bookingOptions :: DescribeResourceResponse -> Lude.Maybe BookingOptions) (\s a -> s {bookingOptions = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsBookingOptions "Use generic-lens or generic-optics with 'bookingOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeResourceResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourceResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
