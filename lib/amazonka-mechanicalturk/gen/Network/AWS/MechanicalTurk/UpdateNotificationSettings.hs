{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateNotificationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateNotificationSettings@ operation creates, updates, disables or re-enables notifications for a HIT type. If you call the UpdateNotificationSettings operation for a HIT type that already has a notification specification, the operation replaces the old specification with a new one. You can call the UpdateNotificationSettings operation to enable or disable notifications for the HIT type, without having to modify the notification specification itself by providing updates to the Active status without specifying a new notification specification. To change the Active status of a HIT type's notifications, the HIT type must already have a notification specification, or one must be provided in the same call to @UpdateNotificationSettings@ .
module Network.AWS.MechanicalTurk.UpdateNotificationSettings
  ( -- * Creating a request
    UpdateNotificationSettings (..),
    mkUpdateNotificationSettings,

    -- ** Request lenses
    unsNotification,
    unsHITTypeId,
    unsActive,

    -- * Destructuring the response
    UpdateNotificationSettingsResponse (..),
    mkUpdateNotificationSettingsResponse,

    -- ** Response lenses
    unsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateNotificationSettings' smart constructor.
data UpdateNotificationSettings = UpdateNotificationSettings'
  { -- | The notification specification for the HIT type.
    notification :: Lude.Maybe NotificationSpecification,
    -- | The ID of the HIT type whose notification specification is being updated.
    hITTypeId :: Lude.Text,
    -- | Specifies whether notifications are sent for HITs of this HIT type, according to the notification specification. You must specify either the Notification parameter or the Active parameter for the call to UpdateNotificationSettings to succeed.
    active :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotificationSettings' with the minimum fields required to make a request.
--
-- * 'notification' - The notification specification for the HIT type.
-- * 'hITTypeId' - The ID of the HIT type whose notification specification is being updated.
-- * 'active' - Specifies whether notifications are sent for HITs of this HIT type, according to the notification specification. You must specify either the Notification parameter or the Active parameter for the call to UpdateNotificationSettings to succeed.
mkUpdateNotificationSettings ::
  -- | 'hITTypeId'
  Lude.Text ->
  UpdateNotificationSettings
mkUpdateNotificationSettings pHITTypeId_ =
  UpdateNotificationSettings'
    { notification = Lude.Nothing,
      hITTypeId = pHITTypeId_,
      active = Lude.Nothing
    }

-- | The notification specification for the HIT type.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsNotification :: Lens.Lens' UpdateNotificationSettings (Lude.Maybe NotificationSpecification)
unsNotification = Lens.lens (notification :: UpdateNotificationSettings -> Lude.Maybe NotificationSpecification) (\s a -> s {notification = a} :: UpdateNotificationSettings)
{-# DEPRECATED unsNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The ID of the HIT type whose notification specification is being updated.
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsHITTypeId :: Lens.Lens' UpdateNotificationSettings Lude.Text
unsHITTypeId = Lens.lens (hITTypeId :: UpdateNotificationSettings -> Lude.Text) (\s a -> s {hITTypeId = a} :: UpdateNotificationSettings)
{-# DEPRECATED unsHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | Specifies whether notifications are sent for HITs of this HIT type, according to the notification specification. You must specify either the Notification parameter or the Active parameter for the call to UpdateNotificationSettings to succeed.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsActive :: Lens.Lens' UpdateNotificationSettings (Lude.Maybe Lude.Bool)
unsActive = Lens.lens (active :: UpdateNotificationSettings -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: UpdateNotificationSettings)
{-# DEPRECATED unsActive "Use generic-lens or generic-optics with 'active' instead." #-}

instance Lude.AWSRequest UpdateNotificationSettings where
  type
    Rs UpdateNotificationSettings =
      UpdateNotificationSettingsResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNotificationSettingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNotificationSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.UpdateNotificationSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNotificationSettings where
  toJSON UpdateNotificationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Notification" Lude..=) Lude.<$> notification,
            Lude.Just ("HITTypeId" Lude..= hITTypeId),
            ("Active" Lude..=) Lude.<$> active
          ]
      )

instance Lude.ToPath UpdateNotificationSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNotificationSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNotificationSettingsResponse' smart constructor.
newtype UpdateNotificationSettingsResponse = UpdateNotificationSettingsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotificationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNotificationSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNotificationSettingsResponse
mkUpdateNotificationSettingsResponse pResponseStatus_ =
  UpdateNotificationSettingsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsrsResponseStatus :: Lens.Lens' UpdateNotificationSettingsResponse Lude.Int
unsrsResponseStatus = Lens.lens (responseStatus :: UpdateNotificationSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNotificationSettingsResponse)
{-# DEPRECATED unsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
