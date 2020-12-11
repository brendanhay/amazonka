{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateEmergencyContactSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of the list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.UpdateEmergencyContactSettings
  ( -- * Creating a request
    UpdateEmergencyContactSettings (..),
    mkUpdateEmergencyContactSettings,

    -- ** Request lenses
    uecsEmergencyContactList,

    -- * Destructuring the response
    UpdateEmergencyContactSettingsResponse (..),
    mkUpdateEmergencyContactSettingsResponse,

    -- ** Response lenses
    uecsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkUpdateEmergencyContactSettings' smart constructor.
newtype UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { emergencyContactList ::
      Lude.Maybe
        [EmergencyContact]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEmergencyContactSettings' with the minimum fields required to make a request.
--
-- * 'emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include at least one phone number.
mkUpdateEmergencyContactSettings ::
  UpdateEmergencyContactSettings
mkUpdateEmergencyContactSettings =
  UpdateEmergencyContactSettings'
    { emergencyContactList =
        Lude.Nothing
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include at least one phone number.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsEmergencyContactList :: Lens.Lens' UpdateEmergencyContactSettings (Lude.Maybe [EmergencyContact])
uecsEmergencyContactList = Lens.lens (emergencyContactList :: UpdateEmergencyContactSettings -> Lude.Maybe [EmergencyContact]) (\s a -> s {emergencyContactList = a} :: UpdateEmergencyContactSettings)
{-# DEPRECATED uecsEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

instance Lude.AWSRequest UpdateEmergencyContactSettings where
  type
    Rs UpdateEmergencyContactSettings =
      UpdateEmergencyContactSettingsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateEmergencyContactSettingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEmergencyContactSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.UpdateEmergencyContactSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEmergencyContactSettings where
  toJSON UpdateEmergencyContactSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("EmergencyContactList" Lude..=) Lude.<$> emergencyContactList]
      )

instance Lude.ToPath UpdateEmergencyContactSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEmergencyContactSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEmergencyContactSettingsResponse' smart constructor.
newtype UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
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

-- | Creates a value of 'UpdateEmergencyContactSettingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateEmergencyContactSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEmergencyContactSettingsResponse
mkUpdateEmergencyContactSettingsResponse pResponseStatus_ =
  UpdateEmergencyContactSettingsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsrsResponseStatus :: Lens.Lens' UpdateEmergencyContactSettingsResponse Lude.Int
uecsrsResponseStatus = Lens.lens (responseStatus :: UpdateEmergencyContactSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEmergencyContactSettingsResponse)
{-# DEPRECATED uecsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
