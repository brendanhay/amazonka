{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateProactiveEngagementDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes proactive engagement and sets the list of contacts for the DDoS Response Team (DRT) to use. You must provide at least one phone number in the emergency contact list.
--
-- After you have initialized proactive engagement using this call, to disable or enable proactive engagement, use the calls @DisableProactiveEngagement@ and @EnableProactiveEngagement@ .
module Network.AWS.Shield.AssociateProactiveEngagementDetails
  ( -- * Creating a request
    AssociateProactiveEngagementDetails (..),
    mkAssociateProactiveEngagementDetails,

    -- ** Request lenses
    apedEmergencyContactList,

    -- * Destructuring the response
    AssociateProactiveEngagementDetailsResponse (..),
    mkAssociateProactiveEngagementDetailsResponse,

    -- ** Response lenses
    apedrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkAssociateProactiveEngagementDetails' smart constructor.
newtype AssociateProactiveEngagementDetails = AssociateProactiveEngagementDetails'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.
    --
    -- To enable proactive engagement, the contact list must include at least one phone number.
    emergencyContactList :: [EmergencyContact]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateProactiveEngagementDetails' with the minimum fields required to make a request.
--
-- * 'emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least one phone number.
mkAssociateProactiveEngagementDetails ::
  AssociateProactiveEngagementDetails
mkAssociateProactiveEngagementDetails =
  AssociateProactiveEngagementDetails'
    { emergencyContactList =
        Lude.mempty
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least one phone number.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apedEmergencyContactList :: Lens.Lens' AssociateProactiveEngagementDetails [EmergencyContact]
apedEmergencyContactList = Lens.lens (emergencyContactList :: AssociateProactiveEngagementDetails -> [EmergencyContact]) (\s a -> s {emergencyContactList = a} :: AssociateProactiveEngagementDetails)
{-# DEPRECATED apedEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

instance Lude.AWSRequest AssociateProactiveEngagementDetails where
  type
    Rs AssociateProactiveEngagementDetails =
      AssociateProactiveEngagementDetailsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateProactiveEngagementDetailsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateProactiveEngagementDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.AssociateProactiveEngagementDetails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateProactiveEngagementDetails where
  toJSON AssociateProactiveEngagementDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EmergencyContactList" Lude..= emergencyContactList)]
      )

instance Lude.ToPath AssociateProactiveEngagementDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateProactiveEngagementDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateProactiveEngagementDetailsResponse' smart constructor.
newtype AssociateProactiveEngagementDetailsResponse = AssociateProactiveEngagementDetailsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateProactiveEngagementDetailsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateProactiveEngagementDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateProactiveEngagementDetailsResponse
mkAssociateProactiveEngagementDetailsResponse pResponseStatus_ =
  AssociateProactiveEngagementDetailsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apedrsResponseStatus :: Lens.Lens' AssociateProactiveEngagementDetailsResponse Lude.Int
apedrsResponseStatus = Lens.lens (responseStatus :: AssociateProactiveEngagementDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateProactiveEngagementDetailsResponse)
{-# DEPRECATED apedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
