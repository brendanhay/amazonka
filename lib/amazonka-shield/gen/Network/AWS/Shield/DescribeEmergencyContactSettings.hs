{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeEmergencyContactSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.DescribeEmergencyContactSettings
  ( -- * Creating a request
    DescribeEmergencyContactSettings (..),
    mkDescribeEmergencyContactSettings,

    -- * Destructuring the response
    DescribeEmergencyContactSettingsResponse (..),
    mkDescribeEmergencyContactSettingsResponse,

    -- ** Response lenses
    decsrsEmergencyContactList,
    decsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeEmergencyContactSettings' smart constructor.
data DescribeEmergencyContactSettings = DescribeEmergencyContactSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEmergencyContactSettings' with the minimum fields required to make a request.
mkDescribeEmergencyContactSettings ::
  DescribeEmergencyContactSettings
mkDescribeEmergencyContactSettings =
  DescribeEmergencyContactSettings'

instance Lude.AWSRequest DescribeEmergencyContactSettings where
  type
    Rs DescribeEmergencyContactSettings =
      DescribeEmergencyContactSettingsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEmergencyContactSettingsResponse'
            Lude.<$> (x Lude..?> "EmergencyContactList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEmergencyContactSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.DescribeEmergencyContactSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEmergencyContactSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeEmergencyContactSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEmergencyContactSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEmergencyContactSettingsResponse' smart constructor.
data DescribeEmergencyContactSettingsResponse = DescribeEmergencyContactSettingsResponse'
  { emergencyContactList ::
      Lude.Maybe
        [EmergencyContact],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEmergencyContactSettingsResponse' with the minimum fields required to make a request.
--
-- * 'emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
-- * 'responseStatus' - The response status code.
mkDescribeEmergencyContactSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEmergencyContactSettingsResponse
mkDescribeEmergencyContactSettingsResponse pResponseStatus_ =
  DescribeEmergencyContactSettingsResponse'
    { emergencyContactList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrsEmergencyContactList :: Lens.Lens' DescribeEmergencyContactSettingsResponse (Lude.Maybe [EmergencyContact])
decsrsEmergencyContactList = Lens.lens (emergencyContactList :: DescribeEmergencyContactSettingsResponse -> Lude.Maybe [EmergencyContact]) (\s a -> s {emergencyContactList = a} :: DescribeEmergencyContactSettingsResponse)
{-# DEPRECATED decsrsEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrsResponseStatus :: Lens.Lens' DescribeEmergencyContactSettingsResponse Lude.Int
decsrsResponseStatus = Lens.lens (responseStatus :: DescribeEmergencyContactSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEmergencyContactSettingsResponse)
{-# DEPRECATED decsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
