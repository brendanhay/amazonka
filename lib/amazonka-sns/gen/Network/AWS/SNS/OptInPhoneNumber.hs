{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.OptInPhoneNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to opt in a phone number that is opted out, which enables you to resume sending SMS messages to the number.
--
-- You can opt in a phone number only once every 30 days.
module Network.AWS.SNS.OptInPhoneNumber
  ( -- * Creating a request
    OptInPhoneNumber (..),
    mkOptInPhoneNumber,

    -- ** Request lenses
    oipnPhoneNumber,

    -- * Destructuring the response
    OptInPhoneNumberResponse (..),
    mkOptInPhoneNumberResponse,

    -- ** Response lenses
    oipnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for the OptInPhoneNumber action.
--
-- /See:/ 'mkOptInPhoneNumber' smart constructor.
newtype OptInPhoneNumber = OptInPhoneNumber'
  { phoneNumber ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptInPhoneNumber' with the minimum fields required to make a request.
--
-- * 'phoneNumber' - The phone number to opt in.
mkOptInPhoneNumber ::
  -- | 'phoneNumber'
  Lude.Text ->
  OptInPhoneNumber
mkOptInPhoneNumber pPhoneNumber_ =
  OptInPhoneNumber' {phoneNumber = pPhoneNumber_}

-- | The phone number to opt in.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipnPhoneNumber :: Lens.Lens' OptInPhoneNumber Lude.Text
oipnPhoneNumber = Lens.lens (phoneNumber :: OptInPhoneNumber -> Lude.Text) (\s a -> s {phoneNumber = a} :: OptInPhoneNumber)
{-# DEPRECATED oipnPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Lude.AWSRequest OptInPhoneNumber where
  type Rs OptInPhoneNumber = OptInPhoneNumberResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "OptInPhoneNumberResult"
      ( \s h x ->
          OptInPhoneNumberResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders OptInPhoneNumber where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath OptInPhoneNumber where
  toPath = Lude.const "/"

instance Lude.ToQuery OptInPhoneNumber where
  toQuery OptInPhoneNumber' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("OptInPhoneNumber" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "phoneNumber" Lude.=: phoneNumber
      ]

-- | The response for the OptInPhoneNumber action.
--
-- /See:/ 'mkOptInPhoneNumberResponse' smart constructor.
newtype OptInPhoneNumberResponse = OptInPhoneNumberResponse'
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

-- | Creates a value of 'OptInPhoneNumberResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkOptInPhoneNumberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  OptInPhoneNumberResponse
mkOptInPhoneNumberResponse pResponseStatus_ =
  OptInPhoneNumberResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipnrsResponseStatus :: Lens.Lens' OptInPhoneNumberResponse Lude.Int
oipnrsResponseStatus = Lens.lens (responseStatus :: OptInPhoneNumberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: OptInPhoneNumberResponse)
{-# DEPRECATED oipnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
