{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a phone number and indicates whether the phone holder has opted out of receiving SMS messages from your account. You cannot send SMS messages to a number that is opted out.
--
-- To resume sending messages, you can opt in the number by using the @OptInPhoneNumber@ action.
module Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
  ( -- * Creating a request
    CheckIfPhoneNumberIsOptedOut (..),
    mkCheckIfPhoneNumberIsOptedOut,

    -- ** Request lenses
    cipniooPhoneNumber,

    -- * Destructuring the response
    CheckIfPhoneNumberIsOptedOutResponse (..),
    mkCheckIfPhoneNumberIsOptedOutResponse,

    -- ** Response lenses
    cipnioorsIsOptedOut,
    cipnioorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | The input for the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'mkCheckIfPhoneNumberIsOptedOut' smart constructor.
newtype CheckIfPhoneNumberIsOptedOut = CheckIfPhoneNumberIsOptedOut'
  { -- | The phone number for which you want to check the opt out status.
    phoneNumber :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckIfPhoneNumberIsOptedOut' with the minimum fields required to make a request.
--
-- * 'phoneNumber' - The phone number for which you want to check the opt out status.
mkCheckIfPhoneNumberIsOptedOut ::
  -- | 'phoneNumber'
  Lude.Text ->
  CheckIfPhoneNumberIsOptedOut
mkCheckIfPhoneNumberIsOptedOut pPhoneNumber_ =
  CheckIfPhoneNumberIsOptedOut' {phoneNumber = pPhoneNumber_}

-- | The phone number for which you want to check the opt out status.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipniooPhoneNumber :: Lens.Lens' CheckIfPhoneNumberIsOptedOut Lude.Text
cipniooPhoneNumber = Lens.lens (phoneNumber :: CheckIfPhoneNumberIsOptedOut -> Lude.Text) (\s a -> s {phoneNumber = a} :: CheckIfPhoneNumberIsOptedOut)
{-# DEPRECATED cipniooPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Lude.AWSRequest CheckIfPhoneNumberIsOptedOut where
  type
    Rs CheckIfPhoneNumberIsOptedOut =
      CheckIfPhoneNumberIsOptedOutResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "CheckIfPhoneNumberIsOptedOutResult"
      ( \s h x ->
          CheckIfPhoneNumberIsOptedOutResponse'
            Lude.<$> (x Lude..@? "isOptedOut") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CheckIfPhoneNumberIsOptedOut where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CheckIfPhoneNumberIsOptedOut where
  toPath = Lude.const "/"

instance Lude.ToQuery CheckIfPhoneNumberIsOptedOut where
  toQuery CheckIfPhoneNumberIsOptedOut' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CheckIfPhoneNumberIsOptedOut" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "phoneNumber" Lude.=: phoneNumber
      ]

-- | The response from the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'mkCheckIfPhoneNumberIsOptedOutResponse' smart constructor.
data CheckIfPhoneNumberIsOptedOutResponse = CheckIfPhoneNumberIsOptedOutResponse'
  { -- | Indicates whether the phone number is opted out:
    --
    --
    --     * @true@ – The phone number is opted out, meaning you cannot publish SMS messages to it.
    --
    --
    --     * @false@ – The phone number is opted in, meaning you can publish SMS messages to it.
    isOptedOut :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckIfPhoneNumberIsOptedOutResponse' with the minimum fields required to make a request.
--
-- * 'isOptedOut' - Indicates whether the phone number is opted out:
--
--
--     * @true@ – The phone number is opted out, meaning you cannot publish SMS messages to it.
--
--
--     * @false@ – The phone number is opted in, meaning you can publish SMS messages to it.
--
--
-- * 'responseStatus' - The response status code.
mkCheckIfPhoneNumberIsOptedOutResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CheckIfPhoneNumberIsOptedOutResponse
mkCheckIfPhoneNumberIsOptedOutResponse pResponseStatus_ =
  CheckIfPhoneNumberIsOptedOutResponse'
    { isOptedOut = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the phone number is opted out:
--
--
--     * @true@ – The phone number is opted out, meaning you cannot publish SMS messages to it.
--
--
--     * @false@ – The phone number is opted in, meaning you can publish SMS messages to it.
--
--
--
-- /Note:/ Consider using 'isOptedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipnioorsIsOptedOut :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse (Lude.Maybe Lude.Bool)
cipnioorsIsOptedOut = Lens.lens (isOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isOptedOut = a} :: CheckIfPhoneNumberIsOptedOutResponse)
{-# DEPRECATED cipnioorsIsOptedOut "Use generic-lens or generic-optics with 'isOptedOut' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipnioorsResponseStatus :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse Lude.Int
cipnioorsResponseStatus = Lens.lens (responseStatus :: CheckIfPhoneNumberIsOptedOutResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CheckIfPhoneNumberIsOptedOutResponse)
{-# DEPRECATED cipnioorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
