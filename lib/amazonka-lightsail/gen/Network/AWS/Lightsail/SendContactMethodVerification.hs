{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.SendContactMethodVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a verification request to an email contact method to ensure it's owned by the requester. SMS contact methods don't need to be verified.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
-- A verification request is sent to the contact method when you initially create it. Use this action to send another verification request if a previous verification request was deleted, or has expired.
-- /Important:/ Notifications are not sent to an email contact method until after it is verified, and confirmed as valid.
module Network.AWS.Lightsail.SendContactMethodVerification
  ( -- * Creating a request
    SendContactMethodVerification (..),
    mkSendContactMethodVerification,

    -- ** Request lenses
    scmvProtocol,

    -- * Destructuring the response
    SendContactMethodVerificationResponse (..),
    mkSendContactMethodVerificationResponse,

    -- ** Response lenses
    scmvrsOperations,
    scmvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendContactMethodVerification' smart constructor.
newtype SendContactMethodVerification = SendContactMethodVerification'
  { -- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
    protocol :: ContactMethodVerificationProtocol
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendContactMethodVerification' with the minimum fields required to make a request.
--
-- * 'protocol' - The protocol to verify, such as @Email@ or @SMS@ (text messaging).
mkSendContactMethodVerification ::
  -- | 'protocol'
  ContactMethodVerificationProtocol ->
  SendContactMethodVerification
mkSendContactMethodVerification pProtocol_ =
  SendContactMethodVerification' {protocol = pProtocol_}

-- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvProtocol :: Lens.Lens' SendContactMethodVerification ContactMethodVerificationProtocol
scmvProtocol = Lens.lens (protocol :: SendContactMethodVerification -> ContactMethodVerificationProtocol) (\s a -> s {protocol = a} :: SendContactMethodVerification)
{-# DEPRECATED scmvProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Lude.AWSRequest SendContactMethodVerification where
  type
    Rs SendContactMethodVerification =
      SendContactMethodVerificationResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          SendContactMethodVerificationResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendContactMethodVerification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.SendContactMethodVerification" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendContactMethodVerification where
  toJSON SendContactMethodVerification' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("protocol" Lude..= protocol)])

instance Lude.ToPath SendContactMethodVerification where
  toPath = Lude.const "/"

instance Lude.ToQuery SendContactMethodVerification where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendContactMethodVerificationResponse' smart constructor.
data SendContactMethodVerificationResponse = SendContactMethodVerificationResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendContactMethodVerificationResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkSendContactMethodVerificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendContactMethodVerificationResponse
mkSendContactMethodVerificationResponse pResponseStatus_ =
  SendContactMethodVerificationResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvrsOperations :: Lens.Lens' SendContactMethodVerificationResponse (Lude.Maybe [Operation])
scmvrsOperations = Lens.lens (operations :: SendContactMethodVerificationResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: SendContactMethodVerificationResponse)
{-# DEPRECATED scmvrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvrsResponseStatus :: Lens.Lens' SendContactMethodVerificationResponse Lude.Int
scmvrsResponseStatus = Lens.lens (responseStatus :: SendContactMethodVerificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendContactMethodVerificationResponse)
{-# DEPRECATED scmvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
