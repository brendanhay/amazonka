{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decodes additional information about the authorization status of a request from an encoded message returned in response to an AWS request.
--
-- For example, if a user is not authorized to perform an operation that he or she has requested, the request returns a @Client.UnauthorizedOperation@ response (an HTTP 403 response). Some AWS operations additionally return an encoded message that can provide details about this authorization failure.
-- The message is encoded because the details of the authorization status can constitute privileged information that the user who requested the operation should not see. To decode an authorization status message, a user must be granted permissions via an IAM policy to request the @DecodeAuthorizationMessage@ (@sts:DecodeAuthorizationMessage@ ) action.
-- The decoded message includes the following type of information:
--
--     * Whether the request was denied due to an explicit deny or due to the absence of an explicit allow. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-denyallow Determining Whether a Request is Allowed or Denied> in the /IAM User Guide/ .
--
--
--     * The principal who made the request.
--
--
--     * The requested action.
--
--
--     * The requested resource.
--
--
--     * The values of condition keys in the context of the user's request.
module Network.AWS.STS.DecodeAuthorizationMessage
  ( -- * Creating a request
    DecodeAuthorizationMessage (..),
    mkDecodeAuthorizationMessage,

    -- ** Request lenses
    damEncodedMessage,

    -- * Destructuring the response
    DecodeAuthorizationMessageResponse (..),
    mkDecodeAuthorizationMessageResponse,

    -- ** Response lenses
    damrsDecodedMessage,
    damrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkDecodeAuthorizationMessage' smart constructor.
newtype DecodeAuthorizationMessage = DecodeAuthorizationMessage'
  { encodedMessage ::
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

-- | Creates a value of 'DecodeAuthorizationMessage' with the minimum fields required to make a request.
--
-- * 'encodedMessage' - The encoded message that was returned with the response.
mkDecodeAuthorizationMessage ::
  -- | 'encodedMessage'
  Lude.Text ->
  DecodeAuthorizationMessage
mkDecodeAuthorizationMessage pEncodedMessage_ =
  DecodeAuthorizationMessage' {encodedMessage = pEncodedMessage_}

-- | The encoded message that was returned with the response.
--
-- /Note:/ Consider using 'encodedMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damEncodedMessage :: Lens.Lens' DecodeAuthorizationMessage Lude.Text
damEncodedMessage = Lens.lens (encodedMessage :: DecodeAuthorizationMessage -> Lude.Text) (\s a -> s {encodedMessage = a} :: DecodeAuthorizationMessage)
{-# DEPRECATED damEncodedMessage "Use generic-lens or generic-optics with 'encodedMessage' instead." #-}

instance Lude.AWSRequest DecodeAuthorizationMessage where
  type
    Rs DecodeAuthorizationMessage =
      DecodeAuthorizationMessageResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "DecodeAuthorizationMessageResult"
      ( \s h x ->
          DecodeAuthorizationMessageResponse'
            Lude.<$> (x Lude..@? "DecodedMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DecodeAuthorizationMessage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DecodeAuthorizationMessage where
  toPath = Lude.const "/"

instance Lude.ToQuery DecodeAuthorizationMessage where
  toQuery DecodeAuthorizationMessage' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DecodeAuthorizationMessage" :: Lude.ByteString),
        "Version" Lude.=: ("2011-06-15" :: Lude.ByteString),
        "EncodedMessage" Lude.=: encodedMessage
      ]

-- | A document that contains additional information about the authorization status of a request from an encoded message that is returned in response to an AWS request.
--
-- /See:/ 'mkDecodeAuthorizationMessageResponse' smart constructor.
data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse'
  { decodedMessage ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DecodeAuthorizationMessageResponse' with the minimum fields required to make a request.
--
-- * 'decodedMessage' - An XML document that contains the decoded message.
-- * 'responseStatus' - The response status code.
mkDecodeAuthorizationMessageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DecodeAuthorizationMessageResponse
mkDecodeAuthorizationMessageResponse pResponseStatus_ =
  DecodeAuthorizationMessageResponse'
    { decodedMessage =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An XML document that contains the decoded message.
--
-- /Note:/ Consider using 'decodedMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsDecodedMessage :: Lens.Lens' DecodeAuthorizationMessageResponse (Lude.Maybe Lude.Text)
damrsDecodedMessage = Lens.lens (decodedMessage :: DecodeAuthorizationMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {decodedMessage = a} :: DecodeAuthorizationMessageResponse)
{-# DEPRECATED damrsDecodedMessage "Use generic-lens or generic-optics with 'decodedMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsResponseStatus :: Lens.Lens' DecodeAuthorizationMessageResponse Lude.Int
damrsResponseStatus = Lens.lens (responseStatus :: DecodeAuthorizationMessageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DecodeAuthorizationMessageResponse)
{-# DEPRECATED damrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
