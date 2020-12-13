{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES account in the current AWS region and attempts to verify it. As a result of executing this operation, a verification email is sent to the specified address.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.VerifyEmailIdentity
  ( -- * Creating a request
    VerifyEmailIdentity (..),
    mkVerifyEmailIdentity,

    -- ** Request lenses
    veiEmailAddress,

    -- * Destructuring the response
    VerifyEmailIdentityResponse (..),
    mkVerifyEmailIdentityResponse,

    -- ** Response lenses
    veirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkVerifyEmailIdentity' smart constructor.
newtype VerifyEmailIdentity = VerifyEmailIdentity'
  { -- | The email address to be verified.
    emailAddress :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyEmailIdentity' with the minimum fields required to make a request.
--
-- * 'emailAddress' - The email address to be verified.
mkVerifyEmailIdentity ::
  -- | 'emailAddress'
  Lude.Text ->
  VerifyEmailIdentity
mkVerifyEmailIdentity pEmailAddress_ =
  VerifyEmailIdentity' {emailAddress = pEmailAddress_}

-- | The email address to be verified.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veiEmailAddress :: Lens.Lens' VerifyEmailIdentity Lude.Text
veiEmailAddress = Lens.lens (emailAddress :: VerifyEmailIdentity -> Lude.Text) (\s a -> s {emailAddress = a} :: VerifyEmailIdentity)
{-# DEPRECATED veiEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

instance Lude.AWSRequest VerifyEmailIdentity where
  type Rs VerifyEmailIdentity = VerifyEmailIdentityResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "VerifyEmailIdentityResult"
      ( \s h x ->
          VerifyEmailIdentityResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders VerifyEmailIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath VerifyEmailIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifyEmailIdentity where
  toQuery VerifyEmailIdentity' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("VerifyEmailIdentity" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EmailAddress" Lude.=: emailAddress
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkVerifyEmailIdentityResponse' smart constructor.
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyEmailIdentityResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkVerifyEmailIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  VerifyEmailIdentityResponse
mkVerifyEmailIdentityResponse pResponseStatus_ =
  VerifyEmailIdentityResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veirsResponseStatus :: Lens.Lens' VerifyEmailIdentityResponse Lude.Int
veirsResponseStatus = Lens.lens (responseStatus :: VerifyEmailIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: VerifyEmailIdentityResponse)
{-# DEPRECATED veirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
