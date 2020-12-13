{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @VerifyEmailIdentity@ operation to verify a new email address.
module Network.AWS.SES.VerifyEmailAddress
  ( -- * Creating a request
    VerifyEmailAddress (..),
    mkVerifyEmailAddress,

    -- ** Request lenses
    veaEmailAddress,

    -- * Destructuring the response
    VerifyEmailAddressResponse (..),
    mkVerifyEmailAddressResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkVerifyEmailAddress' smart constructor.
newtype VerifyEmailAddress = VerifyEmailAddress'
  { -- | The email address to be verified.
    emailAddress :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyEmailAddress' with the minimum fields required to make a request.
--
-- * 'emailAddress' - The email address to be verified.
mkVerifyEmailAddress ::
  -- | 'emailAddress'
  Lude.Text ->
  VerifyEmailAddress
mkVerifyEmailAddress pEmailAddress_ =
  VerifyEmailAddress' {emailAddress = pEmailAddress_}

-- | The email address to be verified.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veaEmailAddress :: Lens.Lens' VerifyEmailAddress Lude.Text
veaEmailAddress = Lens.lens (emailAddress :: VerifyEmailAddress -> Lude.Text) (\s a -> s {emailAddress = a} :: VerifyEmailAddress)
{-# DEPRECATED veaEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

instance Lude.AWSRequest VerifyEmailAddress where
  type Rs VerifyEmailAddress = VerifyEmailAddressResponse
  request = Req.postQuery sesService
  response = Res.receiveNull VerifyEmailAddressResponse'

instance Lude.ToHeaders VerifyEmailAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath VerifyEmailAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifyEmailAddress where
  toQuery VerifyEmailAddress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("VerifyEmailAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EmailAddress" Lude.=: emailAddress
      ]

-- | /See:/ 'mkVerifyEmailAddressResponse' smart constructor.
data VerifyEmailAddressResponse = VerifyEmailAddressResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyEmailAddressResponse' with the minimum fields required to make a request.
mkVerifyEmailAddressResponse ::
  VerifyEmailAddressResponse
mkVerifyEmailAddressResponse = VerifyEmailAddressResponse'
