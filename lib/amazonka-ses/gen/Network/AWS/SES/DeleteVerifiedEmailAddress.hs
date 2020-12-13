{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @DeleteIdentity@ operation to delete email addresses and domains.
module Network.AWS.SES.DeleteVerifiedEmailAddress
  ( -- * Creating a request
    DeleteVerifiedEmailAddress (..),
    mkDeleteVerifiedEmailAddress,

    -- ** Request lenses
    dveaEmailAddress,

    -- * Destructuring the response
    DeleteVerifiedEmailAddressResponse (..),
    mkDeleteVerifiedEmailAddressResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete an email address from the list of email addresses you have attempted to verify under your AWS account.
--
-- /See:/ 'mkDeleteVerifiedEmailAddress' smart constructor.
newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress'
  { -- | An email address to be removed from the list of verified addresses.
    emailAddress :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVerifiedEmailAddress' with the minimum fields required to make a request.
--
-- * 'emailAddress' - An email address to be removed from the list of verified addresses.
mkDeleteVerifiedEmailAddress ::
  -- | 'emailAddress'
  Lude.Text ->
  DeleteVerifiedEmailAddress
mkDeleteVerifiedEmailAddress pEmailAddress_ =
  DeleteVerifiedEmailAddress' {emailAddress = pEmailAddress_}

-- | An email address to be removed from the list of verified addresses.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveaEmailAddress :: Lens.Lens' DeleteVerifiedEmailAddress Lude.Text
dveaEmailAddress = Lens.lens (emailAddress :: DeleteVerifiedEmailAddress -> Lude.Text) (\s a -> s {emailAddress = a} :: DeleteVerifiedEmailAddress)
{-# DEPRECATED dveaEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

instance Lude.AWSRequest DeleteVerifiedEmailAddress where
  type
    Rs DeleteVerifiedEmailAddress =
      DeleteVerifiedEmailAddressResponse
  request = Req.postQuery sesService
  response = Res.receiveNull DeleteVerifiedEmailAddressResponse'

instance Lude.ToHeaders DeleteVerifiedEmailAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVerifiedEmailAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVerifiedEmailAddress where
  toQuery DeleteVerifiedEmailAddress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteVerifiedEmailAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EmailAddress" Lude.=: emailAddress
      ]

-- | /See:/ 'mkDeleteVerifiedEmailAddressResponse' smart constructor.
data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVerifiedEmailAddressResponse' with the minimum fields required to make a request.
mkDeleteVerifiedEmailAddressResponse ::
  DeleteVerifiedEmailAddressResponse
mkDeleteVerifiedEmailAddressResponse =
  DeleteVerifiedEmailAddressResponse'
