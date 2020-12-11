{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.AcceptHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a response to the originator of a handshake agreeing to the action proposed by the handshake request.
--
-- This operation can be called only by the following principals when they also have the relevant IAM permissions:
--
--     * __Invitation to join__ or __Approve all features request__ handshakes: only a principal from the member account.
-- The user who calls the API for an invitation to join must have the @organizations:AcceptHandshake@ permission. If you enabled all features in the organization, the user must also have the @iam:CreateServiceLinkedRole@ permission so that AWS Organizations can create the required service-linked role named @AWSServiceRoleForOrganizations@ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide/ .
--
--
--     * __Enable all features final confirmation__ handshake: only a principal from the management account.
-- For more information about invitations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html Inviting an AWS Account to Join Your Organization> in the /AWS Organizations User Guide./ For more information about requests to enable all features in the organization, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide./
--
--
-- After you accept a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that, it's deleted.
module Network.AWS.Organizations.AcceptHandshake
  ( -- * Creating a request
    AcceptHandshake (..),
    mkAcceptHandshake,

    -- ** Request lenses
    ahHandshakeId,

    -- * Destructuring the response
    AcceptHandshakeResponse (..),
    mkAcceptHandshakeResponse,

    -- ** Response lenses
    ahrsHandshake,
    ahrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptHandshake' smart constructor.
newtype AcceptHandshake = AcceptHandshake'
  { handshakeId ::
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

-- | Creates a value of 'AcceptHandshake' with the minimum fields required to make a request.
--
-- * 'handshakeId' - The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
mkAcceptHandshake ::
  -- | 'handshakeId'
  Lude.Text ->
  AcceptHandshake
mkAcceptHandshake pHandshakeId_ =
  AcceptHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahHandshakeId :: Lens.Lens' AcceptHandshake Lude.Text
ahHandshakeId = Lens.lens (handshakeId :: AcceptHandshake -> Lude.Text) (\s a -> s {handshakeId = a} :: AcceptHandshake)
{-# DEPRECATED ahHandshakeId "Use generic-lens or generic-optics with 'handshakeId' instead." #-}

instance Lude.AWSRequest AcceptHandshake where
  type Rs AcceptHandshake = AcceptHandshakeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcceptHandshakeResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptHandshake where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.AcceptHandshake" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptHandshake where
  toJSON AcceptHandshake' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HandshakeId" Lude..= handshakeId)])

instance Lude.ToPath AcceptHandshake where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptHandshake where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptHandshakeResponse' smart constructor.
data AcceptHandshakeResponse = AcceptHandshakeResponse'
  { handshake ::
      Lude.Maybe Handshake,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptHandshakeResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains details about the accepted handshake.
-- * 'responseStatus' - The response status code.
mkAcceptHandshakeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptHandshakeResponse
mkAcceptHandshakeResponse pResponseStatus_ =
  AcceptHandshakeResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the accepted handshake.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrsHandshake :: Lens.Lens' AcceptHandshakeResponse (Lude.Maybe Handshake)
ahrsHandshake = Lens.lens (handshake :: AcceptHandshakeResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: AcceptHandshakeResponse)
{-# DEPRECATED ahrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrsResponseStatus :: Lens.Lens' AcceptHandshakeResponse Lude.Int
ahrsResponseStatus = Lens.lens (responseStatus :: AcceptHandshakeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptHandshakeResponse)
{-# DEPRECATED ahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
