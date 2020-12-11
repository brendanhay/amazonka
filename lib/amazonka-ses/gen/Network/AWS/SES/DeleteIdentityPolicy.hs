{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified sending authorization policy for the given identity (an email address or a domain). This API returns successfully even if a policy with the specified name does not exist.
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentityPolicy
  ( -- * Creating a request
    DeleteIdentityPolicy (..),
    mkDeleteIdentityPolicy,

    -- ** Request lenses
    dipIdentity,
    dipPolicyName,

    -- * Destructuring the response
    DeleteIdentityPolicyResponse (..),
    mkDeleteIdentityPolicyResponse,

    -- ** Response lenses
    diprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteIdentityPolicy' smart constructor.
data DeleteIdentityPolicy = DeleteIdentityPolicy'
  { identity ::
      Lude.Text,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityPolicy' with the minimum fields required to make a request.
--
-- * 'identity' - The identity that is associated with the policy that you want to delete. You can specify the identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
-- * 'policyName' - The name of the policy to be deleted.
mkDeleteIdentityPolicy ::
  -- | 'identity'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  DeleteIdentityPolicy
mkDeleteIdentityPolicy pIdentity_ pPolicyName_ =
  DeleteIdentityPolicy'
    { identity = pIdentity_,
      policyName = pPolicyName_
    }

-- | The identity that is associated with the policy that you want to delete. You can specify the identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipIdentity :: Lens.Lens' DeleteIdentityPolicy Lude.Text
dipIdentity = Lens.lens (identity :: DeleteIdentityPolicy -> Lude.Text) (\s a -> s {identity = a} :: DeleteIdentityPolicy)
{-# DEPRECATED dipIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The name of the policy to be deleted.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipPolicyName :: Lens.Lens' DeleteIdentityPolicy Lude.Text
dipPolicyName = Lens.lens (policyName :: DeleteIdentityPolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeleteIdentityPolicy)
{-# DEPRECATED dipPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest DeleteIdentityPolicy where
  type Rs DeleteIdentityPolicy = DeleteIdentityPolicyResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteIdentityPolicyResult"
      ( \s h x ->
          DeleteIdentityPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIdentityPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteIdentityPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIdentityPolicy where
  toQuery DeleteIdentityPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteIdentityPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity,
        "PolicyName" Lude.=: policyName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteIdentityPolicyResponse' smart constructor.
newtype DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse'
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

-- | Creates a value of 'DeleteIdentityPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteIdentityPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIdentityPolicyResponse
mkDeleteIdentityPolicyResponse pResponseStatus_ =
  DeleteIdentityPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DeleteIdentityPolicyResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DeleteIdentityPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIdentityPolicyResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
