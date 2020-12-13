{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password policy for the AWS account. For more information about using a password policy, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy> .
module Network.AWS.IAM.GetAccountPasswordPolicy
  ( -- * Creating a request
    GetAccountPasswordPolicy (..),
    mkGetAccountPasswordPolicy,

    -- * Destructuring the response
    GetAccountPasswordPolicyResponse (..),
    mkGetAccountPasswordPolicyResponse,

    -- ** Response lenses
    gapprsPasswordPolicy,
    gapprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy = GetAccountPasswordPolicy'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountPasswordPolicy' with the minimum fields required to make a request.
mkGetAccountPasswordPolicy ::
  GetAccountPasswordPolicy
mkGetAccountPasswordPolicy = GetAccountPasswordPolicy'

instance Lude.AWSRequest GetAccountPasswordPolicy where
  type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetAccountPasswordPolicyResult"
      ( \s h x ->
          GetAccountPasswordPolicyResponse'
            Lude.<$> (x Lude..@ "PasswordPolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountPasswordPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountPasswordPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountPasswordPolicy where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetAccountPasswordPolicy" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'GetAccountPasswordPolicy' request.
--
-- /See:/ 'mkGetAccountPasswordPolicyResponse' smart constructor.
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
  { -- | A structure that contains details about the account's password policy.
    passwordPolicy :: PasswordPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountPasswordPolicyResponse' with the minimum fields required to make a request.
--
-- * 'passwordPolicy' - A structure that contains details about the account's password policy.
-- * 'responseStatus' - The response status code.
mkGetAccountPasswordPolicyResponse ::
  -- | 'passwordPolicy'
  PasswordPolicy ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountPasswordPolicyResponse
mkGetAccountPasswordPolicyResponse
  pPasswordPolicy_
  pResponseStatus_ =
    GetAccountPasswordPolicyResponse'
      { passwordPolicy =
          pPasswordPolicy_,
        responseStatus = pResponseStatus_
      }

-- | A structure that contains details about the account's password policy.
--
-- /Note:/ Consider using 'passwordPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapprsPasswordPolicy :: Lens.Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprsPasswordPolicy = Lens.lens (passwordPolicy :: GetAccountPasswordPolicyResponse -> PasswordPolicy) (\s a -> s {passwordPolicy = a} :: GetAccountPasswordPolicyResponse)
{-# DEPRECATED gapprsPasswordPolicy "Use generic-lens or generic-optics with 'passwordPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapprsResponseStatus :: Lens.Lens' GetAccountPasswordPolicyResponse Lude.Int
gapprsResponseStatus = Lens.lens (responseStatus :: GetAccountPasswordPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountPasswordPolicyResponse)
{-# DEPRECATED gapprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
