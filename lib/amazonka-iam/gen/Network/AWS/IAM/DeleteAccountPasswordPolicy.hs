{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account. There are no parameters.
module Network.AWS.IAM.DeleteAccountPasswordPolicy
  ( -- * Creating a request
    DeleteAccountPasswordPolicy (..),
    mkDeleteAccountPasswordPolicy,

    -- * Destructuring the response
    DeleteAccountPasswordPolicyResponse (..),
    mkDeleteAccountPasswordPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountPasswordPolicy' with the minimum fields required to make a request.
mkDeleteAccountPasswordPolicy ::
  DeleteAccountPasswordPolicy
mkDeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'

instance Lude.AWSRequest DeleteAccountPasswordPolicy where
  type
    Rs DeleteAccountPasswordPolicy =
      DeleteAccountPasswordPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteAccountPasswordPolicyResponse'

instance Lude.ToHeaders DeleteAccountPasswordPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAccountPasswordPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAccountPasswordPolicy where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DeleteAccountPasswordPolicy" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountPasswordPolicyResponse' with the minimum fields required to make a request.
mkDeleteAccountPasswordPolicyResponse ::
  DeleteAccountPasswordPolicyResponse
mkDeleteAccountPasswordPolicyResponse =
  DeleteAccountPasswordPolicyResponse'
