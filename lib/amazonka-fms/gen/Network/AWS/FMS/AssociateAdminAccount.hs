{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.AssociateAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Firewall Manager administrator account. AWS Firewall Manager must be associated with the master account of your AWS organization or associated with a member account that has the appropriate permissions. If the account ID that you submit is not an AWS Organizations master account, AWS Firewall Manager will set the appropriate permissions for the given member account.
--
-- The account that you associate with AWS Firewall Manager is called the AWS Firewall Manager administrator account.
module Network.AWS.FMS.AssociateAdminAccount
  ( -- * Creating a request
    AssociateAdminAccount (..),
    mkAssociateAdminAccount,

    -- ** Request lenses
    aaaAdminAccount,

    -- * Destructuring the response
    AssociateAdminAccountResponse (..),
    mkAssociateAdminAccountResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateAdminAccount' smart constructor.
newtype AssociateAdminAccount = AssociateAdminAccount'
  { -- | The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> .
    adminAccount :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateAdminAccount' with the minimum fields required to make a request.
--
-- * 'adminAccount' - The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> .
mkAssociateAdminAccount ::
  -- | 'adminAccount'
  Lude.Text ->
  AssociateAdminAccount
mkAssociateAdminAccount pAdminAccount_ =
  AssociateAdminAccount' {adminAccount = pAdminAccount_}

-- | The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> .
--
-- /Note:/ Consider using 'adminAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaAdminAccount :: Lens.Lens' AssociateAdminAccount Lude.Text
aaaAdminAccount = Lens.lens (adminAccount :: AssociateAdminAccount -> Lude.Text) (\s a -> s {adminAccount = a} :: AssociateAdminAccount)
{-# DEPRECATED aaaAdminAccount "Use generic-lens or generic-optics with 'adminAccount' instead." #-}

instance Lude.AWSRequest AssociateAdminAccount where
  type Rs AssociateAdminAccount = AssociateAdminAccountResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull AssociateAdminAccountResponse'

instance Lude.ToHeaders AssociateAdminAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.AssociateAdminAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateAdminAccount where
  toJSON AssociateAdminAccount' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AdminAccount" Lude..= adminAccount)])

instance Lude.ToPath AssociateAdminAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateAdminAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateAdminAccountResponse' smart constructor.
data AssociateAdminAccountResponse = AssociateAdminAccountResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateAdminAccountResponse' with the minimum fields required to make a request.
mkAssociateAdminAccountResponse ::
  AssociateAdminAccountResponse
mkAssociateAdminAccountResponse = AssociateAdminAccountResponse'
