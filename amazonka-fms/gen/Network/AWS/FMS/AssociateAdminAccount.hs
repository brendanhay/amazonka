{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.AssociateAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Firewall Manager administrator account. AWS Firewall
-- Manager must be associated with the master account of your AWS
-- organization or associated with a member account that has the
-- appropriate permissions. If the account ID that you submit is not an AWS
-- Organizations master account, AWS Firewall Manager will set the
-- appropriate permissions for the given member account.
--
-- The account that you associate with AWS Firewall Manager is called the
-- AWS Firewall Manager administrator account.
module Network.AWS.FMS.AssociateAdminAccount
  ( -- * Creating a Request
    AssociateAdminAccount (..),
    newAssociateAdminAccount,

    -- * Request Lenses
    associateAdminAccount_adminAccount,

    -- * Destructuring the Response
    AssociateAdminAccountResponse (..),
    newAssociateAdminAccountResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateAdminAccount' smart constructor.
data AssociateAdminAccount = AssociateAdminAccount'
  { -- | The AWS account ID to associate with AWS Firewall Manager as the AWS
    -- Firewall Manager administrator account. This can be an AWS Organizations
    -- master account or a member account. For more information about AWS
    -- Organizations and master accounts, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization>.
    adminAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'associateAdminAccount_adminAccount' - The AWS account ID to associate with AWS Firewall Manager as the AWS
-- Firewall Manager administrator account. This can be an AWS Organizations
-- master account or a member account. For more information about AWS
-- Organizations and master accounts, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization>.
newAssociateAdminAccount ::
  -- | 'adminAccount'
  Prelude.Text ->
  AssociateAdminAccount
newAssociateAdminAccount pAdminAccount_ =
  AssociateAdminAccount'
    { adminAccount =
        pAdminAccount_
    }

-- | The AWS account ID to associate with AWS Firewall Manager as the AWS
-- Firewall Manager administrator account. This can be an AWS Organizations
-- master account or a member account. For more information about AWS
-- Organizations and master accounts, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization>.
associateAdminAccount_adminAccount :: Lens.Lens' AssociateAdminAccount Prelude.Text
associateAdminAccount_adminAccount = Lens.lens (\AssociateAdminAccount' {adminAccount} -> adminAccount) (\s@AssociateAdminAccount' {} a -> s {adminAccount = a} :: AssociateAdminAccount)

instance Prelude.AWSRequest AssociateAdminAccount where
  type
    Rs AssociateAdminAccount =
      AssociateAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AssociateAdminAccountResponse'

instance Prelude.Hashable AssociateAdminAccount

instance Prelude.NFData AssociateAdminAccount

instance Prelude.ToHeaders AssociateAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.AssociateAdminAccount" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateAdminAccount where
  toJSON AssociateAdminAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AdminAccount" Prelude..= adminAccount)
          ]
      )

instance Prelude.ToPath AssociateAdminAccount where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAdminAccountResponse' smart constructor.
data AssociateAdminAccountResponse = AssociateAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateAdminAccountResponse ::
  AssociateAdminAccountResponse
newAssociateAdminAccountResponse =
  AssociateAdminAccountResponse'

instance Prelude.NFData AssociateAdminAccountResponse
