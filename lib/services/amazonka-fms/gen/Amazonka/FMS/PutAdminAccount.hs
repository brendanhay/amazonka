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
-- Module      : Amazonka.FMS.PutAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Firewall Manager administrator account. The
-- account must be a member of the organization that was onboarded to
-- Firewall Manager by AssociateAdminAccount. Only the organization\'s
-- management account can create an Firewall Manager administrator account.
-- When you create an Firewall Manager administrator account, the service
-- checks to see if the account is already a delegated administrator within
-- Organizations. If the account isn\'t a delegated administrator, Firewall
-- Manager calls Organizations to delegate the account within
-- Organizations. For more information about administrator accounts within
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
module Amazonka.FMS.PutAdminAccount
  ( -- * Creating a Request
    PutAdminAccount (..),
    newPutAdminAccount,

    -- * Request Lenses
    putAdminAccount_adminScope,
    putAdminAccount_adminAccount,

    -- * Destructuring the Response
    PutAdminAccountResponse (..),
    newPutAdminAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAdminAccount' smart constructor.
data PutAdminAccount = PutAdminAccount'
  { -- | Configures the resources that the specified Firewall Manager
    -- administrator can manage. As a best practice, set the administrative
    -- scope according to the principles of least privilege. Only grant the
    -- administrator the specific resources or permissions that they need to
    -- perform the duties of their role.
    adminScope :: Prelude.Maybe AdminScope,
    -- | The Amazon Web Services account ID to add as an Firewall Manager
    -- administrator account. The account must be a member of the organization
    -- that was onboarded to Firewall Manager by AssociateAdminAccount. For
    -- more information about Organizations, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
    adminAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminScope', 'putAdminAccount_adminScope' - Configures the resources that the specified Firewall Manager
-- administrator can manage. As a best practice, set the administrative
-- scope according to the principles of least privilege. Only grant the
-- administrator the specific resources or permissions that they need to
-- perform the duties of their role.
--
-- 'adminAccount', 'putAdminAccount_adminAccount' - The Amazon Web Services account ID to add as an Firewall Manager
-- administrator account. The account must be a member of the organization
-- that was onboarded to Firewall Manager by AssociateAdminAccount. For
-- more information about Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
newPutAdminAccount ::
  -- | 'adminAccount'
  Prelude.Text ->
  PutAdminAccount
newPutAdminAccount pAdminAccount_ =
  PutAdminAccount'
    { adminScope = Prelude.Nothing,
      adminAccount = pAdminAccount_
    }

-- | Configures the resources that the specified Firewall Manager
-- administrator can manage. As a best practice, set the administrative
-- scope according to the principles of least privilege. Only grant the
-- administrator the specific resources or permissions that they need to
-- perform the duties of their role.
putAdminAccount_adminScope :: Lens.Lens' PutAdminAccount (Prelude.Maybe AdminScope)
putAdminAccount_adminScope = Lens.lens (\PutAdminAccount' {adminScope} -> adminScope) (\s@PutAdminAccount' {} a -> s {adminScope = a} :: PutAdminAccount)

-- | The Amazon Web Services account ID to add as an Firewall Manager
-- administrator account. The account must be a member of the organization
-- that was onboarded to Firewall Manager by AssociateAdminAccount. For
-- more information about Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
putAdminAccount_adminAccount :: Lens.Lens' PutAdminAccount Prelude.Text
putAdminAccount_adminAccount = Lens.lens (\PutAdminAccount' {adminAccount} -> adminAccount) (\s@PutAdminAccount' {} a -> s {adminAccount = a} :: PutAdminAccount)

instance Core.AWSRequest PutAdminAccount where
  type
    AWSResponse PutAdminAccount =
      PutAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutAdminAccountResponse'

instance Prelude.Hashable PutAdminAccount where
  hashWithSalt _salt PutAdminAccount' {..} =
    _salt
      `Prelude.hashWithSalt` adminScope
      `Prelude.hashWithSalt` adminAccount

instance Prelude.NFData PutAdminAccount where
  rnf PutAdminAccount' {..} =
    Prelude.rnf adminScope
      `Prelude.seq` Prelude.rnf adminAccount

instance Data.ToHeaders PutAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.PutAdminAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAdminAccount where
  toJSON PutAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdminScope" Data..=) Prelude.<$> adminScope,
            Prelude.Just ("AdminAccount" Data..= adminAccount)
          ]
      )

instance Data.ToPath PutAdminAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAdminAccountResponse' smart constructor.
data PutAdminAccountResponse = PutAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutAdminAccountResponse ::
  PutAdminAccountResponse
newPutAdminAccountResponse = PutAdminAccountResponse'

instance Prelude.NFData PutAdminAccountResponse where
  rnf _ = ()
