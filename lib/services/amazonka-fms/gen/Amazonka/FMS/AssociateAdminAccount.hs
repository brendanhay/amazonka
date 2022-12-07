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
-- Module      : Amazonka.FMS.AssociateAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Firewall Manager administrator account. The account must be a
-- member of the organization in Organizations whose resources you want to
-- protect. Firewall Manager sets the permissions that allow the account to
-- administer your Firewall Manager policies.
--
-- The account that you associate with Firewall Manager is called the
-- Firewall Manager administrator account.
module Amazonka.FMS.AssociateAdminAccount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAdminAccount' smart constructor.
data AssociateAdminAccount = AssociateAdminAccount'
  { -- | The Amazon Web Services account ID to associate with Firewall Manager as
    -- the Firewall Manager administrator account. This must be an
    -- Organizations member account. For more information about Organizations,
    -- see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
    adminAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'associateAdminAccount_adminAccount' - The Amazon Web Services account ID to associate with Firewall Manager as
-- the Firewall Manager administrator account. This must be an
-- Organizations member account. For more information about Organizations,
-- see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
newAssociateAdminAccount ::
  -- | 'adminAccount'
  Prelude.Text ->
  AssociateAdminAccount
newAssociateAdminAccount pAdminAccount_ =
  AssociateAdminAccount'
    { adminAccount =
        pAdminAccount_
    }

-- | The Amazon Web Services account ID to associate with Firewall Manager as
-- the Firewall Manager administrator account. This must be an
-- Organizations member account. For more information about Organizations,
-- see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the Amazon Web Services Accounts in Your Organization>.
associateAdminAccount_adminAccount :: Lens.Lens' AssociateAdminAccount Prelude.Text
associateAdminAccount_adminAccount = Lens.lens (\AssociateAdminAccount' {adminAccount} -> adminAccount) (\s@AssociateAdminAccount' {} a -> s {adminAccount = a} :: AssociateAdminAccount)

instance Core.AWSRequest AssociateAdminAccount where
  type
    AWSResponse AssociateAdminAccount =
      AssociateAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull AssociateAdminAccountResponse'

instance Prelude.Hashable AssociateAdminAccount where
  hashWithSalt _salt AssociateAdminAccount' {..} =
    _salt `Prelude.hashWithSalt` adminAccount

instance Prelude.NFData AssociateAdminAccount where
  rnf AssociateAdminAccount' {..} =
    Prelude.rnf adminAccount

instance Data.ToHeaders AssociateAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.AssociateAdminAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateAdminAccount where
  toJSON AssociateAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AdminAccount" Data..= adminAccount)]
      )

instance Data.ToPath AssociateAdminAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAdminAccountResponse' smart constructor.
data AssociateAdminAccountResponse = AssociateAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateAdminAccountResponse ::
  AssociateAdminAccountResponse
newAssociateAdminAccountResponse =
  AssociateAdminAccountResponse'

instance Prelude.NFData AssociateAdminAccountResponse where
  rnf _ = ()
