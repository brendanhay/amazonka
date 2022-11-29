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
-- Module      : Amazonka.Organizations.RegisterDelegatedAdministrator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified member account to administer the Organizations
-- features of the specified Amazon Web Services service. It grants
-- read-only access to Organizations service data. The account still
-- requires IAM permissions to access and administer the Amazon Web
-- Services service.
--
-- You can run this action only for Amazon Web Services services that
-- support this feature. For a current list of services that support it,
-- see the column /Supports Delegated Administrator/ in the table at
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services_list.html Amazon Web Services Services that you can use with Organizations>
-- in the /Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.RegisterDelegatedAdministrator
  ( -- * Creating a Request
    RegisterDelegatedAdministrator (..),
    newRegisterDelegatedAdministrator,

    -- * Request Lenses
    registerDelegatedAdministrator_accountId,
    registerDelegatedAdministrator_servicePrincipal,

    -- * Destructuring the Response
    RegisterDelegatedAdministratorResponse (..),
    newRegisterDelegatedAdministratorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { -- | The account ID number of the member account in the organization to
    -- register as a delegated administrator.
    accountId :: Prelude.Text,
    -- | The service principal of the Amazon Web Services service for which you
    -- want to make the member account a delegated administrator.
    servicePrincipal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'registerDelegatedAdministrator_accountId' - The account ID number of the member account in the organization to
-- register as a delegated administrator.
--
-- 'servicePrincipal', 'registerDelegatedAdministrator_servicePrincipal' - The service principal of the Amazon Web Services service for which you
-- want to make the member account a delegated administrator.
newRegisterDelegatedAdministrator ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'servicePrincipal'
  Prelude.Text ->
  RegisterDelegatedAdministrator
newRegisterDelegatedAdministrator
  pAccountId_
  pServicePrincipal_ =
    RegisterDelegatedAdministrator'
      { accountId =
          pAccountId_,
        servicePrincipal = pServicePrincipal_
      }

-- | The account ID number of the member account in the organization to
-- register as a delegated administrator.
registerDelegatedAdministrator_accountId :: Lens.Lens' RegisterDelegatedAdministrator Prelude.Text
registerDelegatedAdministrator_accountId = Lens.lens (\RegisterDelegatedAdministrator' {accountId} -> accountId) (\s@RegisterDelegatedAdministrator' {} a -> s {accountId = a} :: RegisterDelegatedAdministrator)

-- | The service principal of the Amazon Web Services service for which you
-- want to make the member account a delegated administrator.
registerDelegatedAdministrator_servicePrincipal :: Lens.Lens' RegisterDelegatedAdministrator Prelude.Text
registerDelegatedAdministrator_servicePrincipal = Lens.lens (\RegisterDelegatedAdministrator' {servicePrincipal} -> servicePrincipal) (\s@RegisterDelegatedAdministrator' {} a -> s {servicePrincipal = a} :: RegisterDelegatedAdministrator)

instance
  Core.AWSRequest
    RegisterDelegatedAdministrator
  where
  type
    AWSResponse RegisterDelegatedAdministrator =
      RegisterDelegatedAdministratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RegisterDelegatedAdministratorResponse'

instance
  Prelude.Hashable
    RegisterDelegatedAdministrator
  where
  hashWithSalt
    _salt
    RegisterDelegatedAdministrator' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` servicePrincipal

instance
  Prelude.NFData
    RegisterDelegatedAdministrator
  where
  rnf RegisterDelegatedAdministrator' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf servicePrincipal

instance
  Core.ToHeaders
    RegisterDelegatedAdministrator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.RegisterDelegatedAdministrator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterDelegatedAdministrator where
  toJSON RegisterDelegatedAdministrator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just
              ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.ToPath RegisterDelegatedAdministrator where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterDelegatedAdministrator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDelegatedAdministratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterDelegatedAdministratorResponse ::
  RegisterDelegatedAdministratorResponse
newRegisterDelegatedAdministratorResponse =
  RegisterDelegatedAdministratorResponse'

instance
  Prelude.NFData
    RegisterDelegatedAdministratorResponse
  where
  rnf _ = ()
