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
-- Module      : Network.AWS.Organizations.RegisterDelegatedAdministrator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified member account to administer the Organizations
-- features of the specified AWS service. It grants read-only access to AWS
-- Organizations service data. The account still requires IAM permissions
-- to access and administer the AWS service.
--
-- You can run this action only for AWS services that support this feature.
-- For a current list of services that support it, see the column /Supports
-- Delegated Administrator/ in the table at
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services_list.html AWS Services that you can use with AWS Organizations>
-- in the /AWS Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.RegisterDelegatedAdministrator
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { -- | The account ID number of the member account in the organization to
    -- register as a delegated administrator.
    accountId :: Prelude.Text,
    -- | The service principal of the AWS service for which you want to make the
    -- member account a delegated administrator.
    servicePrincipal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'servicePrincipal', 'registerDelegatedAdministrator_servicePrincipal' - The service principal of the AWS service for which you want to make the
-- member account a delegated administrator.
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

-- | The service principal of the AWS service for which you want to make the
-- member account a delegated administrator.
registerDelegatedAdministrator_servicePrincipal :: Lens.Lens' RegisterDelegatedAdministrator Prelude.Text
registerDelegatedAdministrator_servicePrincipal = Lens.lens (\RegisterDelegatedAdministrator' {servicePrincipal} -> servicePrincipal) (\s@RegisterDelegatedAdministrator' {} a -> s {servicePrincipal = a} :: RegisterDelegatedAdministrator)

instance
  Prelude.AWSRequest
    RegisterDelegatedAdministrator
  where
  type
    Rs RegisterDelegatedAdministrator =
      RegisterDelegatedAdministratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterDelegatedAdministratorResponse'

instance
  Prelude.Hashable
    RegisterDelegatedAdministrator

instance
  Prelude.NFData
    RegisterDelegatedAdministrator

instance
  Prelude.ToHeaders
    RegisterDelegatedAdministrator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.RegisterDelegatedAdministrator" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    RegisterDelegatedAdministrator
  where
  toJSON RegisterDelegatedAdministrator' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just
              ("ServicePrincipal" Prelude..= servicePrincipal)
          ]
      )

instance
  Prelude.ToPath
    RegisterDelegatedAdministrator
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RegisterDelegatedAdministrator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
