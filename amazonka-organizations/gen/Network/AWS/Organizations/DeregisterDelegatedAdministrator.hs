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
-- Module      : Network.AWS.Organizations.DeregisterDelegatedAdministrator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member AWS account as a delegated administrator
-- for the specified AWS service.
--
-- Deregistering a delegated administrator can have unintended impacts on
-- the functionality of the enabled AWS service. See the documentation for
-- the enabled service before you deregister a delegated administrator so
-- that you understand any potential impacts.
--
-- You can run this action only for AWS services that support this feature.
-- For a current list of services that support it, see the column /Supports
-- Delegated Administrator/ in the table at
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services_list.html AWS Services that you can use with AWS Organizations>
-- in the /AWS Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.DeregisterDelegatedAdministrator
  ( -- * Creating a Request
    DeregisterDelegatedAdministrator (..),
    newDeregisterDelegatedAdministrator,

    -- * Request Lenses
    deregisterDelegatedAdministrator_accountId,
    deregisterDelegatedAdministrator_servicePrincipal,

    -- * Destructuring the Response
    DeregisterDelegatedAdministratorResponse (..),
    newDeregisterDelegatedAdministratorResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterDelegatedAdministrator' smart constructor.
data DeregisterDelegatedAdministrator = DeregisterDelegatedAdministrator'
  { -- | The account ID number of the member account in the organization that you
    -- want to deregister as a delegated administrator.
    accountId :: Core.Text,
    -- | The service principal name of an AWS service for which the account is a
    -- delegated administrator.
    --
    -- Delegated administrator privileges are revoked for only the specified
    -- AWS service from the member account. If the specified service is the
    -- only service for which the member account is a delegated administrator,
    -- the operation also revokes Organizations read action permissions.
    servicePrincipal :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deregisterDelegatedAdministrator_accountId' - The account ID number of the member account in the organization that you
-- want to deregister as a delegated administrator.
--
-- 'servicePrincipal', 'deregisterDelegatedAdministrator_servicePrincipal' - The service principal name of an AWS service for which the account is a
-- delegated administrator.
--
-- Delegated administrator privileges are revoked for only the specified
-- AWS service from the member account. If the specified service is the
-- only service for which the member account is a delegated administrator,
-- the operation also revokes Organizations read action permissions.
newDeregisterDelegatedAdministrator ::
  -- | 'accountId'
  Core.Text ->
  -- | 'servicePrincipal'
  Core.Text ->
  DeregisterDelegatedAdministrator
newDeregisterDelegatedAdministrator
  pAccountId_
  pServicePrincipal_ =
    DeregisterDelegatedAdministrator'
      { accountId =
          pAccountId_,
        servicePrincipal = pServicePrincipal_
      }

-- | The account ID number of the member account in the organization that you
-- want to deregister as a delegated administrator.
deregisterDelegatedAdministrator_accountId :: Lens.Lens' DeregisterDelegatedAdministrator Core.Text
deregisterDelegatedAdministrator_accountId = Lens.lens (\DeregisterDelegatedAdministrator' {accountId} -> accountId) (\s@DeregisterDelegatedAdministrator' {} a -> s {accountId = a} :: DeregisterDelegatedAdministrator)

-- | The service principal name of an AWS service for which the account is a
-- delegated administrator.
--
-- Delegated administrator privileges are revoked for only the specified
-- AWS service from the member account. If the specified service is the
-- only service for which the member account is a delegated administrator,
-- the operation also revokes Organizations read action permissions.
deregisterDelegatedAdministrator_servicePrincipal :: Lens.Lens' DeregisterDelegatedAdministrator Core.Text
deregisterDelegatedAdministrator_servicePrincipal = Lens.lens (\DeregisterDelegatedAdministrator' {servicePrincipal} -> servicePrincipal) (\s@DeregisterDelegatedAdministrator' {} a -> s {servicePrincipal = a} :: DeregisterDelegatedAdministrator)

instance
  Core.AWSRequest
    DeregisterDelegatedAdministrator
  where
  type
    AWSResponse DeregisterDelegatedAdministrator =
      DeregisterDelegatedAdministratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeregisterDelegatedAdministratorResponse'

instance
  Core.Hashable
    DeregisterDelegatedAdministrator

instance Core.NFData DeregisterDelegatedAdministrator

instance
  Core.ToHeaders
    DeregisterDelegatedAdministrator
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DeregisterDelegatedAdministrator" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterDelegatedAdministrator where
  toJSON DeregisterDelegatedAdministrator' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just
              ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.ToPath DeregisterDelegatedAdministrator where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeregisterDelegatedAdministrator
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterDelegatedAdministratorResponse' smart constructor.
data DeregisterDelegatedAdministratorResponse = DeregisterDelegatedAdministratorResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDelegatedAdministratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterDelegatedAdministratorResponse ::
  DeregisterDelegatedAdministratorResponse
newDeregisterDelegatedAdministratorResponse =
  DeregisterDelegatedAdministratorResponse'

instance
  Core.NFData
    DeregisterDelegatedAdministratorResponse
