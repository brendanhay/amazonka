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
-- Module      : Amazonka.Organizations.DisableAWSServiceAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the integration of an Amazon Web Services service (the service
-- that is specified by @ServicePrincipal@) with Organizations. When you
-- disable integration, the specified service no longer can create a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role>
-- in /new/ accounts in your organization. This means the service can\'t
-- perform operations on your behalf on any new accounts in your
-- organization. The service can still perform operations in older accounts
-- until the service completes its clean-up from Organizations.
--
-- We __/strongly recommend/__ that you don\'t use this command to disable
-- integration between Organizations and the specified Amazon Web Services
-- service. Instead, use the console or commands that are provided by the
-- specified service. This lets the trusted service perform any required
-- initialization when enabling trusted access, such as creating any
-- required resources and any required clean up of resources when disabling
-- trusted access.
--
-- For information about how to disable trusted service access to your
-- organization using the trusted service, see the __Learn more__ link
-- under the __Supports Trusted Access__ column at
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services_list.html Amazon Web Services services that you can use with Organizations>.
-- on this page.
--
-- If you disable access by using this command, it causes the following
-- actions to occur:
--
-- -   The service can no longer create a service-linked role in the
--     accounts in your organization. This means that the service can\'t
--     perform operations on your behalf on any new accounts in your
--     organization. The service can still perform operations in older
--     accounts until the service completes its clean-up from
--     Organizations.
--
-- -   The service can no longer perform tasks in the member accounts in
--     the organization, unless those operations are explicitly permitted
--     by the IAM policies that are attached to your roles. This includes
--     any data aggregation from the member accounts to the management
--     account, or to a delegated administrator account, where relevant.
--
-- -   Some services detect this and clean up any remaining data or
--     resources related to the integration, while other services stop
--     accessing the organization but leave any historical data and
--     configuration in place to support a possible re-enabling of the
--     integration.
--
-- Using the other service\'s console or commands to disable the
-- integration ensures that the other service is aware that it can clean up
-- any resources that are required only for the integration. How the
-- service cleans up its resources in the organization\'s accounts depends
-- on that service. For more information, see the documentation for the
-- other Amazon Web Services service.
--
-- After you perform the @DisableAWSServiceAccess@ operation, the specified
-- service can no longer perform operations in your organization\'s
-- accounts
--
-- For more information about integrating other services with
-- Organizations, including the list of services that work with
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating Organizations with Other Amazon Web Services Services>
-- in the /Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.DisableAWSServiceAccess
  ( -- * Creating a Request
    DisableAWSServiceAccess (..),
    newDisableAWSServiceAccess,

    -- * Request Lenses
    disableAWSServiceAccess_servicePrincipal,

    -- * Destructuring the Response
    DisableAWSServiceAccessResponse (..),
    newDisableAWSServiceAccessResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableAWSServiceAccess' smart constructor.
data DisableAWSServiceAccess = DisableAWSServiceAccess'
  { -- | The service principal name of the Amazon Web Services service for which
    -- you want to disable integration with your organization. This is
    -- typically in the form of a URL, such as
    -- @ service-abbreviation.amazonaws.com@.
    servicePrincipal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAWSServiceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'disableAWSServiceAccess_servicePrincipal' - The service principal name of the Amazon Web Services service for which
-- you want to disable integration with your organization. This is
-- typically in the form of a URL, such as
-- @ service-abbreviation.amazonaws.com@.
newDisableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Prelude.Text ->
  DisableAWSServiceAccess
newDisableAWSServiceAccess pServicePrincipal_ =
  DisableAWSServiceAccess'
    { servicePrincipal =
        pServicePrincipal_
    }

-- | The service principal name of the Amazon Web Services service for which
-- you want to disable integration with your organization. This is
-- typically in the form of a URL, such as
-- @ service-abbreviation.amazonaws.com@.
disableAWSServiceAccess_servicePrincipal :: Lens.Lens' DisableAWSServiceAccess Prelude.Text
disableAWSServiceAccess_servicePrincipal = Lens.lens (\DisableAWSServiceAccess' {servicePrincipal} -> servicePrincipal) (\s@DisableAWSServiceAccess' {} a -> s {servicePrincipal = a} :: DisableAWSServiceAccess)

instance Core.AWSRequest DisableAWSServiceAccess where
  type
    AWSResponse DisableAWSServiceAccess =
      DisableAWSServiceAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisableAWSServiceAccessResponse'

instance Prelude.Hashable DisableAWSServiceAccess where
  hashWithSalt _salt DisableAWSServiceAccess' {..} =
    _salt `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData DisableAWSServiceAccess where
  rnf DisableAWSServiceAccess' {..} =
    Prelude.rnf servicePrincipal

instance Core.ToHeaders DisableAWSServiceAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DisableAWSServiceAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableAWSServiceAccess where
  toJSON DisableAWSServiceAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.ToPath DisableAWSServiceAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableAWSServiceAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableAWSServiceAccessResponse' smart constructor.
data DisableAWSServiceAccessResponse = DisableAWSServiceAccessResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAWSServiceAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableAWSServiceAccessResponse ::
  DisableAWSServiceAccessResponse
newDisableAWSServiceAccessResponse =
  DisableAWSServiceAccessResponse'

instance
  Prelude.NFData
    DisableAWSServiceAccessResponse
  where
  rnf _ = ()
