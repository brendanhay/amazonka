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
-- Module      : Amazonka.Organizations.EnableAWSServiceAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of an Amazon Web Services service (the service
-- that is specified by @ServicePrincipal@) with Organizations. When you
-- enable integration, you allow the specified service to create a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role>
-- in all the accounts in your organization. This allows the service to
-- perform operations on your behalf in your organization and its accounts.
--
-- We recommend that you enable integration between Organizations and the
-- specified Amazon Web Services service by using the console or commands
-- that are provided by the specified service. Doing so ensures that the
-- service is aware that it can create the resources that are required for
-- the integration. How the service creates those resources in the
-- organization\'s accounts depends on that service. For more information,
-- see the documentation for the other Amazon Web Services service.
--
-- For more information about enabling services to integrate with
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating Organizations with Other Amazon Web Services Services>
-- in the /Organizations User Guide./
--
-- You can only call this operation from the organization\'s management
-- account and only if the organization has
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html enabled all features>.
module Amazonka.Organizations.EnableAWSServiceAccess
  ( -- * Creating a Request
    EnableAWSServiceAccess (..),
    newEnableAWSServiceAccess,

    -- * Request Lenses
    enableAWSServiceAccess_servicePrincipal,

    -- * Destructuring the Response
    EnableAWSServiceAccessResponse (..),
    newEnableAWSServiceAccessResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAWSServiceAccess' smart constructor.
data EnableAWSServiceAccess = EnableAWSServiceAccess'
  { -- | The service principal name of the Amazon Web Services service for which
    -- you want to enable integration with your organization. This is typically
    -- in the form of a URL, such as
    -- @ @/@service-abbreviation@/@.amazonaws.com@.
    servicePrincipal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAWSServiceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'enableAWSServiceAccess_servicePrincipal' - The service principal name of the Amazon Web Services service for which
-- you want to enable integration with your organization. This is typically
-- in the form of a URL, such as
-- @ @/@service-abbreviation@/@.amazonaws.com@.
newEnableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Prelude.Text ->
  EnableAWSServiceAccess
newEnableAWSServiceAccess pServicePrincipal_ =
  EnableAWSServiceAccess'
    { servicePrincipal =
        pServicePrincipal_
    }

-- | The service principal name of the Amazon Web Services service for which
-- you want to enable integration with your organization. This is typically
-- in the form of a URL, such as
-- @ @/@service-abbreviation@/@.amazonaws.com@.
enableAWSServiceAccess_servicePrincipal :: Lens.Lens' EnableAWSServiceAccess Prelude.Text
enableAWSServiceAccess_servicePrincipal = Lens.lens (\EnableAWSServiceAccess' {servicePrincipal} -> servicePrincipal) (\s@EnableAWSServiceAccess' {} a -> s {servicePrincipal = a} :: EnableAWSServiceAccess)

instance Core.AWSRequest EnableAWSServiceAccess where
  type
    AWSResponse EnableAWSServiceAccess =
      EnableAWSServiceAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      EnableAWSServiceAccessResponse'

instance Prelude.Hashable EnableAWSServiceAccess where
  hashWithSalt _salt EnableAWSServiceAccess' {..} =
    _salt `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData EnableAWSServiceAccess where
  rnf EnableAWSServiceAccess' {..} =
    Prelude.rnf servicePrincipal

instance Data.ToHeaders EnableAWSServiceAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.EnableAWSServiceAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableAWSServiceAccess where
  toJSON EnableAWSServiceAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ServicePrincipal" Data..= servicePrincipal)
          ]
      )

instance Data.ToPath EnableAWSServiceAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableAWSServiceAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableAWSServiceAccessResponse' smart constructor.
data EnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAWSServiceAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAWSServiceAccessResponse ::
  EnableAWSServiceAccessResponse
newEnableAWSServiceAccessResponse =
  EnableAWSServiceAccessResponse'

instance
  Prelude.NFData
    EnableAWSServiceAccessResponse
  where
  rnf _ = ()
