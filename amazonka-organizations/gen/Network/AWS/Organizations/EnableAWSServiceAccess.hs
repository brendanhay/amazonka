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
-- Module      : Network.AWS.Organizations.EnableAWSServiceAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of an AWS service (the service that is specified
-- by @ServicePrincipal@) with AWS Organizations. When you enable
-- integration, you allow the specified service to create a
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role>
-- in all the accounts in your organization. This allows the service to
-- perform operations on your behalf in your organization and its accounts.
--
-- We recommend that you enable integration between AWS Organizations and
-- the specified AWS service by using the console or commands that are
-- provided by the specified service. Doing so ensures that the service is
-- aware that it can create the resources that are required for the
-- integration. How the service creates those resources in the
-- organization\'s accounts depends on that service. For more information,
-- see the documentation for the other AWS service.
--
-- For more information about enabling services to integrate with AWS
-- Organizations, see
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services>
-- in the /AWS Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account and only if the organization has
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html enabled all features>.
module Network.AWS.Organizations.EnableAWSServiceAccess
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableAWSServiceAccess' smart constructor.
data EnableAWSServiceAccess = EnableAWSServiceAccess'
  { -- | The service principal name of the AWS service for which you want to
    -- enable integration with your organization. This is typically in the form
    -- of a URL, such as @ service-abbreviation.amazonaws.com@.
    servicePrincipal :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAWSServiceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'enableAWSServiceAccess_servicePrincipal' - The service principal name of the AWS service for which you want to
-- enable integration with your organization. This is typically in the form
-- of a URL, such as @ service-abbreviation.amazonaws.com@.
newEnableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Core.Text ->
  EnableAWSServiceAccess
newEnableAWSServiceAccess pServicePrincipal_ =
  EnableAWSServiceAccess'
    { servicePrincipal =
        pServicePrincipal_
    }

-- | The service principal name of the AWS service for which you want to
-- enable integration with your organization. This is typically in the form
-- of a URL, such as @ service-abbreviation.amazonaws.com@.
enableAWSServiceAccess_servicePrincipal :: Lens.Lens' EnableAWSServiceAccess Core.Text
enableAWSServiceAccess_servicePrincipal = Lens.lens (\EnableAWSServiceAccess' {servicePrincipal} -> servicePrincipal) (\s@EnableAWSServiceAccess' {} a -> s {servicePrincipal = a} :: EnableAWSServiceAccess)

instance Core.AWSRequest EnableAWSServiceAccess where
  type
    AWSResponse EnableAWSServiceAccess =
      EnableAWSServiceAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      EnableAWSServiceAccessResponse'

instance Core.Hashable EnableAWSServiceAccess

instance Core.NFData EnableAWSServiceAccess

instance Core.ToHeaders EnableAWSServiceAccess where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.EnableAWSServiceAccess" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableAWSServiceAccess where
  toJSON EnableAWSServiceAccess' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.ToPath EnableAWSServiceAccess where
  toPath = Core.const "/"

instance Core.ToQuery EnableAWSServiceAccess where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableAWSServiceAccessResponse' smart constructor.
data EnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAWSServiceAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAWSServiceAccessResponse ::
  EnableAWSServiceAccessResponse
newEnableAWSServiceAccessResponse =
  EnableAWSServiceAccessResponse'

instance Core.NFData EnableAWSServiceAccessResponse
