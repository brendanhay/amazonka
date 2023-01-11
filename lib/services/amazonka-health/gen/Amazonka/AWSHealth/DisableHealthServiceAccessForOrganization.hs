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
-- Module      : Amazonka.AWSHealth.DisableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables Health from working with Organizations. To call this operation,
-- you must sign in as an Identity and Access Management (IAM) user, assume
-- an IAM role, or sign in as the root user (not recommended) in the
-- organization\'s management account. For more information, see
-- <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating Health events>
-- in the /Health User Guide/.
--
-- This operation doesn\'t remove the service-linked role from the
-- management account in your organization. You must use the IAM console,
-- API, or Command Line Interface (CLI) to remove the service-linked role.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html#delete-service-linked-role Deleting a Service-Linked Role>
-- in the /IAM User Guide/.
--
-- You can also disable the organizational feature by using the
-- Organizations
-- <https://docs.aws.amazon.com/organizations/latest/APIReference/API_DisableAWSServiceAccess.html DisableAWSServiceAccess>
-- API operation. After you call this operation, Health stops aggregating
-- events for all other Amazon Web Services accounts in your organization.
-- If you call the Health API operations for organizational view, Health
-- returns an error. Health continues to aggregate health events for your
-- Amazon Web Services account.
module Amazonka.AWSHealth.DisableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    DisableHealthServiceAccessForOrganization (..),
    newDisableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    DisableHealthServiceAccessForOrganizationResponse (..),
    newDisableHealthServiceAccessForOrganizationResponse,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableHealthServiceAccessForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableHealthServiceAccessForOrganization ::
  DisableHealthServiceAccessForOrganization
newDisableHealthServiceAccessForOrganization =
  DisableHealthServiceAccessForOrganization'

instance
  Core.AWSRequest
    DisableHealthServiceAccessForOrganization
  where
  type
    AWSResponse
      DisableHealthServiceAccessForOrganization =
      DisableHealthServiceAccessForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.Hashable
    DisableHealthServiceAccessForOrganization
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DisableHealthServiceAccessForOrganization
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DisableHealthServiceAccessForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DisableHealthServiceAccessForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisableHealthServiceAccessForOrganization
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    DisableHealthServiceAccessForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisableHealthServiceAccessForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableHealthServiceAccessForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableHealthServiceAccessForOrganizationResponse ::
  DisableHealthServiceAccessForOrganizationResponse
newDisableHealthServiceAccessForOrganizationResponse =
  DisableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.NFData
    DisableHealthServiceAccessForOrganizationResponse
  where
  rnf _ = ()
