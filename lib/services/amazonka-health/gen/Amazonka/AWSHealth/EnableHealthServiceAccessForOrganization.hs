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
-- Module      : Amazonka.AWSHealth.EnableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Health to work with Organizations. You can use the
-- organizational view feature to aggregate events from all Amazon Web
-- Services accounts in your organization in a centralized location.
--
-- This operation also creates a service-linked role for the management
-- account in the organization.
--
-- To call this operation, you must meet the following requirements:
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan from
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>
--     to use the Health API. If you call the Health API from an Amazon Web
--     Services account that doesn\'t have a Business, Enterprise On-Ramp,
--     or Enterprise Support plan, you receive a
--     @SubscriptionRequiredException@ error.
--
-- -   You must have permission to call this operation from the
--     organization\'s management account. For example IAM policies, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html Health identity-based policy examples>.
--
-- If you don\'t have the required support plan, you can instead use the
-- Health console to enable the organizational view feature. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating Health events>
-- in the /Health User Guide/.
module Amazonka.AWSHealth.EnableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    EnableHealthServiceAccessForOrganization (..),
    newEnableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    EnableHealthServiceAccessForOrganizationResponse (..),
    newEnableHealthServiceAccessForOrganizationResponse,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableHealthServiceAccessForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableHealthServiceAccessForOrganization ::
  EnableHealthServiceAccessForOrganization
newEnableHealthServiceAccessForOrganization =
  EnableHealthServiceAccessForOrganization'

instance
  Core.AWSRequest
    EnableHealthServiceAccessForOrganization
  where
  type
    AWSResponse
      EnableHealthServiceAccessForOrganization =
      EnableHealthServiceAccessForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      EnableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.Hashable
    EnableHealthServiceAccessForOrganization
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    EnableHealthServiceAccessForOrganization
  where
  rnf _ = ()

instance
  Core.ToHeaders
    EnableHealthServiceAccessForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.EnableHealthServiceAccessForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    EnableHealthServiceAccessForOrganization
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance
  Core.ToPath
    EnableHealthServiceAccessForOrganization
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    EnableHealthServiceAccessForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableHealthServiceAccessForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableHealthServiceAccessForOrganizationResponse ::
  EnableHealthServiceAccessForOrganizationResponse
newEnableHealthServiceAccessForOrganizationResponse =
  EnableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.NFData
    EnableHealthServiceAccessForOrganizationResponse
  where
  rnf _ = ()
