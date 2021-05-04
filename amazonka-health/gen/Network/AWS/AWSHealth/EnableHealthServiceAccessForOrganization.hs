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
-- Module      : Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables AWS Health to work with AWS Organizations. You can use the
-- organizational view feature to aggregate events from all AWS accounts in
-- your organization in a centralized location.
--
-- This operation also creates a service-linked role for the management
-- account in the organization.
--
-- To call this operation, you must meet the following requirements:
--
-- -   You must have a Business or Enterprise support plan from
--     <http://aws.amazon.com/premiumsupport/ AWS Support> to use the AWS
--     Health API. If you call the AWS Health API from an AWS account that
--     doesn\'t have a Business or Enterprise support plan, you receive a
--     @SubscriptionRequiredException@ error.
--
-- -   You must have permission to call this operation from the
--     organization\'s management account. For example IAM policies, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html AWS Health identity-based policy examples>.
--
-- If you don\'t have the required support plan, you can instead use the
-- AWS Health console to enable the organizational view feature. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events>
-- in the /AWS Health User Guide/.
module Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    EnableHealthServiceAccessForOrganization (..),
    newEnableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    EnableHealthServiceAccessForOrganizationResponse (..),
    newEnableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableHealthServiceAccessForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableHealthServiceAccessForOrganization ::
  EnableHealthServiceAccessForOrganization
newEnableHealthServiceAccessForOrganization =
  EnableHealthServiceAccessForOrganization'

instance
  Prelude.AWSRequest
    EnableHealthServiceAccessForOrganization
  where
  type
    Rs EnableHealthServiceAccessForOrganization =
      EnableHealthServiceAccessForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      EnableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.Hashable
    EnableHealthServiceAccessForOrganization

instance
  Prelude.NFData
    EnableHealthServiceAccessForOrganization

instance
  Prelude.ToHeaders
    EnableHealthServiceAccessForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSHealth_20160804.EnableHealthServiceAccessForOrganization" ::
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
    EnableHealthServiceAccessForOrganization
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    EnableHealthServiceAccessForOrganization
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    EnableHealthServiceAccessForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
