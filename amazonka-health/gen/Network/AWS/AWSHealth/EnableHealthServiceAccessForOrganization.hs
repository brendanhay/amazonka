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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      EnableHealthServiceAccessForOrganizationResponse'

instance
  Core.Hashable
    EnableHealthServiceAccessForOrganization

instance
  Core.NFData
    EnableHealthServiceAccessForOrganization

instance
  Core.ToHeaders
    EnableHealthServiceAccessForOrganization
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.EnableHealthServiceAccessForOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    EnableHealthServiceAccessForOrganization
  where
  toJSON = Core.const (Core.Object Core.mempty)

instance
  Core.ToPath
    EnableHealthServiceAccessForOrganization
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    EnableHealthServiceAccessForOrganization
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableHealthServiceAccessForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableHealthServiceAccessForOrganizationResponse ::
  EnableHealthServiceAccessForOrganizationResponse
newEnableHealthServiceAccessForOrganizationResponse =
  EnableHealthServiceAccessForOrganizationResponse'

instance
  Core.NFData
    EnableHealthServiceAccessForOrganizationResponse
