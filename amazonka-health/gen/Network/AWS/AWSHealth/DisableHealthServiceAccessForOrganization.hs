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
-- Module      : Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables AWS Health from working with AWS Organizations. To call this
-- operation, you must sign in as an AWS Identity and Access Management
-- (IAM) user, assume an IAM role, or sign in as the root user (not
-- recommended) in the organization\'s management account. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events>
-- in the /AWS Health User Guide/.
--
-- This operation doesn\'t remove the service-linked role from the
-- management account in your organization. You must use the IAM console,
-- API, or AWS Command Line Interface (AWS CLI) to remove the
-- service-linked role. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html#delete-service-linked-role Deleting a Service-Linked Role>
-- in the /IAM User Guide/.
--
-- You can also disable the organizational feature by using the
-- Organizations
-- <https://docs.aws.amazon.com/organizations/latest/APIReference/API_DisableAWSServiceAccess.html DisableAWSServiceAccess>
-- API operation. After you call this operation, AWS Health stops
-- aggregating events for all other AWS accounts in your organization. If
-- you call the AWS Health API operations for organizational view, AWS
-- Health returns an error. AWS Health continues to aggregate health events
-- for your AWS account.
module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    DisableHealthServiceAccessForOrganization (..),
    newDisableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    DisableHealthServiceAccessForOrganizationResponse (..),
    newDisableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableHealthServiceAccessForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableHealthServiceAccessForOrganization ::
  DisableHealthServiceAccessForOrganization
newDisableHealthServiceAccessForOrganization =
  DisableHealthServiceAccessForOrganization'

instance
  Prelude.AWSRequest
    DisableHealthServiceAccessForOrganization
  where
  type
    Rs DisableHealthServiceAccessForOrganization =
      DisableHealthServiceAccessForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisableHealthServiceAccessForOrganizationResponse'

instance
  Prelude.Hashable
    DisableHealthServiceAccessForOrganization

instance
  Prelude.NFData
    DisableHealthServiceAccessForOrganization

instance
  Prelude.ToHeaders
    DisableHealthServiceAccessForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSHealth_20160804.DisableHealthServiceAccessForOrganization" ::
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
    DisableHealthServiceAccessForOrganization
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    DisableHealthServiceAccessForOrganization
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisableHealthServiceAccessForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
