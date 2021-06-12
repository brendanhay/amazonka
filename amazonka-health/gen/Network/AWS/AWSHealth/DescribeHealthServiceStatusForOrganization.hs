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
-- Module      : Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation provides status information on enabling or disabling AWS
-- Health to work with your organization. To call this operation, you must
-- sign in as an IAM user, assume an IAM role, or sign in as the root user
-- (not recommended) in the organization\'s management account.
module Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
  ( -- * Creating a Request
    DescribeHealthServiceStatusForOrganization (..),
    newDescribeHealthServiceStatusForOrganization,

    -- * Destructuring the Response
    DescribeHealthServiceStatusForOrganizationResponse (..),
    newDescribeHealthServiceStatusForOrganizationResponse,

    -- * Response Lenses
    describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization,
    describeHealthServiceStatusForOrganizationResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHealthServiceStatusForOrganization' smart constructor.
data DescribeHealthServiceStatusForOrganization = DescribeHealthServiceStatusForOrganization'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHealthServiceStatusForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeHealthServiceStatusForOrganization ::
  DescribeHealthServiceStatusForOrganization
newDescribeHealthServiceStatusForOrganization =
  DescribeHealthServiceStatusForOrganization'

instance
  Core.AWSRequest
    DescribeHealthServiceStatusForOrganization
  where
  type
    AWSResponse
      DescribeHealthServiceStatusForOrganization =
      DescribeHealthServiceStatusForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHealthServiceStatusForOrganizationResponse'
            Core.<$> ( x
                         Core..?> "healthServiceAccessStatusForOrganization"
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeHealthServiceStatusForOrganization

instance
  Core.NFData
    DescribeHealthServiceStatusForOrganization

instance
  Core.ToHeaders
    DescribeHealthServiceStatusForOrganization
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeHealthServiceStatusForOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeHealthServiceStatusForOrganization
  where
  toJSON = Core.const (Core.Object Core.mempty)

instance
  Core.ToPath
    DescribeHealthServiceStatusForOrganization
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeHealthServiceStatusForOrganization
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeHealthServiceStatusForOrganizationResponse' smart constructor.
data DescribeHealthServiceStatusForOrganizationResponse = DescribeHealthServiceStatusForOrganizationResponse'
  { -- | Information about the status of enabling or disabling AWS Health
    -- Organizational View in your organization.
    --
    -- Valid values are @ENABLED | DISABLED | PENDING@.
    healthServiceAccessStatusForOrganization :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHealthServiceStatusForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthServiceAccessStatusForOrganization', 'describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization' - Information about the status of enabling or disabling AWS Health
-- Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@.
--
-- 'httpStatus', 'describeHealthServiceStatusForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeHealthServiceStatusForOrganizationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHealthServiceStatusForOrganizationResponse
newDescribeHealthServiceStatusForOrganizationResponse
  pHttpStatus_ =
    DescribeHealthServiceStatusForOrganizationResponse'
      { healthServiceAccessStatusForOrganization =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the status of enabling or disabling AWS Health
-- Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@.
describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse (Core.Maybe Core.Text)
describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization = Lens.lens (\DescribeHealthServiceStatusForOrganizationResponse' {healthServiceAccessStatusForOrganization} -> healthServiceAccessStatusForOrganization) (\s@DescribeHealthServiceStatusForOrganizationResponse' {} a -> s {healthServiceAccessStatusForOrganization = a} :: DescribeHealthServiceStatusForOrganizationResponse)

-- | The response's http status code.
describeHealthServiceStatusForOrganizationResponse_httpStatus :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse Core.Int
describeHealthServiceStatusForOrganizationResponse_httpStatus = Lens.lens (\DescribeHealthServiceStatusForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeHealthServiceStatusForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeHealthServiceStatusForOrganizationResponse)

instance
  Core.NFData
    DescribeHealthServiceStatusForOrganizationResponse
