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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHealthServiceStatusForOrganization' smart constructor.
data DescribeHealthServiceStatusForOrganization = DescribeHealthServiceStatusForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHealthServiceStatusForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeHealthServiceStatusForOrganization ::
  DescribeHealthServiceStatusForOrganization
newDescribeHealthServiceStatusForOrganization =
  DescribeHealthServiceStatusForOrganization'

instance
  Prelude.AWSRequest
    DescribeHealthServiceStatusForOrganization
  where
  type
    Rs DescribeHealthServiceStatusForOrganization =
      DescribeHealthServiceStatusForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHealthServiceStatusForOrganizationResponse'
            Prelude.<$> ( x
                            Prelude..?> "healthServiceAccessStatusForOrganization"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeHealthServiceStatusForOrganization

instance
  Prelude.NFData
    DescribeHealthServiceStatusForOrganization

instance
  Prelude.ToHeaders
    DescribeHealthServiceStatusForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSHealth_20160804.DescribeHealthServiceStatusForOrganization" ::
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
    DescribeHealthServiceStatusForOrganization
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    DescribeHealthServiceStatusForOrganization
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeHealthServiceStatusForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHealthServiceStatusForOrganizationResponse' smart constructor.
data DescribeHealthServiceStatusForOrganizationResponse = DescribeHealthServiceStatusForOrganizationResponse'
  { -- | Information about the status of enabling or disabling AWS Health
    -- Organizational View in your organization.
    --
    -- Valid values are @ENABLED | DISABLED | PENDING@.
    healthServiceAccessStatusForOrganization :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeHealthServiceStatusForOrganizationResponse
newDescribeHealthServiceStatusForOrganizationResponse
  pHttpStatus_ =
    DescribeHealthServiceStatusForOrganizationResponse'
      { healthServiceAccessStatusForOrganization =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the status of enabling or disabling AWS Health
-- Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@.
describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse (Prelude.Maybe Prelude.Text)
describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization = Lens.lens (\DescribeHealthServiceStatusForOrganizationResponse' {healthServiceAccessStatusForOrganization} -> healthServiceAccessStatusForOrganization) (\s@DescribeHealthServiceStatusForOrganizationResponse' {} a -> s {healthServiceAccessStatusForOrganization = a} :: DescribeHealthServiceStatusForOrganizationResponse)

-- | The response's http status code.
describeHealthServiceStatusForOrganizationResponse_httpStatus :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse Prelude.Int
describeHealthServiceStatusForOrganizationResponse_httpStatus = Lens.lens (\DescribeHealthServiceStatusForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeHealthServiceStatusForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeHealthServiceStatusForOrganizationResponse)

instance
  Prelude.NFData
    DescribeHealthServiceStatusForOrganizationResponse
