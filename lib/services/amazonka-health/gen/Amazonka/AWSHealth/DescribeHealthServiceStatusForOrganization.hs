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
-- Module      : Amazonka.AWSHealth.DescribeHealthServiceStatusForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation provides status information on enabling or disabling
-- Health to work with your organization. To call this operation, you must
-- sign in as an IAM user, assume an IAM role, or sign in as the root user
-- (not recommended) in the organization\'s management account.
module Amazonka.AWSHealth.DescribeHealthServiceStatusForOrganization
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

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeHealthServiceStatusForOrganization' smart constructor.
data DescribeHealthServiceStatusForOrganization = DescribeHealthServiceStatusForOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHealthServiceStatusForOrganizationResponse'
            Prelude.<$> ( x
                            Data..?> "healthServiceAccessStatusForOrganization"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeHealthServiceStatusForOrganization
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeHealthServiceStatusForOrganization
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeHealthServiceStatusForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeHealthServiceStatusForOrganization" ::
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
    DescribeHealthServiceStatusForOrganization
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    DescribeHealthServiceStatusForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeHealthServiceStatusForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHealthServiceStatusForOrganizationResponse' smart constructor.
data DescribeHealthServiceStatusForOrganizationResponse = DescribeHealthServiceStatusForOrganizationResponse'
  { -- | Information about the status of enabling or disabling the Health
    -- organizational view feature in your organization.
    --
    -- Valid values are @ENABLED | DISABLED | PENDING@.
    healthServiceAccessStatusForOrganization :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHealthServiceStatusForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthServiceAccessStatusForOrganization', 'describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization' - Information about the status of enabling or disabling the Health
-- organizational view feature in your organization.
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

-- | Information about the status of enabling or disabling the Health
-- organizational view feature in your organization.
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
  where
  rnf
    DescribeHealthServiceStatusForOrganizationResponse' {..} =
      Prelude.rnf
        healthServiceAccessStatusForOrganization
        `Prelude.seq` Prelude.rnf httpStatus
