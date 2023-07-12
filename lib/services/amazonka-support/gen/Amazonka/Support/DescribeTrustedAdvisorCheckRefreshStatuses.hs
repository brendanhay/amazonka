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
-- Module      : Amazonka.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the refresh status of the Trusted Advisor checks that have the
-- specified check IDs. You can get the check IDs by calling the
-- DescribeTrustedAdvisorChecks operation.
--
-- Some checks are refreshed automatically, and you can\'t return their
-- refresh statuses by using the
-- @DescribeTrustedAdvisorCheckRefreshStatuses@ operation. If you call this
-- operation for these checks, you might see an @InvalidParameterValue@
-- error.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     doesn\'t have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
--
-- To call the Trusted Advisor operations in the Amazon Web Services
-- Support API, you must use the US East (N. Virginia) endpoint. Currently,
-- the US West (Oregon) and Europe (Ireland) endpoints don\'t support the
-- Trusted Advisor operations. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/about-support-api.html#endpoint About the Amazon Web Services Support API>
-- in the /Amazon Web Services Support User Guide/.
module Amazonka.Support.DescribeTrustedAdvisorCheckRefreshStatuses
  ( -- * Creating a Request
    DescribeTrustedAdvisorCheckRefreshStatuses (..),
    newDescribeTrustedAdvisorCheckRefreshStatuses,

    -- * Request Lenses
    describeTrustedAdvisorCheckRefreshStatuses_checkIds,

    -- * Destructuring the Response
    DescribeTrustedAdvisorCheckRefreshStatusesResponse (..),
    newDescribeTrustedAdvisorCheckRefreshStatusesResponse,

    -- * Response Lenses
    describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus,
    describeTrustedAdvisorCheckRefreshStatusesResponse_statuses,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorCheckRefreshStatuses' smart constructor.
data DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses'
  { -- | The IDs of the Trusted Advisor checks to get the status.
    --
    -- If you specify the check ID of a check that is automatically refreshed,
    -- you might see an @InvalidParameterValue@ error.
    checkIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckRefreshStatuses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkIds', 'describeTrustedAdvisorCheckRefreshStatuses_checkIds' - The IDs of the Trusted Advisor checks to get the status.
--
-- If you specify the check ID of a check that is automatically refreshed,
-- you might see an @InvalidParameterValue@ error.
newDescribeTrustedAdvisorCheckRefreshStatuses ::
  DescribeTrustedAdvisorCheckRefreshStatuses
newDescribeTrustedAdvisorCheckRefreshStatuses =
  DescribeTrustedAdvisorCheckRefreshStatuses'
    { checkIds =
        Prelude.mempty
    }

-- | The IDs of the Trusted Advisor checks to get the status.
--
-- If you specify the check ID of a check that is automatically refreshed,
-- you might see an @InvalidParameterValue@ error.
describeTrustedAdvisorCheckRefreshStatuses_checkIds :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Prelude.Text]
describeTrustedAdvisorCheckRefreshStatuses_checkIds = Lens.lens (\DescribeTrustedAdvisorCheckRefreshStatuses' {checkIds} -> checkIds) (\s@DescribeTrustedAdvisorCheckRefreshStatuses' {} a -> s {checkIds = a} :: DescribeTrustedAdvisorCheckRefreshStatuses) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  type
    AWSResponse
      DescribeTrustedAdvisorCheckRefreshStatuses =
      DescribeTrustedAdvisorCheckRefreshStatusesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckRefreshStatusesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "statuses" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  hashWithSalt
    _salt
    DescribeTrustedAdvisorCheckRefreshStatuses' {..} =
      _salt `Prelude.hashWithSalt` checkIds

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  rnf DescribeTrustedAdvisorCheckRefreshStatuses' {..} =
    Prelude.rnf checkIds

instance
  Data.ToHeaders
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckRefreshStatuses" ::
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
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toJSON
    DescribeTrustedAdvisorCheckRefreshStatuses' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("checkIds" Data..= checkIds)]
        )

instance
  Data.ToPath
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toQuery = Prelude.const Prelude.mempty

-- | The statuses of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckRefreshStatuses operation.
--
-- /See:/ 'newDescribeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The refresh status of the specified Trusted Advisor checks.
    statuses :: [TrustedAdvisorCheckRefreshStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus' - The response's http status code.
--
-- 'statuses', 'describeTrustedAdvisorCheckRefreshStatusesResponse_statuses' - The refresh status of the specified Trusted Advisor checks.
newDescribeTrustedAdvisorCheckRefreshStatusesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrustedAdvisorCheckRefreshStatusesResponse
newDescribeTrustedAdvisorCheckRefreshStatusesResponse
  pHttpStatus_ =
    DescribeTrustedAdvisorCheckRefreshStatusesResponse'
      { httpStatus =
          pHttpStatus_,
        statuses =
          Prelude.mempty
      }

-- | The response's http status code.
describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse Prelude.Int
describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus = Lens.lens (\DescribeTrustedAdvisorCheckRefreshStatusesResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustedAdvisorCheckRefreshStatusesResponse' {} a -> s {httpStatus = a} :: DescribeTrustedAdvisorCheckRefreshStatusesResponse)

-- | The refresh status of the specified Trusted Advisor checks.
describeTrustedAdvisorCheckRefreshStatusesResponse_statuses :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
describeTrustedAdvisorCheckRefreshStatusesResponse_statuses = Lens.lens (\DescribeTrustedAdvisorCheckRefreshStatusesResponse' {statuses} -> statuses) (\s@DescribeTrustedAdvisorCheckRefreshStatusesResponse' {} a -> s {statuses = a} :: DescribeTrustedAdvisorCheckRefreshStatusesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckRefreshStatusesResponse
  where
  rnf
    DescribeTrustedAdvisorCheckRefreshStatusesResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf statuses
