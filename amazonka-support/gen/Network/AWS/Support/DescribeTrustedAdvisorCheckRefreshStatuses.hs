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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the refresh status of the AWS Trusted Advisor checks that have
-- the specified check IDs. You can get the check IDs by calling the
-- DescribeTrustedAdvisorChecks operation.
--
-- Some checks are refreshed automatically, and you can\'t return their
-- refresh statuses by using the
-- @DescribeTrustedAdvisorCheckRefreshStatuses@ operation. If you call this
-- operation for these checks, you might see an @InvalidParameterValue@
-- error.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorCheckRefreshStatuses' smart constructor.
data DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses'
  { -- | The IDs of the Trusted Advisor checks to get the status of.
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
-- 'checkIds', 'describeTrustedAdvisorCheckRefreshStatuses_checkIds' - The IDs of the Trusted Advisor checks to get the status of.
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

-- | The IDs of the Trusted Advisor checks to get the status of.
--
-- If you specify the check ID of a check that is automatically refreshed,
-- you might see an @InvalidParameterValue@ error.
describeTrustedAdvisorCheckRefreshStatuses_checkIds :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Prelude.Text]
describeTrustedAdvisorCheckRefreshStatuses_checkIds = Lens.lens (\DescribeTrustedAdvisorCheckRefreshStatuses' {checkIds} -> checkIds) (\s@DescribeTrustedAdvisorCheckRefreshStatuses' {} a -> s {checkIds = a} :: DescribeTrustedAdvisorCheckRefreshStatuses) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  type
    AWSResponse
      DescribeTrustedAdvisorCheckRefreshStatuses =
      DescribeTrustedAdvisorCheckRefreshStatusesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckRefreshStatusesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..?> "statuses" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeTrustedAdvisorCheckRefreshStatuses

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckRefreshStatuses

instance
  Core.ToHeaders
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckRefreshStatuses" ::
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
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toJSON
    DescribeTrustedAdvisorCheckRefreshStatuses' {..} =
      Core.object
        ( Prelude.catMaybes
            [Prelude.Just ("checkIds" Core..= checkIds)]
        )

instance
  Core.ToPath
    DescribeTrustedAdvisorCheckRefreshStatuses
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
describeTrustedAdvisorCheckRefreshStatusesResponse_statuses = Lens.lens (\DescribeTrustedAdvisorCheckRefreshStatusesResponse' {statuses} -> statuses) (\s@DescribeTrustedAdvisorCheckRefreshStatusesResponse' {} a -> s {statuses = a} :: DescribeTrustedAdvisorCheckRefreshStatusesResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckRefreshStatusesResponse
