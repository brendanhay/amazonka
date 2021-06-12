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
-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the AWS Trusted Advisor check that you specify using the check
-- ID. You can get the check IDs by calling the
-- DescribeTrustedAdvisorChecks operation.
--
-- Some checks are refreshed automatically. If you call the
-- @RefreshTrustedAdvisorCheck@ operation to refresh them, you might see
-- the @InvalidParameterValue@ error.
--
-- The response contains a TrustedAdvisorCheckRefreshStatus object.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.RefreshTrustedAdvisorCheck
  ( -- * Creating a Request
    RefreshTrustedAdvisorCheck (..),
    newRefreshTrustedAdvisorCheck,

    -- * Request Lenses
    refreshTrustedAdvisorCheck_checkId,

    -- * Destructuring the Response
    RefreshTrustedAdvisorCheckResponse (..),
    newRefreshTrustedAdvisorCheckResponse,

    -- * Response Lenses
    refreshTrustedAdvisorCheckResponse_httpStatus,
    refreshTrustedAdvisorCheckResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- |
--
-- /See:/ 'newRefreshTrustedAdvisorCheck' smart constructor.
data RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck'
  { -- | The unique identifier for the Trusted Advisor check to refresh.
    -- __Note:__ Specifying the check ID of a check that is automatically
    -- refreshed causes an @InvalidParameterValue@ error.
    checkId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RefreshTrustedAdvisorCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkId', 'refreshTrustedAdvisorCheck_checkId' - The unique identifier for the Trusted Advisor check to refresh.
-- __Note:__ Specifying the check ID of a check that is automatically
-- refreshed causes an @InvalidParameterValue@ error.
newRefreshTrustedAdvisorCheck ::
  -- | 'checkId'
  Core.Text ->
  RefreshTrustedAdvisorCheck
newRefreshTrustedAdvisorCheck pCheckId_ =
  RefreshTrustedAdvisorCheck' {checkId = pCheckId_}

-- | The unique identifier for the Trusted Advisor check to refresh.
-- __Note:__ Specifying the check ID of a check that is automatically
-- refreshed causes an @InvalidParameterValue@ error.
refreshTrustedAdvisorCheck_checkId :: Lens.Lens' RefreshTrustedAdvisorCheck Core.Text
refreshTrustedAdvisorCheck_checkId = Lens.lens (\RefreshTrustedAdvisorCheck' {checkId} -> checkId) (\s@RefreshTrustedAdvisorCheck' {} a -> s {checkId = a} :: RefreshTrustedAdvisorCheck)

instance Core.AWSRequest RefreshTrustedAdvisorCheck where
  type
    AWSResponse RefreshTrustedAdvisorCheck =
      RefreshTrustedAdvisorCheckResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshTrustedAdvisorCheckResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "status")
      )

instance Core.Hashable RefreshTrustedAdvisorCheck

instance Core.NFData RefreshTrustedAdvisorCheck

instance Core.ToHeaders RefreshTrustedAdvisorCheck where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.RefreshTrustedAdvisorCheck" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RefreshTrustedAdvisorCheck where
  toJSON RefreshTrustedAdvisorCheck' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("checkId" Core..= checkId)]
      )

instance Core.ToPath RefreshTrustedAdvisorCheck where
  toPath = Core.const "/"

instance Core.ToQuery RefreshTrustedAdvisorCheck where
  toQuery = Core.const Core.mempty

-- | The current refresh status of a Trusted Advisor check.
--
-- /See:/ 'newRefreshTrustedAdvisorCheckResponse' smart constructor.
data RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The current refresh status for a check, including the amount of time
    -- until the check is eligible for refresh.
    status :: TrustedAdvisorCheckRefreshStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RefreshTrustedAdvisorCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'refreshTrustedAdvisorCheckResponse_httpStatus' - The response's http status code.
--
-- 'status', 'refreshTrustedAdvisorCheckResponse_status' - The current refresh status for a check, including the amount of time
-- until the check is eligible for refresh.
newRefreshTrustedAdvisorCheckResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'status'
  TrustedAdvisorCheckRefreshStatus ->
  RefreshTrustedAdvisorCheckResponse
newRefreshTrustedAdvisorCheckResponse
  pHttpStatus_
  pStatus_ =
    RefreshTrustedAdvisorCheckResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
refreshTrustedAdvisorCheckResponse_httpStatus :: Lens.Lens' RefreshTrustedAdvisorCheckResponse Core.Int
refreshTrustedAdvisorCheckResponse_httpStatus = Lens.lens (\RefreshTrustedAdvisorCheckResponse' {httpStatus} -> httpStatus) (\s@RefreshTrustedAdvisorCheckResponse' {} a -> s {httpStatus = a} :: RefreshTrustedAdvisorCheckResponse)

-- | The current refresh status for a check, including the amount of time
-- until the check is eligible for refresh.
refreshTrustedAdvisorCheckResponse_status :: Lens.Lens' RefreshTrustedAdvisorCheckResponse TrustedAdvisorCheckRefreshStatus
refreshTrustedAdvisorCheckResponse_status = Lens.lens (\RefreshTrustedAdvisorCheckResponse' {status} -> status) (\s@RefreshTrustedAdvisorCheckResponse' {} a -> s {status = a} :: RefreshTrustedAdvisorCheckResponse)

instance
  Core.NFData
    RefreshTrustedAdvisorCheckResponse
