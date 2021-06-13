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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results for the AWS Trusted Advisor check summaries for the
-- check IDs that you specified. You can get the check IDs by calling the
-- DescribeTrustedAdvisorChecks operation.
--
-- The response contains an array of TrustedAdvisorCheckSummary objects.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
  ( -- * Creating a Request
    DescribeTrustedAdvisorCheckSummaries (..),
    newDescribeTrustedAdvisorCheckSummaries,

    -- * Request Lenses
    describeTrustedAdvisorCheckSummaries_checkIds,

    -- * Destructuring the Response
    DescribeTrustedAdvisorCheckSummariesResponse (..),
    newDescribeTrustedAdvisorCheckSummariesResponse,

    -- * Response Lenses
    describeTrustedAdvisorCheckSummariesResponse_httpStatus,
    describeTrustedAdvisorCheckSummariesResponse_summaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorCheckSummaries' smart constructor.
data DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries'
  { -- | The IDs of the Trusted Advisor checks.
    checkIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkIds', 'describeTrustedAdvisorCheckSummaries_checkIds' - The IDs of the Trusted Advisor checks.
newDescribeTrustedAdvisorCheckSummaries ::
  DescribeTrustedAdvisorCheckSummaries
newDescribeTrustedAdvisorCheckSummaries =
  DescribeTrustedAdvisorCheckSummaries'
    { checkIds =
        Prelude.mempty
    }

-- | The IDs of the Trusted Advisor checks.
describeTrustedAdvisorCheckSummaries_checkIds :: Lens.Lens' DescribeTrustedAdvisorCheckSummaries [Prelude.Text]
describeTrustedAdvisorCheckSummaries_checkIds = Lens.lens (\DescribeTrustedAdvisorCheckSummaries' {checkIds} -> checkIds) (\s@DescribeTrustedAdvisorCheckSummaries' {} a -> s {checkIds = a} :: DescribeTrustedAdvisorCheckSummaries) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    DescribeTrustedAdvisorCheckSummaries
  where
  type
    AWSResponse DescribeTrustedAdvisorCheckSummaries =
      DescribeTrustedAdvisorCheckSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckSummariesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeTrustedAdvisorCheckSummaries

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckSummaries

instance
  Core.ToHeaders
    DescribeTrustedAdvisorCheckSummaries
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckSummaries" ::
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
    DescribeTrustedAdvisorCheckSummaries
  where
  toJSON DescribeTrustedAdvisorCheckSummaries' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("checkIds" Core..= checkIds)]
      )

instance
  Core.ToPath
    DescribeTrustedAdvisorCheckSummaries
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTrustedAdvisorCheckSummaries
  where
  toQuery = Prelude.const Prelude.mempty

-- | The summaries of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckSummaries operation.
--
-- /See:/ 'newDescribeTrustedAdvisorCheckSummariesResponse' smart constructor.
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary information for the requested Trusted Advisor checks.
    summaries :: [TrustedAdvisorCheckSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeTrustedAdvisorCheckSummariesResponse_httpStatus' - The response's http status code.
--
-- 'summaries', 'describeTrustedAdvisorCheckSummariesResponse_summaries' - The summary information for the requested Trusted Advisor checks.
newDescribeTrustedAdvisorCheckSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrustedAdvisorCheckSummariesResponse
newDescribeTrustedAdvisorCheckSummariesResponse
  pHttpStatus_ =
    DescribeTrustedAdvisorCheckSummariesResponse'
      { httpStatus =
          pHttpStatus_,
        summaries = Prelude.mempty
      }

-- | The response's http status code.
describeTrustedAdvisorCheckSummariesResponse_httpStatus :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse Prelude.Int
describeTrustedAdvisorCheckSummariesResponse_httpStatus = Lens.lens (\DescribeTrustedAdvisorCheckSummariesResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustedAdvisorCheckSummariesResponse' {} a -> s {httpStatus = a} :: DescribeTrustedAdvisorCheckSummariesResponse)

-- | The summary information for the requested Trusted Advisor checks.
describeTrustedAdvisorCheckSummariesResponse_summaries :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
describeTrustedAdvisorCheckSummariesResponse_summaries = Lens.lens (\DescribeTrustedAdvisorCheckSummariesResponse' {summaries} -> summaries) (\s@DescribeTrustedAdvisorCheckSummariesResponse' {} a -> s {summaries = a} :: DescribeTrustedAdvisorCheckSummariesResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeTrustedAdvisorCheckSummariesResponse
