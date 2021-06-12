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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results of the AWS Trusted Advisor check that has the
-- specified check ID. You can get the check IDs by calling the
-- DescribeTrustedAdvisorChecks operation.
--
-- The response contains a TrustedAdvisorCheckResult object, which contains
-- these three objects:
--
-- -   TrustedAdvisorCategorySpecificSummary
--
-- -   TrustedAdvisorResourceDetail
--
-- -   TrustedAdvisorResourcesSummary
--
-- In addition, the response contains these fields:
--
-- -   __status__ - The alert status of the check: \"ok\" (green),
--     \"warning\" (yellow), \"error\" (red), or \"not_available\".
--
-- -   __timestamp__ - The time of the last refresh of the check.
--
-- -   __checkId__ - The unique identifier for the check.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
  ( -- * Creating a Request
    DescribeTrustedAdvisorCheckResult (..),
    newDescribeTrustedAdvisorCheckResult,

    -- * Request Lenses
    describeTrustedAdvisorCheckResult_language,
    describeTrustedAdvisorCheckResult_checkId,

    -- * Destructuring the Response
    DescribeTrustedAdvisorCheckResultResponse (..),
    newDescribeTrustedAdvisorCheckResultResponse,

    -- * Response Lenses
    describeTrustedAdvisorCheckResultResponse_result,
    describeTrustedAdvisorCheckResultResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- |
--
-- /See:/ 'newDescribeTrustedAdvisorCheckResult' smart constructor.
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Core.Maybe Core.Text,
    -- | The unique identifier for the Trusted Advisor check.
    checkId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'describeTrustedAdvisorCheckResult_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
--
-- 'checkId', 'describeTrustedAdvisorCheckResult_checkId' - The unique identifier for the Trusted Advisor check.
newDescribeTrustedAdvisorCheckResult ::
  -- | 'checkId'
  Core.Text ->
  DescribeTrustedAdvisorCheckResult
newDescribeTrustedAdvisorCheckResult pCheckId_ =
  DescribeTrustedAdvisorCheckResult'
    { language =
        Core.Nothing,
      checkId = pCheckId_
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
describeTrustedAdvisorCheckResult_language :: Lens.Lens' DescribeTrustedAdvisorCheckResult (Core.Maybe Core.Text)
describeTrustedAdvisorCheckResult_language = Lens.lens (\DescribeTrustedAdvisorCheckResult' {language} -> language) (\s@DescribeTrustedAdvisorCheckResult' {} a -> s {language = a} :: DescribeTrustedAdvisorCheckResult)

-- | The unique identifier for the Trusted Advisor check.
describeTrustedAdvisorCheckResult_checkId :: Lens.Lens' DescribeTrustedAdvisorCheckResult Core.Text
describeTrustedAdvisorCheckResult_checkId = Lens.lens (\DescribeTrustedAdvisorCheckResult' {checkId} -> checkId) (\s@DescribeTrustedAdvisorCheckResult' {} a -> s {checkId = a} :: DescribeTrustedAdvisorCheckResult)

instance
  Core.AWSRequest
    DescribeTrustedAdvisorCheckResult
  where
  type
    AWSResponse DescribeTrustedAdvisorCheckResult =
      DescribeTrustedAdvisorCheckResultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckResultResponse'
            Core.<$> (x Core..?> "result")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeTrustedAdvisorCheckResult

instance
  Core.NFData
    DescribeTrustedAdvisorCheckResult

instance
  Core.ToHeaders
    DescribeTrustedAdvisorCheckResult
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckResult" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeTrustedAdvisorCheckResult
  where
  toJSON DescribeTrustedAdvisorCheckResult' {..} =
    Core.object
      ( Core.catMaybes
          [ ("language" Core..=) Core.<$> language,
            Core.Just ("checkId" Core..= checkId)
          ]
      )

instance
  Core.ToPath
    DescribeTrustedAdvisorCheckResult
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeTrustedAdvisorCheckResult
  where
  toQuery = Core.const Core.mempty

-- | The result of the Trusted Advisor check returned by the
-- DescribeTrustedAdvisorCheckResult operation.
--
-- /See:/ 'newDescribeTrustedAdvisorCheckResultResponse' smart constructor.
data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'
  { -- | The detailed results of the Trusted Advisor check.
    result :: Core.Maybe TrustedAdvisorCheckResult,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorCheckResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'describeTrustedAdvisorCheckResultResponse_result' - The detailed results of the Trusted Advisor check.
--
-- 'httpStatus', 'describeTrustedAdvisorCheckResultResponse_httpStatus' - The response's http status code.
newDescribeTrustedAdvisorCheckResultResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrustedAdvisorCheckResultResponse
newDescribeTrustedAdvisorCheckResultResponse
  pHttpStatus_ =
    DescribeTrustedAdvisorCheckResultResponse'
      { result =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The detailed results of the Trusted Advisor check.
describeTrustedAdvisorCheckResultResponse_result :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse (Core.Maybe TrustedAdvisorCheckResult)
describeTrustedAdvisorCheckResultResponse_result = Lens.lens (\DescribeTrustedAdvisorCheckResultResponse' {result} -> result) (\s@DescribeTrustedAdvisorCheckResultResponse' {} a -> s {result = a} :: DescribeTrustedAdvisorCheckResultResponse)

-- | The response's http status code.
describeTrustedAdvisorCheckResultResponse_httpStatus :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse Core.Int
describeTrustedAdvisorCheckResultResponse_httpStatus = Lens.lens (\DescribeTrustedAdvisorCheckResultResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustedAdvisorCheckResultResponse' {} a -> s {httpStatus = a} :: DescribeTrustedAdvisorCheckResultResponse)

instance
  Core.NFData
    DescribeTrustedAdvisorCheckResultResponse
