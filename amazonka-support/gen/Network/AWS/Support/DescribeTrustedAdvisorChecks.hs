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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all available AWS Trusted Advisor checks,
-- including the name, ID, category, description, and metadata. You must
-- specify a language code. The AWS Support API currently supports English
-- (\"en\") and Japanese (\"ja\"). The response contains a
-- TrustedAdvisorCheckDescription object for each check. You must set the
-- AWS Region to us-east-1.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeTrustedAdvisorChecks
  ( -- * Creating a Request
    DescribeTrustedAdvisorChecks (..),
    newDescribeTrustedAdvisorChecks,

    -- * Request Lenses
    describeTrustedAdvisorChecks_language,

    -- * Destructuring the Response
    DescribeTrustedAdvisorChecksResponse (..),
    newDescribeTrustedAdvisorChecksResponse,

    -- * Response Lenses
    describeTrustedAdvisorChecksResponse_httpStatus,
    describeTrustedAdvisorChecksResponse_checks,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorChecks' smart constructor.
data DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'describeTrustedAdvisorChecks_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
newDescribeTrustedAdvisorChecks ::
  -- | 'language'
  Core.Text ->
  DescribeTrustedAdvisorChecks
newDescribeTrustedAdvisorChecks pLanguage_ =
  DescribeTrustedAdvisorChecks'
    { language =
        pLanguage_
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
describeTrustedAdvisorChecks_language :: Lens.Lens' DescribeTrustedAdvisorChecks Core.Text
describeTrustedAdvisorChecks_language = Lens.lens (\DescribeTrustedAdvisorChecks' {language} -> language) (\s@DescribeTrustedAdvisorChecks' {} a -> s {language = a} :: DescribeTrustedAdvisorChecks)

instance Core.AWSRequest DescribeTrustedAdvisorChecks where
  type
    AWSResponse DescribeTrustedAdvisorChecks =
      DescribeTrustedAdvisorChecksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorChecksResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "checks" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeTrustedAdvisorChecks

instance Core.NFData DescribeTrustedAdvisorChecks

instance Core.ToHeaders DescribeTrustedAdvisorChecks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorChecks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTrustedAdvisorChecks where
  toJSON DescribeTrustedAdvisorChecks' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("language" Core..= language)]
      )

instance Core.ToPath DescribeTrustedAdvisorChecks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrustedAdvisorChecks where
  toQuery = Core.const Core.mempty

-- | Information about the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorChecks operation.
--
-- /See:/ 'newDescribeTrustedAdvisorChecksResponse' smart constructor.
data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about all available Trusted Advisor checks.
    checks :: [TrustedAdvisorCheckDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorChecksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeTrustedAdvisorChecksResponse_httpStatus' - The response's http status code.
--
-- 'checks', 'describeTrustedAdvisorChecksResponse_checks' - Information about all available Trusted Advisor checks.
newDescribeTrustedAdvisorChecksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrustedAdvisorChecksResponse
newDescribeTrustedAdvisorChecksResponse pHttpStatus_ =
  DescribeTrustedAdvisorChecksResponse'
    { httpStatus =
        pHttpStatus_,
      checks = Core.mempty
    }

-- | The response's http status code.
describeTrustedAdvisorChecksResponse_httpStatus :: Lens.Lens' DescribeTrustedAdvisorChecksResponse Core.Int
describeTrustedAdvisorChecksResponse_httpStatus = Lens.lens (\DescribeTrustedAdvisorChecksResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustedAdvisorChecksResponse' {} a -> s {httpStatus = a} :: DescribeTrustedAdvisorChecksResponse)

-- | Information about all available Trusted Advisor checks.
describeTrustedAdvisorChecksResponse_checks :: Lens.Lens' DescribeTrustedAdvisorChecksResponse [TrustedAdvisorCheckDescription]
describeTrustedAdvisorChecksResponse_checks = Lens.lens (\DescribeTrustedAdvisorChecksResponse' {checks} -> checks) (\s@DescribeTrustedAdvisorChecksResponse' {} a -> s {checks = a} :: DescribeTrustedAdvisorChecksResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeTrustedAdvisorChecksResponse
