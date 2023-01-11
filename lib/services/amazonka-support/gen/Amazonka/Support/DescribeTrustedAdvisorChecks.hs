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
-- Module      : Amazonka.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all available Trusted Advisor checks,
-- including the name, ID, category, description, and metadata. You must
-- specify a language code.
--
-- The response contains a TrustedAdvisorCheckDescription object for each
-- check. You must set the Amazon Web Services Region to us-east-1.
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
-- -   The names and descriptions for Trusted Advisor checks are subject to
--     change. We recommend that you specify the check ID in your code to
--     uniquely identify a check.
--
-- To call the Trusted Advisor operations in the Amazon Web Services
-- Support API, you must use the US East (N. Virginia) endpoint. Currently,
-- the US West (Oregon) and Europe (Ireland) endpoints don\'t support the
-- Trusted Advisor operations. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/about-support-api.html#endpoint About the Amazon Web Services Support API>
-- in the /Amazon Web Services Support User Guide/.
module Amazonka.Support.DescribeTrustedAdvisorChecks
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorChecks' smart constructor.
data DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
  { -- | The ISO 639-1 code for the language that you want your checks to appear
    -- in.
    --
    -- The Amazon Web Services Support API currently supports the following
    -- languages for Trusted Advisor:
    --
    -- -   Chinese, Simplified - @zh@
    --
    -- -   Chinese, Traditional - @zh_TW@
    --
    -- -   English - @en@
    --
    -- -   French - @fr@
    --
    -- -   German - @de@
    --
    -- -   Indonesian - @id@
    --
    -- -   Italian - @it@
    --
    -- -   Japanese - @ja@
    --
    -- -   Korean - @ko@
    --
    -- -   Portuguese, Brazilian - @pt_BR@
    --
    -- -   Spanish - @es@
    language :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustedAdvisorChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'describeTrustedAdvisorChecks_language' - The ISO 639-1 code for the language that you want your checks to appear
-- in.
--
-- The Amazon Web Services Support API currently supports the following
-- languages for Trusted Advisor:
--
-- -   Chinese, Simplified - @zh@
--
-- -   Chinese, Traditional - @zh_TW@
--
-- -   English - @en@
--
-- -   French - @fr@
--
-- -   German - @de@
--
-- -   Indonesian - @id@
--
-- -   Italian - @it@
--
-- -   Japanese - @ja@
--
-- -   Korean - @ko@
--
-- -   Portuguese, Brazilian - @pt_BR@
--
-- -   Spanish - @es@
newDescribeTrustedAdvisorChecks ::
  -- | 'language'
  Prelude.Text ->
  DescribeTrustedAdvisorChecks
newDescribeTrustedAdvisorChecks pLanguage_ =
  DescribeTrustedAdvisorChecks'
    { language =
        pLanguage_
    }

-- | The ISO 639-1 code for the language that you want your checks to appear
-- in.
--
-- The Amazon Web Services Support API currently supports the following
-- languages for Trusted Advisor:
--
-- -   Chinese, Simplified - @zh@
--
-- -   Chinese, Traditional - @zh_TW@
--
-- -   English - @en@
--
-- -   French - @fr@
--
-- -   German - @de@
--
-- -   Indonesian - @id@
--
-- -   Italian - @it@
--
-- -   Japanese - @ja@
--
-- -   Korean - @ko@
--
-- -   Portuguese, Brazilian - @pt_BR@
--
-- -   Spanish - @es@
describeTrustedAdvisorChecks_language :: Lens.Lens' DescribeTrustedAdvisorChecks Prelude.Text
describeTrustedAdvisorChecks_language = Lens.lens (\DescribeTrustedAdvisorChecks' {language} -> language) (\s@DescribeTrustedAdvisorChecks' {} a -> s {language = a} :: DescribeTrustedAdvisorChecks)

instance Core.AWSRequest DescribeTrustedAdvisorChecks where
  type
    AWSResponse DescribeTrustedAdvisorChecks =
      DescribeTrustedAdvisorChecksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorChecksResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "checks" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeTrustedAdvisorChecks
  where
  hashWithSalt _salt DescribeTrustedAdvisorChecks' {..} =
    _salt `Prelude.hashWithSalt` language

instance Prelude.NFData DescribeTrustedAdvisorChecks where
  rnf DescribeTrustedAdvisorChecks' {..} =
    Prelude.rnf language

instance Data.ToHeaders DescribeTrustedAdvisorChecks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorChecks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTrustedAdvisorChecks where
  toJSON DescribeTrustedAdvisorChecks' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("language" Data..= language)]
      )

instance Data.ToPath DescribeTrustedAdvisorChecks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrustedAdvisorChecks where
  toQuery = Prelude.const Prelude.mempty

-- | Information about the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorChecks operation.
--
-- /See:/ 'newDescribeTrustedAdvisorChecksResponse' smart constructor.
data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about all available Trusted Advisor checks.
    checks :: [TrustedAdvisorCheckDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTrustedAdvisorChecksResponse
newDescribeTrustedAdvisorChecksResponse pHttpStatus_ =
  DescribeTrustedAdvisorChecksResponse'
    { httpStatus =
        pHttpStatus_,
      checks = Prelude.mempty
    }

-- | The response's http status code.
describeTrustedAdvisorChecksResponse_httpStatus :: Lens.Lens' DescribeTrustedAdvisorChecksResponse Prelude.Int
describeTrustedAdvisorChecksResponse_httpStatus = Lens.lens (\DescribeTrustedAdvisorChecksResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustedAdvisorChecksResponse' {} a -> s {httpStatus = a} :: DescribeTrustedAdvisorChecksResponse)

-- | Information about all available Trusted Advisor checks.
describeTrustedAdvisorChecksResponse_checks :: Lens.Lens' DescribeTrustedAdvisorChecksResponse [TrustedAdvisorCheckDescription]
describeTrustedAdvisorChecksResponse_checks = Lens.lens (\DescribeTrustedAdvisorChecksResponse' {checks} -> checks) (\s@DescribeTrustedAdvisorChecksResponse' {} a -> s {checks = a} :: DescribeTrustedAdvisorChecksResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeTrustedAdvisorChecksResponse
  where
  rnf DescribeTrustedAdvisorChecksResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf checks
