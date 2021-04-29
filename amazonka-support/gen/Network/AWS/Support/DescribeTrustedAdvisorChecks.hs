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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeTrustedAdvisorChecks' smart constructor.
data DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
describeTrustedAdvisorChecks_language :: Lens.Lens' DescribeTrustedAdvisorChecks Prelude.Text
describeTrustedAdvisorChecks_language = Lens.lens (\DescribeTrustedAdvisorChecks' {language} -> language) (\s@DescribeTrustedAdvisorChecks' {} a -> s {language = a} :: DescribeTrustedAdvisorChecks)

instance
  Prelude.AWSRequest
    DescribeTrustedAdvisorChecks
  where
  type
    Rs DescribeTrustedAdvisorChecks =
      DescribeTrustedAdvisorChecksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorChecksResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "checks" Prelude..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeTrustedAdvisorChecks

instance Prelude.NFData DescribeTrustedAdvisorChecks

instance
  Prelude.ToHeaders
    DescribeTrustedAdvisorChecks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorChecks" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTrustedAdvisorChecks where
  toJSON DescribeTrustedAdvisorChecks' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("language" Prelude..= language)]
      )

instance Prelude.ToPath DescribeTrustedAdvisorChecks where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTrustedAdvisorChecks where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeTrustedAdvisorChecksResponse_checks = Lens.lens (\DescribeTrustedAdvisorChecksResponse' {checks} -> checks) (\s@DescribeTrustedAdvisorChecksResponse' {} a -> s {checks = a} :: DescribeTrustedAdvisorChecksResponse) Prelude.. Prelude._Coerce

instance
  Prelude.NFData
    DescribeTrustedAdvisorChecksResponse
