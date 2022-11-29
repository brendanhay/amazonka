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
-- Module      : Amazonka.Inspector.DescribeFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the findings that are specified by the ARNs of the findings.
module Amazonka.Inspector.DescribeFindings
  ( -- * Creating a Request
    DescribeFindings (..),
    newDescribeFindings,

    -- * Request Lenses
    describeFindings_locale,
    describeFindings_findingArns,

    -- * Destructuring the Response
    DescribeFindingsResponse (..),
    newDescribeFindingsResponse,

    -- * Response Lenses
    describeFindingsResponse_httpStatus,
    describeFindingsResponse_findings,
    describeFindingsResponse_failedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFindings' smart constructor.
data DescribeFindings = DescribeFindings'
  { -- | The locale into which you want to translate a finding description,
    -- recommendation, and the short description that identifies the finding.
    locale :: Prelude.Maybe Locale,
    -- | The ARN that specifies the finding that you want to describe.
    findingArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'describeFindings_locale' - The locale into which you want to translate a finding description,
-- recommendation, and the short description that identifies the finding.
--
-- 'findingArns', 'describeFindings_findingArns' - The ARN that specifies the finding that you want to describe.
newDescribeFindings ::
  -- | 'findingArns'
  Prelude.NonEmpty Prelude.Text ->
  DescribeFindings
newDescribeFindings pFindingArns_ =
  DescribeFindings'
    { locale = Prelude.Nothing,
      findingArns = Lens.coerced Lens.# pFindingArns_
    }

-- | The locale into which you want to translate a finding description,
-- recommendation, and the short description that identifies the finding.
describeFindings_locale :: Lens.Lens' DescribeFindings (Prelude.Maybe Locale)
describeFindings_locale = Lens.lens (\DescribeFindings' {locale} -> locale) (\s@DescribeFindings' {} a -> s {locale = a} :: DescribeFindings)

-- | The ARN that specifies the finding that you want to describe.
describeFindings_findingArns :: Lens.Lens' DescribeFindings (Prelude.NonEmpty Prelude.Text)
describeFindings_findingArns = Lens.lens (\DescribeFindings' {findingArns} -> findingArns) (\s@DescribeFindings' {} a -> s {findingArns = a} :: DescribeFindings) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeFindings where
  type
    AWSResponse DescribeFindings =
      DescribeFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "findings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeFindings where
  hashWithSalt _salt DescribeFindings' {..} =
    _salt `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` findingArns

instance Prelude.NFData DescribeFindings where
  rnf DescribeFindings' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf findingArns

instance Core.ToHeaders DescribeFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeFindings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFindings where
  toJSON DescribeFindings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("locale" Core..=) Prelude.<$> locale,
            Prelude.Just ("findingArns" Core..= findingArns)
          ]
      )

instance Core.ToPath DescribeFindings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFindingsResponse' smart constructor.
data DescribeFindingsResponse = DescribeFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the finding.
    findings :: [Finding],
    -- | Finding details that cannot be described. An error code is provided for
    -- each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeFindingsResponse_httpStatus' - The response's http status code.
--
-- 'findings', 'describeFindingsResponse_findings' - Information about the finding.
--
-- 'failedItems', 'describeFindingsResponse_failedItems' - Finding details that cannot be described. An error code is provided for
-- each failed item.
newDescribeFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFindingsResponse
newDescribeFindingsResponse pHttpStatus_ =
  DescribeFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      findings = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeFindingsResponse_httpStatus :: Lens.Lens' DescribeFindingsResponse Prelude.Int
describeFindingsResponse_httpStatus = Lens.lens (\DescribeFindingsResponse' {httpStatus} -> httpStatus) (\s@DescribeFindingsResponse' {} a -> s {httpStatus = a} :: DescribeFindingsResponse)

-- | Information about the finding.
describeFindingsResponse_findings :: Lens.Lens' DescribeFindingsResponse [Finding]
describeFindingsResponse_findings = Lens.lens (\DescribeFindingsResponse' {findings} -> findings) (\s@DescribeFindingsResponse' {} a -> s {findings = a} :: DescribeFindingsResponse) Prelude.. Lens.coerced

-- | Finding details that cannot be described. An error code is provided for
-- each failed item.
describeFindingsResponse_failedItems :: Lens.Lens' DescribeFindingsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeFindingsResponse_failedItems = Lens.lens (\DescribeFindingsResponse' {failedItems} -> failedItems) (\s@DescribeFindingsResponse' {} a -> s {failedItems = a} :: DescribeFindingsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeFindingsResponse where
  rnf DescribeFindingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf findings
      `Prelude.seq` Prelude.rnf failedItems
