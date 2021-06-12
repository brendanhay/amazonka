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
-- Module      : Network.AWS.Inspector.DescribeFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the findings that are specified by the ARNs of the findings.
module Network.AWS.Inspector.DescribeFindings
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFindings' smart constructor.
data DescribeFindings = DescribeFindings'
  { -- | The locale into which you want to translate a finding description,
    -- recommendation, and the short description that identifies the finding.
    locale :: Core.Maybe Locale,
    -- | The ARN that specifies the finding that you want to describe.
    findingArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeFindings
newDescribeFindings pFindingArns_ =
  DescribeFindings'
    { locale = Core.Nothing,
      findingArns = Lens._Coerce Lens.# pFindingArns_
    }

-- | The locale into which you want to translate a finding description,
-- recommendation, and the short description that identifies the finding.
describeFindings_locale :: Lens.Lens' DescribeFindings (Core.Maybe Locale)
describeFindings_locale = Lens.lens (\DescribeFindings' {locale} -> locale) (\s@DescribeFindings' {} a -> s {locale = a} :: DescribeFindings)

-- | The ARN that specifies the finding that you want to describe.
describeFindings_findingArns :: Lens.Lens' DescribeFindings (Core.NonEmpty Core.Text)
describeFindings_findingArns = Lens.lens (\DescribeFindings' {findingArns} -> findingArns) (\s@DescribeFindings' {} a -> s {findingArns = a} :: DescribeFindings) Core.. Lens._Coerce

instance Core.AWSRequest DescribeFindings where
  type
    AWSResponse DescribeFindings =
      DescribeFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "findings" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeFindings

instance Core.NFData DescribeFindings

instance Core.ToHeaders DescribeFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeFindings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeFindings where
  toJSON DescribeFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("locale" Core..=) Core.<$> locale,
            Core.Just ("findingArns" Core..= findingArns)
          ]
      )

instance Core.ToPath DescribeFindings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeFindingsResponse' smart constructor.
data DescribeFindingsResponse = DescribeFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about the finding.
    findings :: [Finding],
    -- | Finding details that cannot be described. An error code is provided for
    -- each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeFindingsResponse
newDescribeFindingsResponse pHttpStatus_ =
  DescribeFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      findings = Core.mempty,
      failedItems = Core.mempty
    }

-- | The response's http status code.
describeFindingsResponse_httpStatus :: Lens.Lens' DescribeFindingsResponse Core.Int
describeFindingsResponse_httpStatus = Lens.lens (\DescribeFindingsResponse' {httpStatus} -> httpStatus) (\s@DescribeFindingsResponse' {} a -> s {httpStatus = a} :: DescribeFindingsResponse)

-- | Information about the finding.
describeFindingsResponse_findings :: Lens.Lens' DescribeFindingsResponse [Finding]
describeFindingsResponse_findings = Lens.lens (\DescribeFindingsResponse' {findings} -> findings) (\s@DescribeFindingsResponse' {} a -> s {findings = a} :: DescribeFindingsResponse) Core.. Lens._Coerce

-- | Finding details that cannot be described. An error code is provided for
-- each failed item.
describeFindingsResponse_failedItems :: Lens.Lens' DescribeFindingsResponse (Core.HashMap Core.Text FailedItemDetails)
describeFindingsResponse_failedItems = Lens.lens (\DescribeFindingsResponse' {failedItems} -> failedItems) (\s@DescribeFindingsResponse' {} a -> s {failedItems = a} :: DescribeFindingsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeFindingsResponse
