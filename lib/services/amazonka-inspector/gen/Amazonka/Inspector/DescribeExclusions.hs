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
-- Module      : Amazonka.Inspector.DescribeExclusions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the exclusions that are specified by the exclusions\' ARNs.
module Amazonka.Inspector.DescribeExclusions
  ( -- * Creating a Request
    DescribeExclusions (..),
    newDescribeExclusions,

    -- * Request Lenses
    describeExclusions_locale,
    describeExclusions_exclusionArns,

    -- * Destructuring the Response
    DescribeExclusionsResponse (..),
    newDescribeExclusionsResponse,

    -- * Response Lenses
    describeExclusionsResponse_httpStatus,
    describeExclusionsResponse_exclusions,
    describeExclusionsResponse_failedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExclusions' smart constructor.
data DescribeExclusions = DescribeExclusions'
  { -- | The locale into which you want to translate the exclusion\'s title,
    -- description, and recommendation.
    locale :: Prelude.Maybe Locale,
    -- | The list of ARNs that specify the exclusions that you want to describe.
    exclusionArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExclusions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'describeExclusions_locale' - The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
--
-- 'exclusionArns', 'describeExclusions_exclusionArns' - The list of ARNs that specify the exclusions that you want to describe.
newDescribeExclusions ::
  -- | 'exclusionArns'
  Prelude.NonEmpty Prelude.Text ->
  DescribeExclusions
newDescribeExclusions pExclusionArns_ =
  DescribeExclusions'
    { locale = Prelude.Nothing,
      exclusionArns = Lens.coerced Lens.# pExclusionArns_
    }

-- | The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
describeExclusions_locale :: Lens.Lens' DescribeExclusions (Prelude.Maybe Locale)
describeExclusions_locale = Lens.lens (\DescribeExclusions' {locale} -> locale) (\s@DescribeExclusions' {} a -> s {locale = a} :: DescribeExclusions)

-- | The list of ARNs that specify the exclusions that you want to describe.
describeExclusions_exclusionArns :: Lens.Lens' DescribeExclusions (Prelude.NonEmpty Prelude.Text)
describeExclusions_exclusionArns = Lens.lens (\DescribeExclusions' {exclusionArns} -> exclusionArns) (\s@DescribeExclusions' {} a -> s {exclusionArns = a} :: DescribeExclusions) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeExclusions where
  type
    AWSResponse DescribeExclusions =
      DescribeExclusionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExclusionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "exclusions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeExclusions where
  hashWithSalt _salt DescribeExclusions' {..} =
    _salt `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` exclusionArns

instance Prelude.NFData DescribeExclusions where
  rnf DescribeExclusions' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf exclusionArns

instance Data.ToHeaders DescribeExclusions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.DescribeExclusions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeExclusions where
  toJSON DescribeExclusions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            Prelude.Just
              ("exclusionArns" Data..= exclusionArns)
          ]
      )

instance Data.ToPath DescribeExclusions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExclusions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExclusionsResponse' smart constructor.
data DescribeExclusionsResponse = DescribeExclusionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the exclusions.
    exclusions :: Prelude.HashMap Prelude.Text Exclusion,
    -- | Exclusion details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExclusionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeExclusionsResponse_httpStatus' - The response's http status code.
--
-- 'exclusions', 'describeExclusionsResponse_exclusions' - Information about the exclusions.
--
-- 'failedItems', 'describeExclusionsResponse_failedItems' - Exclusion details that cannot be described. An error code is provided
-- for each failed item.
newDescribeExclusionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExclusionsResponse
newDescribeExclusionsResponse pHttpStatus_ =
  DescribeExclusionsResponse'
    { httpStatus =
        pHttpStatus_,
      exclusions = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeExclusionsResponse_httpStatus :: Lens.Lens' DescribeExclusionsResponse Prelude.Int
describeExclusionsResponse_httpStatus = Lens.lens (\DescribeExclusionsResponse' {httpStatus} -> httpStatus) (\s@DescribeExclusionsResponse' {} a -> s {httpStatus = a} :: DescribeExclusionsResponse)

-- | Information about the exclusions.
describeExclusionsResponse_exclusions :: Lens.Lens' DescribeExclusionsResponse (Prelude.HashMap Prelude.Text Exclusion)
describeExclusionsResponse_exclusions = Lens.lens (\DescribeExclusionsResponse' {exclusions} -> exclusions) (\s@DescribeExclusionsResponse' {} a -> s {exclusions = a} :: DescribeExclusionsResponse) Prelude.. Lens.coerced

-- | Exclusion details that cannot be described. An error code is provided
-- for each failed item.
describeExclusionsResponse_failedItems :: Lens.Lens' DescribeExclusionsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeExclusionsResponse_failedItems = Lens.lens (\DescribeExclusionsResponse' {failedItems} -> failedItems) (\s@DescribeExclusionsResponse' {} a -> s {failedItems = a} :: DescribeExclusionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeExclusionsResponse where
  rnf DescribeExclusionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exclusions
      `Prelude.seq` Prelude.rnf failedItems
