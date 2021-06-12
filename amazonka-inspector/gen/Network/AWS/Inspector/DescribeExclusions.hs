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
-- Module      : Network.AWS.Inspector.DescribeExclusions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the exclusions that are specified by the exclusions\' ARNs.
module Network.AWS.Inspector.DescribeExclusions
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExclusions' smart constructor.
data DescribeExclusions = DescribeExclusions'
  { -- | The locale into which you want to translate the exclusion\'s title,
    -- description, and recommendation.
    locale :: Core.Maybe Locale,
    -- | The list of ARNs that specify the exclusions that you want to describe.
    exclusionArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeExclusions
newDescribeExclusions pExclusionArns_ =
  DescribeExclusions'
    { locale = Core.Nothing,
      exclusionArns = Lens._Coerce Lens.# pExclusionArns_
    }

-- | The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
describeExclusions_locale :: Lens.Lens' DescribeExclusions (Core.Maybe Locale)
describeExclusions_locale = Lens.lens (\DescribeExclusions' {locale} -> locale) (\s@DescribeExclusions' {} a -> s {locale = a} :: DescribeExclusions)

-- | The list of ARNs that specify the exclusions that you want to describe.
describeExclusions_exclusionArns :: Lens.Lens' DescribeExclusions (Core.NonEmpty Core.Text)
describeExclusions_exclusionArns = Lens.lens (\DescribeExclusions' {exclusionArns} -> exclusionArns) (\s@DescribeExclusions' {} a -> s {exclusionArns = a} :: DescribeExclusions) Core.. Lens._Coerce

instance Core.AWSRequest DescribeExclusions where
  type
    AWSResponse DescribeExclusions =
      DescribeExclusionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExclusionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "exclusions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeExclusions

instance Core.NFData DescribeExclusions

instance Core.ToHeaders DescribeExclusions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeExclusions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeExclusions where
  toJSON DescribeExclusions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("locale" Core..=) Core.<$> locale,
            Core.Just ("exclusionArns" Core..= exclusionArns)
          ]
      )

instance Core.ToPath DescribeExclusions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExclusions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExclusionsResponse' smart constructor.
data DescribeExclusionsResponse = DescribeExclusionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about the exclusions.
    exclusions :: Core.HashMap Core.Text Exclusion,
    -- | Exclusion details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeExclusionsResponse
newDescribeExclusionsResponse pHttpStatus_ =
  DescribeExclusionsResponse'
    { httpStatus =
        pHttpStatus_,
      exclusions = Core.mempty,
      failedItems = Core.mempty
    }

-- | The response's http status code.
describeExclusionsResponse_httpStatus :: Lens.Lens' DescribeExclusionsResponse Core.Int
describeExclusionsResponse_httpStatus = Lens.lens (\DescribeExclusionsResponse' {httpStatus} -> httpStatus) (\s@DescribeExclusionsResponse' {} a -> s {httpStatus = a} :: DescribeExclusionsResponse)

-- | Information about the exclusions.
describeExclusionsResponse_exclusions :: Lens.Lens' DescribeExclusionsResponse (Core.HashMap Core.Text Exclusion)
describeExclusionsResponse_exclusions = Lens.lens (\DescribeExclusionsResponse' {exclusions} -> exclusions) (\s@DescribeExclusionsResponse' {} a -> s {exclusions = a} :: DescribeExclusionsResponse) Core.. Lens._Coerce

-- | Exclusion details that cannot be described. An error code is provided
-- for each failed item.
describeExclusionsResponse_failedItems :: Lens.Lens' DescribeExclusionsResponse (Core.HashMap Core.Text FailedItemDetails)
describeExclusionsResponse_failedItems = Lens.lens (\DescribeExclusionsResponse' {failedItems} -> failedItems) (\s@DescribeExclusionsResponse' {} a -> s {failedItems = a} :: DescribeExclusionsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeExclusionsResponse
