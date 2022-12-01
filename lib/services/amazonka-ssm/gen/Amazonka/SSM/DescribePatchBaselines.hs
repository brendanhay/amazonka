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
-- Module      : Amazonka.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribePatchBaselines
  ( -- * Creating a Request
    DescribePatchBaselines (..),
    newDescribePatchBaselines,

    -- * Request Lenses
    describePatchBaselines_nextToken,
    describePatchBaselines_filters,
    describePatchBaselines_maxResults,

    -- * Destructuring the Response
    DescribePatchBaselinesResponse (..),
    newDescribePatchBaselinesResponse,

    -- * Response Lenses
    describePatchBaselinesResponse_nextToken,
    describePatchBaselinesResponse_baselineIdentities,
    describePatchBaselinesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each element in the array is a structure containing a key-value pair.
    --
    -- Supported keys for @DescribePatchBaselines@ include the following:
    --
    -- -   __@NAME_PREFIX@__
    --
    --     Sample values: @AWS-@ | @My-@
    --
    -- -   __@OWNER@__
    --
    --     Sample values: @AWS@ | @Self@
    --
    -- -   __@OPERATING_SYSTEM@__
    --
    --     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The maximum number of patch baselines to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchBaselines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchBaselines_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describePatchBaselines_filters' - Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribePatchBaselines@ include the following:
--
-- -   __@NAME_PREFIX@__
--
--     Sample values: @AWS-@ | @My-@
--
-- -   __@OWNER@__
--
--     Sample values: @AWS@ | @Self@
--
-- -   __@OPERATING_SYSTEM@__
--
--     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
--
-- 'maxResults', 'describePatchBaselines_maxResults' - The maximum number of patch baselines to return (per page).
newDescribePatchBaselines ::
  DescribePatchBaselines
newDescribePatchBaselines =
  DescribePatchBaselines'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchBaselines_nextToken :: Lens.Lens' DescribePatchBaselines (Prelude.Maybe Prelude.Text)
describePatchBaselines_nextToken = Lens.lens (\DescribePatchBaselines' {nextToken} -> nextToken) (\s@DescribePatchBaselines' {} a -> s {nextToken = a} :: DescribePatchBaselines)

-- | Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribePatchBaselines@ include the following:
--
-- -   __@NAME_PREFIX@__
--
--     Sample values: @AWS-@ | @My-@
--
-- -   __@OWNER@__
--
--     Sample values: @AWS@ | @Self@
--
-- -   __@OPERATING_SYSTEM@__
--
--     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
describePatchBaselines_filters :: Lens.Lens' DescribePatchBaselines (Prelude.Maybe [PatchOrchestratorFilter])
describePatchBaselines_filters = Lens.lens (\DescribePatchBaselines' {filters} -> filters) (\s@DescribePatchBaselines' {} a -> s {filters = a} :: DescribePatchBaselines) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of patch baselines to return (per page).
describePatchBaselines_maxResults :: Lens.Lens' DescribePatchBaselines (Prelude.Maybe Prelude.Natural)
describePatchBaselines_maxResults = Lens.lens (\DescribePatchBaselines' {maxResults} -> maxResults) (\s@DescribePatchBaselines' {} a -> s {maxResults = a} :: DescribePatchBaselines)

instance Core.AWSPager DescribePatchBaselines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePatchBaselinesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchBaselinesResponse_baselineIdentities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePatchBaselines_nextToken
          Lens..~ rs
          Lens.^? describePatchBaselinesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePatchBaselines where
  type
    AWSResponse DescribePatchBaselines =
      DescribePatchBaselinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchBaselinesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "BaselineIdentities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePatchBaselines where
  hashWithSalt _salt DescribePatchBaselines' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribePatchBaselines where
  rnf DescribePatchBaselines' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribePatchBaselines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribePatchBaselines" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePatchBaselines where
  toJSON DescribePatchBaselines' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribePatchBaselines where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePatchBaselines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @PatchBaselineIdentity@ elements.
    baselineIdentities :: Prelude.Maybe [PatchBaselineIdentity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchBaselinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchBaselinesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'baselineIdentities', 'describePatchBaselinesResponse_baselineIdentities' - An array of @PatchBaselineIdentity@ elements.
--
-- 'httpStatus', 'describePatchBaselinesResponse_httpStatus' - The response's http status code.
newDescribePatchBaselinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePatchBaselinesResponse
newDescribePatchBaselinesResponse pHttpStatus_ =
  DescribePatchBaselinesResponse'
    { nextToken =
        Prelude.Nothing,
      baselineIdentities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describePatchBaselinesResponse_nextToken :: Lens.Lens' DescribePatchBaselinesResponse (Prelude.Maybe Prelude.Text)
describePatchBaselinesResponse_nextToken = Lens.lens (\DescribePatchBaselinesResponse' {nextToken} -> nextToken) (\s@DescribePatchBaselinesResponse' {} a -> s {nextToken = a} :: DescribePatchBaselinesResponse)

-- | An array of @PatchBaselineIdentity@ elements.
describePatchBaselinesResponse_baselineIdentities :: Lens.Lens' DescribePatchBaselinesResponse (Prelude.Maybe [PatchBaselineIdentity])
describePatchBaselinesResponse_baselineIdentities = Lens.lens (\DescribePatchBaselinesResponse' {baselineIdentities} -> baselineIdentities) (\s@DescribePatchBaselinesResponse' {} a -> s {baselineIdentities = a} :: DescribePatchBaselinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePatchBaselinesResponse_httpStatus :: Lens.Lens' DescribePatchBaselinesResponse Prelude.Int
describePatchBaselinesResponse_httpStatus = Lens.lens (\DescribePatchBaselinesResponse' {httpStatus} -> httpStatus) (\s@DescribePatchBaselinesResponse' {} a -> s {httpStatus = a} :: DescribePatchBaselinesResponse)

instance
  Prelude.NFData
    DescribePatchBaselinesResponse
  where
  rnf DescribePatchBaselinesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf baselineIdentities
      `Prelude.seq` Prelude.rnf httpStatus
