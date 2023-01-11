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
-- Module      : Amazonka.SSM.DescribeEffectivePatchesForPatchBaseline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current effective patches (the patch and the approval
-- state) for the specified patch baseline. Applies to patch baselines for
-- Windows only.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeEffectivePatchesForPatchBaseline
  ( -- * Creating a Request
    DescribeEffectivePatchesForPatchBaseline (..),
    newDescribeEffectivePatchesForPatchBaseline,

    -- * Request Lenses
    describeEffectivePatchesForPatchBaseline_maxResults,
    describeEffectivePatchesForPatchBaseline_nextToken,
    describeEffectivePatchesForPatchBaseline_baselineId,

    -- * Destructuring the Response
    DescribeEffectivePatchesForPatchBaselineResponse (..),
    newDescribeEffectivePatchesForPatchBaselineResponse,

    -- * Response Lenses
    describeEffectivePatchesForPatchBaselineResponse_effectivePatches,
    describeEffectivePatchesForPatchBaselineResponse_nextToken,
    describeEffectivePatchesForPatchBaselineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeEffectivePatchesForPatchBaseline' smart constructor.
data DescribeEffectivePatchesForPatchBaseline = DescribeEffectivePatchesForPatchBaseline'
  { -- | The maximum number of patches to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the patch baseline to retrieve the effective patches for.
    baselineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEffectivePatchesForPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeEffectivePatchesForPatchBaseline_maxResults' - The maximum number of patches to return (per page).
--
-- 'nextToken', 'describeEffectivePatchesForPatchBaseline_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'baselineId', 'describeEffectivePatchesForPatchBaseline_baselineId' - The ID of the patch baseline to retrieve the effective patches for.
newDescribeEffectivePatchesForPatchBaseline ::
  -- | 'baselineId'
  Prelude.Text ->
  DescribeEffectivePatchesForPatchBaseline
newDescribeEffectivePatchesForPatchBaseline
  pBaselineId_ =
    DescribeEffectivePatchesForPatchBaseline'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        baselineId = pBaselineId_
      }

-- | The maximum number of patches to return (per page).
describeEffectivePatchesForPatchBaseline_maxResults :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Prelude.Maybe Prelude.Natural)
describeEffectivePatchesForPatchBaseline_maxResults = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {maxResults} -> maxResults) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {maxResults = a} :: DescribeEffectivePatchesForPatchBaseline)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeEffectivePatchesForPatchBaseline_nextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Prelude.Maybe Prelude.Text)
describeEffectivePatchesForPatchBaseline_nextToken = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {nextToken} -> nextToken) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaseline)

-- | The ID of the patch baseline to retrieve the effective patches for.
describeEffectivePatchesForPatchBaseline_baselineId :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline Prelude.Text
describeEffectivePatchesForPatchBaseline_baselineId = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {baselineId} -> baselineId) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {baselineId = a} :: DescribeEffectivePatchesForPatchBaseline)

instance
  Core.AWSPager
    DescribeEffectivePatchesForPatchBaseline
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_effectivePatches
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEffectivePatchesForPatchBaseline_nextToken
          Lens..~ rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEffectivePatchesForPatchBaseline
  where
  type
    AWSResponse
      DescribeEffectivePatchesForPatchBaseline =
      DescribeEffectivePatchesForPatchBaselineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePatchesForPatchBaselineResponse'
            Prelude.<$> ( x Data..?> "EffectivePatches"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEffectivePatchesForPatchBaseline
  where
  hashWithSalt
    _salt
    DescribeEffectivePatchesForPatchBaseline' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` baselineId

instance
  Prelude.NFData
    DescribeEffectivePatchesForPatchBaseline
  where
  rnf DescribeEffectivePatchesForPatchBaseline' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf baselineId

instance
  Data.ToHeaders
    DescribeEffectivePatchesForPatchBaseline
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeEffectivePatchesForPatchBaseline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeEffectivePatchesForPatchBaseline
  where
  toJSON DescribeEffectivePatchesForPatchBaseline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("BaselineId" Data..= baselineId)
          ]
      )

instance
  Data.ToPath
    DescribeEffectivePatchesForPatchBaseline
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeEffectivePatchesForPatchBaseline
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEffectivePatchesForPatchBaselineResponse' smart constructor.
data DescribeEffectivePatchesForPatchBaselineResponse = DescribeEffectivePatchesForPatchBaselineResponse'
  { -- | An array of patches and patch status.
    effectivePatches :: Prelude.Maybe [EffectivePatch],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEffectivePatchesForPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectivePatches', 'describeEffectivePatchesForPatchBaselineResponse_effectivePatches' - An array of patches and patch status.
--
-- 'nextToken', 'describeEffectivePatchesForPatchBaselineResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'describeEffectivePatchesForPatchBaselineResponse_httpStatus' - The response's http status code.
newDescribeEffectivePatchesForPatchBaselineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEffectivePatchesForPatchBaselineResponse
newDescribeEffectivePatchesForPatchBaselineResponse
  pHttpStatus_ =
    DescribeEffectivePatchesForPatchBaselineResponse'
      { effectivePatches =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of patches and patch status.
describeEffectivePatchesForPatchBaselineResponse_effectivePatches :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Prelude.Maybe [EffectivePatch])
describeEffectivePatchesForPatchBaselineResponse_effectivePatches = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {effectivePatches} -> effectivePatches) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {effectivePatches = a} :: DescribeEffectivePatchesForPatchBaselineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeEffectivePatchesForPatchBaselineResponse_nextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Prelude.Maybe Prelude.Text)
describeEffectivePatchesForPatchBaselineResponse_nextToken = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {nextToken} -> nextToken) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaselineResponse)

-- | The response's http status code.
describeEffectivePatchesForPatchBaselineResponse_httpStatus :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse Prelude.Int
describeEffectivePatchesForPatchBaselineResponse_httpStatus = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {httpStatus = a} :: DescribeEffectivePatchesForPatchBaselineResponse)

instance
  Prelude.NFData
    DescribeEffectivePatchesForPatchBaselineResponse
  where
  rnf
    DescribeEffectivePatchesForPatchBaselineResponse' {..} =
      Prelude.rnf effectivePatches
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
