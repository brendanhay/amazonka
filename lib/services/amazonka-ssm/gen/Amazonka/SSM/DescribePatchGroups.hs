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
-- Module      : Amazonka.SSM.DescribePatchGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patch groups that have been registered with patch baselines.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribePatchGroups
  ( -- * Creating a Request
    DescribePatchGroups (..),
    newDescribePatchGroups,

    -- * Request Lenses
    describePatchGroups_nextToken,
    describePatchGroups_filters,
    describePatchGroups_maxResults,

    -- * Destructuring the Response
    DescribePatchGroupsResponse (..),
    newDescribePatchGroupsResponse,

    -- * Response Lenses
    describePatchGroupsResponse_nextToken,
    describePatchGroupsResponse_mappings,
    describePatchGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribePatchGroups' smart constructor.
data DescribePatchGroups = DescribePatchGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each element in the array is a structure containing a key-value pair.
    --
    -- Supported keys for @DescribePatchGroups@ include the following:
    --
    -- -   __@NAME_PREFIX@__
    --
    --     Sample values: @AWS-@ | @My-@.
    --
    -- -   __@OPERATING_SYSTEM@__
    --
    --     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The maximum number of patch groups to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describePatchGroups_filters' - Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribePatchGroups@ include the following:
--
-- -   __@NAME_PREFIX@__
--
--     Sample values: @AWS-@ | @My-@.
--
-- -   __@OPERATING_SYSTEM@__
--
--     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
--
-- 'maxResults', 'describePatchGroups_maxResults' - The maximum number of patch groups to return (per page).
newDescribePatchGroups ::
  DescribePatchGroups
newDescribePatchGroups =
  DescribePatchGroups'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchGroups_nextToken :: Lens.Lens' DescribePatchGroups (Prelude.Maybe Prelude.Text)
describePatchGroups_nextToken = Lens.lens (\DescribePatchGroups' {nextToken} -> nextToken) (\s@DescribePatchGroups' {} a -> s {nextToken = a} :: DescribePatchGroups)

-- | Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribePatchGroups@ include the following:
--
-- -   __@NAME_PREFIX@__
--
--     Sample values: @AWS-@ | @My-@.
--
-- -   __@OPERATING_SYSTEM@__
--
--     Sample values: @AMAZON_LINUX@ | @SUSE@ | @WINDOWS@
describePatchGroups_filters :: Lens.Lens' DescribePatchGroups (Prelude.Maybe [PatchOrchestratorFilter])
describePatchGroups_filters = Lens.lens (\DescribePatchGroups' {filters} -> filters) (\s@DescribePatchGroups' {} a -> s {filters = a} :: DescribePatchGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of patch groups to return (per page).
describePatchGroups_maxResults :: Lens.Lens' DescribePatchGroups (Prelude.Maybe Prelude.Natural)
describePatchGroups_maxResults = Lens.lens (\DescribePatchGroups' {maxResults} -> maxResults) (\s@DescribePatchGroups' {} a -> s {maxResults = a} :: DescribePatchGroups)

instance Core.AWSPager DescribePatchGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePatchGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchGroupsResponse_mappings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePatchGroups_nextToken
          Lens..~ rs
          Lens.^? describePatchGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePatchGroups where
  type
    AWSResponse DescribePatchGroups =
      DescribePatchGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Mappings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePatchGroups where
  hashWithSalt _salt DescribePatchGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribePatchGroups where
  rnf DescribePatchGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribePatchGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribePatchGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePatchGroups where
  toJSON DescribePatchGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribePatchGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePatchGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePatchGroupsResponse' smart constructor.
data DescribePatchGroupsResponse = DescribePatchGroupsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each entry in the array contains:
    --
    -- -   @PatchGroup@: string (between 1 and 256 characters. Regex:
    --     @^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)@
    --
    -- -   @PatchBaselineIdentity@: A @PatchBaselineIdentity@ element.
    mappings :: Prelude.Maybe [PatchGroupPatchBaselineMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchGroupsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'mappings', 'describePatchGroupsResponse_mappings' - Each entry in the array contains:
--
-- -   @PatchGroup@: string (between 1 and 256 characters. Regex:
--     @^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)@
--
-- -   @PatchBaselineIdentity@: A @PatchBaselineIdentity@ element.
--
-- 'httpStatus', 'describePatchGroupsResponse_httpStatus' - The response's http status code.
newDescribePatchGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePatchGroupsResponse
newDescribePatchGroupsResponse pHttpStatus_ =
  DescribePatchGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      mappings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describePatchGroupsResponse_nextToken :: Lens.Lens' DescribePatchGroupsResponse (Prelude.Maybe Prelude.Text)
describePatchGroupsResponse_nextToken = Lens.lens (\DescribePatchGroupsResponse' {nextToken} -> nextToken) (\s@DescribePatchGroupsResponse' {} a -> s {nextToken = a} :: DescribePatchGroupsResponse)

-- | Each entry in the array contains:
--
-- -   @PatchGroup@: string (between 1 and 256 characters. Regex:
--     @^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)@
--
-- -   @PatchBaselineIdentity@: A @PatchBaselineIdentity@ element.
describePatchGroupsResponse_mappings :: Lens.Lens' DescribePatchGroupsResponse (Prelude.Maybe [PatchGroupPatchBaselineMapping])
describePatchGroupsResponse_mappings = Lens.lens (\DescribePatchGroupsResponse' {mappings} -> mappings) (\s@DescribePatchGroupsResponse' {} a -> s {mappings = a} :: DescribePatchGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePatchGroupsResponse_httpStatus :: Lens.Lens' DescribePatchGroupsResponse Prelude.Int
describePatchGroupsResponse_httpStatus = Lens.lens (\DescribePatchGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribePatchGroupsResponse' {} a -> s {httpStatus = a} :: DescribePatchGroupsResponse)

instance Prelude.NFData DescribePatchGroupsResponse where
  rnf DescribePatchGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf mappings
      `Prelude.seq` Prelude.rnf httpStatus
