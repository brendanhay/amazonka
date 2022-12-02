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
-- Module      : Amazonka.SSM.DescribeInstancePatchStatesForPatchGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state for the managed nodes in the
-- specified patch group.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeInstancePatchStatesForPatchGroup
  ( -- * Creating a Request
    DescribeInstancePatchStatesForPatchGroup (..),
    newDescribeInstancePatchStatesForPatchGroup,

    -- * Request Lenses
    describeInstancePatchStatesForPatchGroup_nextToken,
    describeInstancePatchStatesForPatchGroup_filters,
    describeInstancePatchStatesForPatchGroup_maxResults,
    describeInstancePatchStatesForPatchGroup_patchGroup,

    -- * Destructuring the Response
    DescribeInstancePatchStatesForPatchGroupResponse (..),
    newDescribeInstancePatchStatesForPatchGroupResponse,

    -- * Response Lenses
    describeInstancePatchStatesForPatchGroupResponse_nextToken,
    describeInstancePatchStatesForPatchGroupResponse_instancePatchStates,
    describeInstancePatchStatesForPatchGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeInstancePatchStatesForPatchGroup' smart constructor.
data DescribeInstancePatchStatesForPatchGroup = DescribeInstancePatchStatesForPatchGroup'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each entry in the array is a structure containing:
    --
    -- -   Key (string between 1 and 200 characters)
    --
    -- -   Values (array containing a single string)
    --
    -- -   Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
    filters :: Prelude.Maybe [InstancePatchStateFilter],
    -- | The maximum number of patches to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the patch group for which the patch state information should
    -- be retrieved.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesForPatchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStatesForPatchGroup_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describeInstancePatchStatesForPatchGroup_filters' - Each entry in the array is a structure containing:
--
-- -   Key (string between 1 and 200 characters)
--
-- -   Values (array containing a single string)
--
-- -   Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
--
-- 'maxResults', 'describeInstancePatchStatesForPatchGroup_maxResults' - The maximum number of patches to return (per page).
--
-- 'patchGroup', 'describeInstancePatchStatesForPatchGroup_patchGroup' - The name of the patch group for which the patch state information should
-- be retrieved.
newDescribeInstancePatchStatesForPatchGroup ::
  -- | 'patchGroup'
  Prelude.Text ->
  DescribeInstancePatchStatesForPatchGroup
newDescribeInstancePatchStatesForPatchGroup
  pPatchGroup_ =
    DescribeInstancePatchStatesForPatchGroup'
      { nextToken =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        patchGroup = pPatchGroup_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatchStatesForPatchGroup_nextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Prelude.Maybe Prelude.Text)
describeInstancePatchStatesForPatchGroup_nextToken = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroup)

-- | Each entry in the array is a structure containing:
--
-- -   Key (string between 1 and 200 characters)
--
-- -   Values (array containing a single string)
--
-- -   Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
describeInstancePatchStatesForPatchGroup_filters :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Prelude.Maybe [InstancePatchStateFilter])
describeInstancePatchStatesForPatchGroup_filters = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {filters} -> filters) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {filters = a} :: DescribeInstancePatchStatesForPatchGroup) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of patches to return (per page).
describeInstancePatchStatesForPatchGroup_maxResults :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Prelude.Maybe Prelude.Natural)
describeInstancePatchStatesForPatchGroup_maxResults = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {maxResults} -> maxResults) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {maxResults = a} :: DescribeInstancePatchStatesForPatchGroup)

-- | The name of the patch group for which the patch state information should
-- be retrieved.
describeInstancePatchStatesForPatchGroup_patchGroup :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup Prelude.Text
describeInstancePatchStatesForPatchGroup_patchGroup = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {patchGroup} -> patchGroup) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {patchGroup = a} :: DescribeInstancePatchStatesForPatchGroup)

instance
  Core.AWSPager
    DescribeInstancePatchStatesForPatchGroup
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_instancePatchStates
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstancePatchStatesForPatchGroup_nextToken
          Lens..~ rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstancePatchStatesForPatchGroup
  where
  type
    AWSResponse
      DescribeInstancePatchStatesForPatchGroup =
      DescribeInstancePatchStatesForPatchGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesForPatchGroupResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "InstancePatchStates")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstancePatchStatesForPatchGroup
  where
  hashWithSalt
    _salt
    DescribeInstancePatchStatesForPatchGroup' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` patchGroup

instance
  Prelude.NFData
    DescribeInstancePatchStatesForPatchGroup
  where
  rnf DescribeInstancePatchStatesForPatchGroup' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf patchGroup

instance
  Data.ToHeaders
    DescribeInstancePatchStatesForPatchGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeInstancePatchStatesForPatchGroup" ::
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
    DescribeInstancePatchStatesForPatchGroup
  where
  toJSON DescribeInstancePatchStatesForPatchGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("PatchGroup" Data..= patchGroup)
          ]
      )

instance
  Data.ToPath
    DescribeInstancePatchStatesForPatchGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeInstancePatchStatesForPatchGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancePatchStatesForPatchGroupResponse' smart constructor.
data DescribeInstancePatchStatesForPatchGroupResponse = DescribeInstancePatchStatesForPatchGroupResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The high-level patch state for the requested managed nodes.
    instancePatchStates :: Prelude.Maybe (Prelude.NonEmpty InstancePatchState),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesForPatchGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStatesForPatchGroupResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instancePatchStates', 'describeInstancePatchStatesForPatchGroupResponse_instancePatchStates' - The high-level patch state for the requested managed nodes.
--
-- 'httpStatus', 'describeInstancePatchStatesForPatchGroupResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchStatesForPatchGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancePatchStatesForPatchGroupResponse
newDescribeInstancePatchStatesForPatchGroupResponse
  pHttpStatus_ =
    DescribeInstancePatchStatesForPatchGroupResponse'
      { nextToken =
          Prelude.Nothing,
        instancePatchStates =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchStatesForPatchGroupResponse_nextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Prelude.Maybe Prelude.Text)
describeInstancePatchStatesForPatchGroupResponse_nextToken = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroupResponse)

-- | The high-level patch state for the requested managed nodes.
describeInstancePatchStatesForPatchGroupResponse_instancePatchStates :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Prelude.Maybe (Prelude.NonEmpty InstancePatchState))
describeInstancePatchStatesForPatchGroupResponse_instancePatchStates = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {instancePatchStates} -> instancePatchStates) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesForPatchGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstancePatchStatesForPatchGroupResponse_httpStatus :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse Prelude.Int
describeInstancePatchStatesForPatchGroupResponse_httpStatus = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchStatesForPatchGroupResponse)

instance
  Prelude.NFData
    DescribeInstancePatchStatesForPatchGroupResponse
  where
  rnf
    DescribeInstancePatchStatesForPatchGroupResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf instancePatchStates
        `Prelude.seq` Prelude.rnf httpStatus
