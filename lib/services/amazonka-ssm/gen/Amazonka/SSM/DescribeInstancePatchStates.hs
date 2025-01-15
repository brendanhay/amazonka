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
-- Module      : Amazonka.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more managed nodes.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeInstancePatchStates
  ( -- * Creating a Request
    DescribeInstancePatchStates (..),
    newDescribeInstancePatchStates,

    -- * Request Lenses
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_instanceIds,

    -- * Destructuring the Response
    DescribeInstancePatchStatesResponse (..),
    newDescribeInstancePatchStatesResponse,

    -- * Response Lenses
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { -- | The maximum number of managed nodes to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the managed node for which patch state information should be
    -- retrieved.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeInstancePatchStates_maxResults' - The maximum number of managed nodes to return (per page).
--
-- 'nextToken', 'describeInstancePatchStates_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'instanceIds', 'describeInstancePatchStates_instanceIds' - The ID of the managed node for which patch state information should be
-- retrieved.
newDescribeInstancePatchStates ::
  DescribeInstancePatchStates
newDescribeInstancePatchStates =
  DescribeInstancePatchStates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | The maximum number of managed nodes to return (per page).
describeInstancePatchStates_maxResults :: Lens.Lens' DescribeInstancePatchStates (Prelude.Maybe Prelude.Natural)
describeInstancePatchStates_maxResults = Lens.lens (\DescribeInstancePatchStates' {maxResults} -> maxResults) (\s@DescribeInstancePatchStates' {} a -> s {maxResults = a} :: DescribeInstancePatchStates)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatchStates_nextToken :: Lens.Lens' DescribeInstancePatchStates (Prelude.Maybe Prelude.Text)
describeInstancePatchStates_nextToken = Lens.lens (\DescribeInstancePatchStates' {nextToken} -> nextToken) (\s@DescribeInstancePatchStates' {} a -> s {nextToken = a} :: DescribeInstancePatchStates)

-- | The ID of the managed node for which patch state information should be
-- retrieved.
describeInstancePatchStates_instanceIds :: Lens.Lens' DescribeInstancePatchStates [Prelude.Text]
describeInstancePatchStates_instanceIds = Lens.lens (\DescribeInstancePatchStates' {instanceIds} -> instanceIds) (\s@DescribeInstancePatchStates' {} a -> s {instanceIds = a} :: DescribeInstancePatchStates) Prelude.. Lens.coerced

instance Core.AWSPager DescribeInstancePatchStates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesResponse_instancePatchStates
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeInstancePatchStates_nextToken
              Lens..~ rs
              Lens.^? describeInstancePatchStatesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstancePatchStates where
  type
    AWSResponse DescribeInstancePatchStates =
      DescribeInstancePatchStatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesResponse'
            Prelude.<$> ( x
                            Data..?> "InstancePatchStates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstancePatchStates where
  hashWithSalt _salt DescribeInstancePatchStates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData DescribeInstancePatchStates where
  rnf DescribeInstancePatchStates' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf instanceIds

instance Data.ToHeaders DescribeInstancePatchStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeInstancePatchStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeInstancePatchStates where
  toJSON DescribeInstancePatchStates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InstanceIds" Data..= instanceIds)
          ]
      )

instance Data.ToPath DescribeInstancePatchStates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstancePatchStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { -- | The high-level patch state for the requested managed nodes.
    instancePatchStates :: Prelude.Maybe [InstancePatchState],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancePatchStates', 'describeInstancePatchStatesResponse_instancePatchStates' - The high-level patch state for the requested managed nodes.
--
-- 'nextToken', 'describeInstancePatchStatesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'describeInstancePatchStatesResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancePatchStatesResponse
newDescribeInstancePatchStatesResponse pHttpStatus_ =
  DescribeInstancePatchStatesResponse'
    { instancePatchStates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The high-level patch state for the requested managed nodes.
describeInstancePatchStatesResponse_instancePatchStates :: Lens.Lens' DescribeInstancePatchStatesResponse (Prelude.Maybe [InstancePatchState])
describeInstancePatchStatesResponse_instancePatchStates = Lens.lens (\DescribeInstancePatchStatesResponse' {instancePatchStates} -> instancePatchStates) (\s@DescribeInstancePatchStatesResponse' {} a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchStatesResponse_nextToken :: Lens.Lens' DescribeInstancePatchStatesResponse (Prelude.Maybe Prelude.Text)
describeInstancePatchStatesResponse_nextToken = Lens.lens (\DescribeInstancePatchStatesResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesResponse)

-- | The response's http status code.
describeInstancePatchStatesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchStatesResponse Prelude.Int
describeInstancePatchStatesResponse_httpStatus = Lens.lens (\DescribeInstancePatchStatesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchStatesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchStatesResponse)

instance
  Prelude.NFData
    DescribeInstancePatchStatesResponse
  where
  rnf DescribeInstancePatchStatesResponse' {..} =
    Prelude.rnf instancePatchStates `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
