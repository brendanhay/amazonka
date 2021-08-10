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
-- Module      : Network.AWS.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more instances.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStates
  ( -- * Creating a Request
    DescribeInstancePatchStates (..),
    newDescribeInstancePatchStates,

    -- * Request Lenses
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_instanceIds,

    -- * Destructuring the Response
    DescribeInstancePatchStatesResponse (..),
    newDescribeInstancePatchStatesResponse,

    -- * Response Lenses
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the instance whose patch state information should be
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
-- 'nextToken', 'describeInstancePatchStates_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstancePatchStates_maxResults' - The maximum number of instances to return (per page).
--
-- 'instanceIds', 'describeInstancePatchStates_instanceIds' - The ID of the instance whose patch state information should be
-- retrieved.
newDescribeInstancePatchStates ::
  DescribeInstancePatchStates
newDescribeInstancePatchStates =
  DescribeInstancePatchStates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatchStates_nextToken :: Lens.Lens' DescribeInstancePatchStates (Prelude.Maybe Prelude.Text)
describeInstancePatchStates_nextToken = Lens.lens (\DescribeInstancePatchStates' {nextToken} -> nextToken) (\s@DescribeInstancePatchStates' {} a -> s {nextToken = a} :: DescribeInstancePatchStates)

-- | The maximum number of instances to return (per page).
describeInstancePatchStates_maxResults :: Lens.Lens' DescribeInstancePatchStates (Prelude.Maybe Prelude.Natural)
describeInstancePatchStates_maxResults = Lens.lens (\DescribeInstancePatchStates' {maxResults} -> maxResults) (\s@DescribeInstancePatchStates' {} a -> s {maxResults = a} :: DescribeInstancePatchStates)

-- | The ID of the instance whose patch state information should be
-- retrieved.
describeInstancePatchStates_instanceIds :: Lens.Lens' DescribeInstancePatchStates [Prelude.Text]
describeInstancePatchStates_instanceIds = Lens.lens (\DescribeInstancePatchStates' {instanceIds} -> instanceIds) (\s@DescribeInstancePatchStates' {} a -> s {instanceIds = a} :: DescribeInstancePatchStates) Prelude.. Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "InstancePatchStates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstancePatchStates

instance Prelude.NFData DescribeInstancePatchStates

instance Core.ToHeaders DescribeInstancePatchStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInstancePatchStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeInstancePatchStates where
  toJSON DescribeInstancePatchStates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceIds" Core..= instanceIds)
          ]
      )

instance Core.ToPath DescribeInstancePatchStates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstancePatchStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The high-level patch state for the requested instances.
    instancePatchStates :: Prelude.Maybe [InstancePatchState],
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
-- 'nextToken', 'describeInstancePatchStatesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instancePatchStates', 'describeInstancePatchStatesResponse_instancePatchStates' - The high-level patch state for the requested instances.
--
-- 'httpStatus', 'describeInstancePatchStatesResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancePatchStatesResponse
newDescribeInstancePatchStatesResponse pHttpStatus_ =
  DescribeInstancePatchStatesResponse'
    { nextToken =
        Prelude.Nothing,
      instancePatchStates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchStatesResponse_nextToken :: Lens.Lens' DescribeInstancePatchStatesResponse (Prelude.Maybe Prelude.Text)
describeInstancePatchStatesResponse_nextToken = Lens.lens (\DescribeInstancePatchStatesResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesResponse)

-- | The high-level patch state for the requested instances.
describeInstancePatchStatesResponse_instancePatchStates :: Lens.Lens' DescribeInstancePatchStatesResponse (Prelude.Maybe [InstancePatchState])
describeInstancePatchStatesResponse_instancePatchStates = Lens.lens (\DescribeInstancePatchStatesResponse' {instancePatchStates} -> instancePatchStates) (\s@DescribeInstancePatchStatesResponse' {} a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstancePatchStatesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchStatesResponse Prelude.Int
describeInstancePatchStatesResponse_httpStatus = Lens.lens (\DescribeInstancePatchStatesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchStatesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchStatesResponse)

instance
  Prelude.NFData
    DescribeInstancePatchStatesResponse
