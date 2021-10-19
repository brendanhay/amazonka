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
-- Module      : Network.AWS.CloudWatchLogs.DescribeResourcePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource policies in this account.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeResourcePolicies
  ( -- * Creating a Request
    DescribeResourcePolicies (..),
    newDescribeResourcePolicies,

    -- * Request Lenses
    describeResourcePolicies_nextToken,
    describeResourcePolicies_limit,

    -- * Destructuring the Response
    DescribeResourcePoliciesResponse (..),
    newDescribeResourcePoliciesResponse,

    -- * Response Lenses
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourcePolicies' smart constructor.
data DescribeResourcePolicies = DescribeResourcePolicies'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resource policies to be displayed with one call of
    -- this API.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeResourcePolicies_nextToken' - Undocumented member.
--
-- 'limit', 'describeResourcePolicies_limit' - The maximum number of resource policies to be displayed with one call of
-- this API.
newDescribeResourcePolicies ::
  DescribeResourcePolicies
newDescribeResourcePolicies =
  DescribeResourcePolicies'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Undocumented member.
describeResourcePolicies_nextToken :: Lens.Lens' DescribeResourcePolicies (Prelude.Maybe Prelude.Text)
describeResourcePolicies_nextToken = Lens.lens (\DescribeResourcePolicies' {nextToken} -> nextToken) (\s@DescribeResourcePolicies' {} a -> s {nextToken = a} :: DescribeResourcePolicies)

-- | The maximum number of resource policies to be displayed with one call of
-- this API.
describeResourcePolicies_limit :: Lens.Lens' DescribeResourcePolicies (Prelude.Maybe Prelude.Natural)
describeResourcePolicies_limit = Lens.lens (\DescribeResourcePolicies' {limit} -> limit) (\s@DescribeResourcePolicies' {} a -> s {limit = a} :: DescribeResourcePolicies)

instance Core.AWSPager DescribeResourcePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_resourcePolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeResourcePolicies_nextToken
          Lens..~ rs
          Lens.^? describeResourcePoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeResourcePolicies where
  type
    AWSResponse DescribeResourcePolicies =
      DescribeResourcePoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePoliciesResponse'
            Prelude.<$> ( x Core..?> "resourcePolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicies

instance Prelude.NFData DescribeResourcePolicies

instance Core.ToHeaders DescribeResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeResourcePolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeResourcePolicies where
  toJSON DescribeResourcePolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeResourcePolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { -- | The resource policies that exist in this account.
    resourcePolicies :: Prelude.Maybe [ResourcePolicy],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePolicies', 'describeResourcePoliciesResponse_resourcePolicies' - The resource policies that exist in this account.
--
-- 'nextToken', 'describeResourcePoliciesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeResourcePoliciesResponse_httpStatus' - The response's http status code.
newDescribeResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePoliciesResponse
newDescribeResourcePoliciesResponse pHttpStatus_ =
  DescribeResourcePoliciesResponse'
    { resourcePolicies =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policies that exist in this account.
describeResourcePoliciesResponse_resourcePolicies :: Lens.Lens' DescribeResourcePoliciesResponse (Prelude.Maybe [ResourcePolicy])
describeResourcePoliciesResponse_resourcePolicies = Lens.lens (\DescribeResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@DescribeResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: DescribeResourcePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeResourcePoliciesResponse_nextToken :: Lens.Lens' DescribeResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
describeResourcePoliciesResponse_nextToken = Lens.lens (\DescribeResourcePoliciesResponse' {nextToken} -> nextToken) (\s@DescribeResourcePoliciesResponse' {} a -> s {nextToken = a} :: DescribeResourcePoliciesResponse)

-- | The response's http status code.
describeResourcePoliciesResponse_httpStatus :: Lens.Lens' DescribeResourcePoliciesResponse Prelude.Int
describeResourcePoliciesResponse_httpStatus = Lens.lens (\DescribeResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePoliciesResponse' {} a -> s {httpStatus = a} :: DescribeResourcePoliciesResponse)

instance
  Prelude.NFData
    DescribeResourcePoliciesResponse
