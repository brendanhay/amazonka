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
-- Module      : Amazonka.CloudWatchLogs.DescribeResourcePolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource policies in this account.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.DescribeResourcePolicies
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
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePoliciesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "resourcePolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicies where
  hashWithSalt _salt DescribeResourcePolicies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit

instance Prelude.NFData DescribeResourcePolicies where
  rnf DescribeResourcePolicies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit

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
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource policies that exist in this account.
    resourcePolicies :: Prelude.Maybe [ResourcePolicy],
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
-- 'nextToken', 'describeResourcePoliciesResponse_nextToken' - Undocumented member.
--
-- 'resourcePolicies', 'describeResourcePoliciesResponse_resourcePolicies' - The resource policies that exist in this account.
--
-- 'httpStatus', 'describeResourcePoliciesResponse_httpStatus' - The response's http status code.
newDescribeResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePoliciesResponse
newDescribeResourcePoliciesResponse pHttpStatus_ =
  DescribeResourcePoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      resourcePolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeResourcePoliciesResponse_nextToken :: Lens.Lens' DescribeResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
describeResourcePoliciesResponse_nextToken = Lens.lens (\DescribeResourcePoliciesResponse' {nextToken} -> nextToken) (\s@DescribeResourcePoliciesResponse' {} a -> s {nextToken = a} :: DescribeResourcePoliciesResponse)

-- | The resource policies that exist in this account.
describeResourcePoliciesResponse_resourcePolicies :: Lens.Lens' DescribeResourcePoliciesResponse (Prelude.Maybe [ResourcePolicy])
describeResourcePoliciesResponse_resourcePolicies = Lens.lens (\DescribeResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@DescribeResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: DescribeResourcePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeResourcePoliciesResponse_httpStatus :: Lens.Lens' DescribeResourcePoliciesResponse Prelude.Int
describeResourcePoliciesResponse_httpStatus = Lens.lens (\DescribeResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePoliciesResponse' {} a -> s {httpStatus = a} :: DescribeResourcePoliciesResponse)

instance
  Prelude.NFData
    DescribeResourcePoliciesResponse
  where
  rnf DescribeResourcePoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourcePolicies
      `Prelude.seq` Prelude.rnf httpStatus
