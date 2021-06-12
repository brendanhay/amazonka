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
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourcePolicies' smart constructor.
data DescribeResourcePolicies = DescribeResourcePolicies'
  { nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of resource policies to be displayed with one call of
    -- this API.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      limit = Core.Nothing
    }

-- | Undocumented member.
describeResourcePolicies_nextToken :: Lens.Lens' DescribeResourcePolicies (Core.Maybe Core.Text)
describeResourcePolicies_nextToken = Lens.lens (\DescribeResourcePolicies' {nextToken} -> nextToken) (\s@DescribeResourcePolicies' {} a -> s {nextToken = a} :: DescribeResourcePolicies)

-- | The maximum number of resource policies to be displayed with one call of
-- this API.
describeResourcePolicies_limit :: Lens.Lens' DescribeResourcePolicies (Core.Maybe Core.Natural)
describeResourcePolicies_limit = Lens.lens (\DescribeResourcePolicies' {limit} -> limit) (\s@DescribeResourcePolicies' {} a -> s {limit = a} :: DescribeResourcePolicies)

instance Core.AWSPager DescribeResourcePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_resourcePolicies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeResourcePolicies_nextToken
          Lens..~ rs
          Lens.^? describeResourcePoliciesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeResourcePolicies where
  type
    AWSResponse DescribeResourcePolicies =
      DescribeResourcePoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePoliciesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "resourcePolicies" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeResourcePolicies

instance Core.NFData DescribeResourcePolicies

instance Core.ToHeaders DescribeResourcePolicies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeResourcePolicies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeResourcePolicies where
  toJSON DescribeResourcePolicies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeResourcePolicies where
  toPath = Core.const "/"

instance Core.ToQuery DescribeResourcePolicies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The resource policies that exist in this account.
    resourcePolicies :: Core.Maybe [ResourcePolicy],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeResourcePoliciesResponse
newDescribeResourcePoliciesResponse pHttpStatus_ =
  DescribeResourcePoliciesResponse'
    { nextToken =
        Core.Nothing,
      resourcePolicies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeResourcePoliciesResponse_nextToken :: Lens.Lens' DescribeResourcePoliciesResponse (Core.Maybe Core.Text)
describeResourcePoliciesResponse_nextToken = Lens.lens (\DescribeResourcePoliciesResponse' {nextToken} -> nextToken) (\s@DescribeResourcePoliciesResponse' {} a -> s {nextToken = a} :: DescribeResourcePoliciesResponse)

-- | The resource policies that exist in this account.
describeResourcePoliciesResponse_resourcePolicies :: Lens.Lens' DescribeResourcePoliciesResponse (Core.Maybe [ResourcePolicy])
describeResourcePoliciesResponse_resourcePolicies = Lens.lens (\DescribeResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@DescribeResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: DescribeResourcePoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeResourcePoliciesResponse_httpStatus :: Lens.Lens' DescribeResourcePoliciesResponse Core.Int
describeResourcePoliciesResponse_httpStatus = Lens.lens (\DescribeResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePoliciesResponse' {} a -> s {httpStatus = a} :: DescribeResourcePoliciesResponse)

instance Core.NFData DescribeResourcePoliciesResponse
