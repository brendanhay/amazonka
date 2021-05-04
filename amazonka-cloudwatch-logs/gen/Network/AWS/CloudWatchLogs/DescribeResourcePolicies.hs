{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager DescribeResourcePolicies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeResourcePoliciesResponse_resourcePolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeResourcePolicies_nextToken
          Lens..~ rs
          Lens.^? describeResourcePoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeResourcePolicies where
  type
    Rs DescribeResourcePolicies =
      DescribeResourcePoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePoliciesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "resourcePolicies"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicies

instance Prelude.NFData DescribeResourcePolicies

instance Prelude.ToHeaders DescribeResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.DescribeResourcePolicies" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeResourcePolicies where
  toJSON DescribeResourcePolicies' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath DescribeResourcePolicies where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource policies that exist in this account.
    resourcePolicies :: Prelude.Maybe [ResourcePolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeResourcePoliciesResponse_resourcePolicies = Lens.lens (\DescribeResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@DescribeResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: DescribeResourcePoliciesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeResourcePoliciesResponse_httpStatus :: Lens.Lens' DescribeResourcePoliciesResponse Prelude.Int
describeResourcePoliciesResponse_httpStatus = Lens.lens (\DescribeResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePoliciesResponse' {} a -> s {httpStatus = a} :: DescribeResourcePoliciesResponse)

instance
  Prelude.NFData
    DescribeResourcePoliciesResponse
