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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the available WorkSpace bundles.
--
-- You can filter the results using either bundle ID or owner, but not
-- both.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
  ( -- * Creating a Request
    DescribeWorkspaceBundles (..),
    newDescribeWorkspaceBundles,

    -- * Request Lenses
    describeWorkspaceBundles_nextToken,
    describeWorkspaceBundles_owner,
    describeWorkspaceBundles_bundleIds,

    -- * Destructuring the Response
    DescribeWorkspaceBundlesResponse (..),
    newDescribeWorkspaceBundlesResponse,

    -- * Response Lenses
    describeWorkspaceBundlesResponse_nextToken,
    describeWorkspaceBundlesResponse_bundles,
    describeWorkspaceBundlesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceBundles' smart constructor.
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
  { -- | The token for the next set of results. (You received this token from a
    -- previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The owner of the bundles. You cannot combine this parameter with any
    -- other filter.
    --
    -- Specify @AMAZON@ to describe the bundles provided by AWS or null to
    -- describe the bundles that belong to your account.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the bundles. You cannot combine this parameter with
    -- any other filter.
    bundleIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceBundles_nextToken' - The token for the next set of results. (You received this token from a
-- previous call.)
--
-- 'owner', 'describeWorkspaceBundles_owner' - The owner of the bundles. You cannot combine this parameter with any
-- other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to
-- describe the bundles that belong to your account.
--
-- 'bundleIds', 'describeWorkspaceBundles_bundleIds' - The identifiers of the bundles. You cannot combine this parameter with
-- any other filter.
newDescribeWorkspaceBundles ::
  DescribeWorkspaceBundles
newDescribeWorkspaceBundles =
  DescribeWorkspaceBundles'
    { nextToken =
        Prelude.Nothing,
      owner = Prelude.Nothing,
      bundleIds = Prelude.Nothing
    }

-- | The token for the next set of results. (You received this token from a
-- previous call.)
describeWorkspaceBundles_nextToken :: Lens.Lens' DescribeWorkspaceBundles (Prelude.Maybe Prelude.Text)
describeWorkspaceBundles_nextToken = Lens.lens (\DescribeWorkspaceBundles' {nextToken} -> nextToken) (\s@DescribeWorkspaceBundles' {} a -> s {nextToken = a} :: DescribeWorkspaceBundles)

-- | The owner of the bundles. You cannot combine this parameter with any
-- other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to
-- describe the bundles that belong to your account.
describeWorkspaceBundles_owner :: Lens.Lens' DescribeWorkspaceBundles (Prelude.Maybe Prelude.Text)
describeWorkspaceBundles_owner = Lens.lens (\DescribeWorkspaceBundles' {owner} -> owner) (\s@DescribeWorkspaceBundles' {} a -> s {owner = a} :: DescribeWorkspaceBundles)

-- | The identifiers of the bundles. You cannot combine this parameter with
-- any other filter.
describeWorkspaceBundles_bundleIds :: Lens.Lens' DescribeWorkspaceBundles (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspaceBundles_bundleIds = Lens.lens (\DescribeWorkspaceBundles' {bundleIds} -> bundleIds) (\s@DescribeWorkspaceBundles' {} a -> s {bundleIds = a} :: DescribeWorkspaceBundles) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeWorkspaceBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceBundlesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceBundlesResponse_bundles
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeWorkspaceBundles_nextToken
          Lens..~ rs
          Lens.^? describeWorkspaceBundlesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeWorkspaceBundles where
  type
    AWSResponse DescribeWorkspaceBundles =
      DescribeWorkspaceBundlesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceBundlesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkspaceBundles

instance Prelude.NFData DescribeWorkspaceBundles

instance Core.ToHeaders DescribeWorkspaceBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkspaceBundles where
  toJSON DescribeWorkspaceBundles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Owner" Core..=) Prelude.<$> owner,
            ("BundleIds" Core..=) Prelude.<$> bundleIds
          ]
      )

instance Core.ToPath DescribeWorkspaceBundles where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkspaceBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceBundlesResponse' smart constructor.
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
  { -- | The token to use to retrieve the next set of results, or null if there
    -- are no more results available. This token is valid for one day and must
    -- be used within that time frame.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the bundles.
    bundles :: Prelude.Maybe [WorkspaceBundle],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceBundlesResponse_nextToken' - The token to use to retrieve the next set of results, or null if there
-- are no more results available. This token is valid for one day and must
-- be used within that time frame.
--
-- 'bundles', 'describeWorkspaceBundlesResponse_bundles' - Information about the bundles.
--
-- 'httpStatus', 'describeWorkspaceBundlesResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspaceBundlesResponse
newDescribeWorkspaceBundlesResponse pHttpStatus_ =
  DescribeWorkspaceBundlesResponse'
    { nextToken =
        Prelude.Nothing,
      bundles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if there
-- are no more results available. This token is valid for one day and must
-- be used within that time frame.
describeWorkspaceBundlesResponse_nextToken :: Lens.Lens' DescribeWorkspaceBundlesResponse (Prelude.Maybe Prelude.Text)
describeWorkspaceBundlesResponse_nextToken = Lens.lens (\DescribeWorkspaceBundlesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceBundlesResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceBundlesResponse)

-- | Information about the bundles.
describeWorkspaceBundlesResponse_bundles :: Lens.Lens' DescribeWorkspaceBundlesResponse (Prelude.Maybe [WorkspaceBundle])
describeWorkspaceBundlesResponse_bundles = Lens.lens (\DescribeWorkspaceBundlesResponse' {bundles} -> bundles) (\s@DescribeWorkspaceBundlesResponse' {} a -> s {bundles = a} :: DescribeWorkspaceBundlesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspaceBundlesResponse_httpStatus :: Lens.Lens' DescribeWorkspaceBundlesResponse Prelude.Int
describeWorkspaceBundlesResponse_httpStatus = Lens.lens (\DescribeWorkspaceBundlesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceBundlesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceBundlesResponse)

instance
  Prelude.NFData
    DescribeWorkspaceBundlesResponse
