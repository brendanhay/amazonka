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
-- Module      : Network.AWS.RAM.GetResourceShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the resource shares that you own or the resource shares that are
-- shared with you.
--
-- This operation returns paginated results.
module Network.AWS.RAM.GetResourceShares
  ( -- * Creating a Request
    GetResourceShares (..),
    newGetResourceShares,

    -- * Request Lenses
    getResourceShares_tagFilters,
    getResourceShares_nextToken,
    getResourceShares_name,
    getResourceShares_resourceShareStatus,
    getResourceShares_permissionArn,
    getResourceShares_maxResults,
    getResourceShares_resourceShareArns,
    getResourceShares_resourceOwner,

    -- * Destructuring the Response
    GetResourceSharesResponse (..),
    newGetResourceSharesResponse,

    -- * Response Lenses
    getResourceSharesResponse_resourceShares,
    getResourceSharesResponse_nextToken,
    getResourceSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourceShares' smart constructor.
data GetResourceShares = GetResourceShares'
  { -- | One or more tag filters.
    tagFilters :: Prelude.Maybe [TagFilter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource share.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the resource share.
    resourceShareStatus :: Prelude.Maybe ResourceShareStatus,
    -- | The Amazon Resource Name (ARN) of the RAM permission that is associated
    -- with the resource share.
    permissionArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARNs) of the resource shares.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | The type of owner.
    resourceOwner :: ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilters', 'getResourceShares_tagFilters' - One or more tag filters.
--
-- 'nextToken', 'getResourceShares_nextToken' - The token for the next page of results.
--
-- 'name', 'getResourceShares_name' - The name of the resource share.
--
-- 'resourceShareStatus', 'getResourceShares_resourceShareStatus' - The status of the resource share.
--
-- 'permissionArn', 'getResourceShares_permissionArn' - The Amazon Resource Name (ARN) of the RAM permission that is associated
-- with the resource share.
--
-- 'maxResults', 'getResourceShares_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareArns', 'getResourceShares_resourceShareArns' - The Amazon Resource Names (ARNs) of the resource shares.
--
-- 'resourceOwner', 'getResourceShares_resourceOwner' - The type of owner.
newGetResourceShares ::
  -- | 'resourceOwner'
  ResourceOwner ->
  GetResourceShares
newGetResourceShares pResourceOwner_ =
  GetResourceShares'
    { tagFilters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceShareStatus = Prelude.Nothing,
      permissionArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      resourceOwner = pResourceOwner_
    }

-- | One or more tag filters.
getResourceShares_tagFilters :: Lens.Lens' GetResourceShares (Prelude.Maybe [TagFilter])
getResourceShares_tagFilters = Lens.lens (\GetResourceShares' {tagFilters} -> tagFilters) (\s@GetResourceShares' {} a -> s {tagFilters = a} :: GetResourceShares) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getResourceShares_nextToken :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_nextToken = Lens.lens (\GetResourceShares' {nextToken} -> nextToken) (\s@GetResourceShares' {} a -> s {nextToken = a} :: GetResourceShares)

-- | The name of the resource share.
getResourceShares_name :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_name = Lens.lens (\GetResourceShares' {name} -> name) (\s@GetResourceShares' {} a -> s {name = a} :: GetResourceShares)

-- | The status of the resource share.
getResourceShares_resourceShareStatus :: Lens.Lens' GetResourceShares (Prelude.Maybe ResourceShareStatus)
getResourceShares_resourceShareStatus = Lens.lens (\GetResourceShares' {resourceShareStatus} -> resourceShareStatus) (\s@GetResourceShares' {} a -> s {resourceShareStatus = a} :: GetResourceShares)

-- | The Amazon Resource Name (ARN) of the RAM permission that is associated
-- with the resource share.
getResourceShares_permissionArn :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_permissionArn = Lens.lens (\GetResourceShares' {permissionArn} -> permissionArn) (\s@GetResourceShares' {} a -> s {permissionArn = a} :: GetResourceShares)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getResourceShares_maxResults :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Natural)
getResourceShares_maxResults = Lens.lens (\GetResourceShares' {maxResults} -> maxResults) (\s@GetResourceShares' {} a -> s {maxResults = a} :: GetResourceShares)

-- | The Amazon Resource Names (ARNs) of the resource shares.
getResourceShares_resourceShareArns :: Lens.Lens' GetResourceShares (Prelude.Maybe [Prelude.Text])
getResourceShares_resourceShareArns = Lens.lens (\GetResourceShares' {resourceShareArns} -> resourceShareArns) (\s@GetResourceShares' {} a -> s {resourceShareArns = a} :: GetResourceShares) Prelude.. Lens.mapping Lens.coerced

-- | The type of owner.
getResourceShares_resourceOwner :: Lens.Lens' GetResourceShares ResourceOwner
getResourceShares_resourceOwner = Lens.lens (\GetResourceShares' {resourceOwner} -> resourceOwner) (\s@GetResourceShares' {} a -> s {resourceOwner = a} :: GetResourceShares)

instance Core.AWSPager GetResourceShares where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceSharesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceSharesResponse_resourceShares
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourceShares_nextToken
          Lens..~ rs
          Lens.^? getResourceSharesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourceShares where
  type
    AWSResponse GetResourceShares =
      GetResourceSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceSharesResponse'
            Prelude.<$> (x Core..?> "resourceShares" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceShares

instance Prelude.NFData GetResourceShares

instance Core.ToHeaders GetResourceShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourceShares where
  toJSON GetResourceShares' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tagFilters" Core..=) Prelude.<$> tagFilters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("name" Core..=) Prelude.<$> name,
            ("resourceShareStatus" Core..=)
              Prelude.<$> resourceShareStatus,
            ("permissionArn" Core..=) Prelude.<$> permissionArn,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("resourceShareArns" Core..=)
              Prelude.<$> resourceShareArns,
            Prelude.Just
              ("resourceOwner" Core..= resourceOwner)
          ]
      )

instance Core.ToPath GetResourceShares where
  toPath = Prelude.const "/getresourceshares"

instance Core.ToQuery GetResourceShares where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceSharesResponse' smart constructor.
data GetResourceSharesResponse = GetResourceSharesResponse'
  { -- | Information about the resource shares.
    resourceShares :: Prelude.Maybe [ResourceShare],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceShares', 'getResourceSharesResponse_resourceShares' - Information about the resource shares.
--
-- 'nextToken', 'getResourceSharesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getResourceSharesResponse_httpStatus' - The response's http status code.
newGetResourceSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceSharesResponse
newGetResourceSharesResponse pHttpStatus_ =
  GetResourceSharesResponse'
    { resourceShares =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resource shares.
getResourceSharesResponse_resourceShares :: Lens.Lens' GetResourceSharesResponse (Prelude.Maybe [ResourceShare])
getResourceSharesResponse_resourceShares = Lens.lens (\GetResourceSharesResponse' {resourceShares} -> resourceShares) (\s@GetResourceSharesResponse' {} a -> s {resourceShares = a} :: GetResourceSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getResourceSharesResponse_nextToken :: Lens.Lens' GetResourceSharesResponse (Prelude.Maybe Prelude.Text)
getResourceSharesResponse_nextToken = Lens.lens (\GetResourceSharesResponse' {nextToken} -> nextToken) (\s@GetResourceSharesResponse' {} a -> s {nextToken = a} :: GetResourceSharesResponse)

-- | The response's http status code.
getResourceSharesResponse_httpStatus :: Lens.Lens' GetResourceSharesResponse Prelude.Int
getResourceSharesResponse_httpStatus = Lens.lens (\GetResourceSharesResponse' {httpStatus} -> httpStatus) (\s@GetResourceSharesResponse' {} a -> s {httpStatus = a} :: GetResourceSharesResponse)

instance Prelude.NFData GetResourceSharesResponse
