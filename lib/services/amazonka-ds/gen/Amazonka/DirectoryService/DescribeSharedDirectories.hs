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
-- Module      : Amazonka.DirectoryService.DescribeSharedDirectories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the shared directories in your account.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.DescribeSharedDirectories
  ( -- * Creating a Request
    DescribeSharedDirectories (..),
    newDescribeSharedDirectories,

    -- * Request Lenses
    describeSharedDirectories_limit,
    describeSharedDirectories_nextToken,
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_ownerDirectoryId,

    -- * Destructuring the Response
    DescribeSharedDirectoriesResponse (..),
    newDescribeSharedDirectoriesResponse,

    -- * Response Lenses
    describeSharedDirectoriesResponse_nextToken,
    describeSharedDirectoriesResponse_sharedDirectories,
    describeSharedDirectoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { -- | The number of shared directories to return in the response object.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
    -- call to DescribeSharedDirectories. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers of all shared directories in your account.
    sharedDirectoryIds :: Prelude.Maybe [Prelude.Text],
    -- | Returns the identifier of the directory in the directory owner account.
    ownerDirectoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSharedDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeSharedDirectories_limit' - The number of shared directories to return in the response object.
--
-- 'nextToken', 'describeSharedDirectories_nextToken' - The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
--
-- 'sharedDirectoryIds', 'describeSharedDirectories_sharedDirectoryIds' - A list of identifiers of all shared directories in your account.
--
-- 'ownerDirectoryId', 'describeSharedDirectories_ownerDirectoryId' - Returns the identifier of the directory in the directory owner account.
newDescribeSharedDirectories ::
  -- | 'ownerDirectoryId'
  Prelude.Text ->
  DescribeSharedDirectories
newDescribeSharedDirectories pOwnerDirectoryId_ =
  DescribeSharedDirectories'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sharedDirectoryIds = Prelude.Nothing,
      ownerDirectoryId = pOwnerDirectoryId_
    }

-- | The number of shared directories to return in the response object.
describeSharedDirectories_limit :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe Prelude.Natural)
describeSharedDirectories_limit = Lens.lens (\DescribeSharedDirectories' {limit} -> limit) (\s@DescribeSharedDirectories' {} a -> s {limit = a} :: DescribeSharedDirectories)

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
describeSharedDirectories_nextToken :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe Prelude.Text)
describeSharedDirectories_nextToken = Lens.lens (\DescribeSharedDirectories' {nextToken} -> nextToken) (\s@DescribeSharedDirectories' {} a -> s {nextToken = a} :: DescribeSharedDirectories)

-- | A list of identifiers of all shared directories in your account.
describeSharedDirectories_sharedDirectoryIds :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe [Prelude.Text])
describeSharedDirectories_sharedDirectoryIds = Lens.lens (\DescribeSharedDirectories' {sharedDirectoryIds} -> sharedDirectoryIds) (\s@DescribeSharedDirectories' {} a -> s {sharedDirectoryIds = a} :: DescribeSharedDirectories) Prelude.. Lens.mapping Lens.coerced

-- | Returns the identifier of the directory in the directory owner account.
describeSharedDirectories_ownerDirectoryId :: Lens.Lens' DescribeSharedDirectories Prelude.Text
describeSharedDirectories_ownerDirectoryId = Lens.lens (\DescribeSharedDirectories' {ownerDirectoryId} -> ownerDirectoryId) (\s@DescribeSharedDirectories' {} a -> s {ownerDirectoryId = a} :: DescribeSharedDirectories)

instance Core.AWSPager DescribeSharedDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSharedDirectoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSharedDirectoriesResponse_sharedDirectories
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSharedDirectories_nextToken
          Lens..~ rs
          Lens.^? describeSharedDirectoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSharedDirectories where
  type
    AWSResponse DescribeSharedDirectories =
      DescribeSharedDirectoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSharedDirectoriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "SharedDirectories"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSharedDirectories where
  hashWithSalt _salt DescribeSharedDirectories' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sharedDirectoryIds
      `Prelude.hashWithSalt` ownerDirectoryId

instance Prelude.NFData DescribeSharedDirectories where
  rnf DescribeSharedDirectories' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sharedDirectoryIds
      `Prelude.seq` Prelude.rnf ownerDirectoryId

instance Data.ToHeaders DescribeSharedDirectories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeSharedDirectories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSharedDirectories where
  toJSON DescribeSharedDirectories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SharedDirectoryIds" Data..=)
              Prelude.<$> sharedDirectoryIds,
            Prelude.Just
              ("OwnerDirectoryId" Data..= ownerDirectoryId)
          ]
      )

instance Data.ToPath DescribeSharedDirectories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSharedDirectories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { -- | If not null, token that indicates that more results are available. Pass
    -- this value for the @NextToken@ parameter in a subsequent call to
    -- DescribeSharedDirectories to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all shared directories in your account.
    sharedDirectories :: Prelude.Maybe [SharedDirectory],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSharedDirectoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSharedDirectoriesResponse_nextToken' - If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- DescribeSharedDirectories to retrieve the next set of items.
--
-- 'sharedDirectories', 'describeSharedDirectoriesResponse_sharedDirectories' - A list of all shared directories in your account.
--
-- 'httpStatus', 'describeSharedDirectoriesResponse_httpStatus' - The response's http status code.
newDescribeSharedDirectoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSharedDirectoriesResponse
newDescribeSharedDirectoriesResponse pHttpStatus_ =
  DescribeSharedDirectoriesResponse'
    { nextToken =
        Prelude.Nothing,
      sharedDirectories = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- DescribeSharedDirectories to retrieve the next set of items.
describeSharedDirectoriesResponse_nextToken :: Lens.Lens' DescribeSharedDirectoriesResponse (Prelude.Maybe Prelude.Text)
describeSharedDirectoriesResponse_nextToken = Lens.lens (\DescribeSharedDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeSharedDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeSharedDirectoriesResponse)

-- | A list of all shared directories in your account.
describeSharedDirectoriesResponse_sharedDirectories :: Lens.Lens' DescribeSharedDirectoriesResponse (Prelude.Maybe [SharedDirectory])
describeSharedDirectoriesResponse_sharedDirectories = Lens.lens (\DescribeSharedDirectoriesResponse' {sharedDirectories} -> sharedDirectories) (\s@DescribeSharedDirectoriesResponse' {} a -> s {sharedDirectories = a} :: DescribeSharedDirectoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSharedDirectoriesResponse_httpStatus :: Lens.Lens' DescribeSharedDirectoriesResponse Prelude.Int
describeSharedDirectoriesResponse_httpStatus = Lens.lens (\DescribeSharedDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeSharedDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeSharedDirectoriesResponse)

instance
  Prelude.NFData
    DescribeSharedDirectoriesResponse
  where
  rnf DescribeSharedDirectoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sharedDirectories
      `Prelude.seq` Prelude.rnf httpStatus
