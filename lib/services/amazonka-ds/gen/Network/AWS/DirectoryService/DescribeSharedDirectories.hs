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
-- Module      : Network.AWS.DirectoryService.DescribeSharedDirectories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the shared directories in your account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSharedDirectories
  ( -- * Creating a Request
    DescribeSharedDirectories (..),
    newDescribeSharedDirectories,

    -- * Request Lenses
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_nextToken,
    describeSharedDirectories_limit,
    describeSharedDirectories_ownerDirectoryId,

    -- * Destructuring the Response
    DescribeSharedDirectoriesResponse (..),
    newDescribeSharedDirectoriesResponse,

    -- * Response Lenses
    describeSharedDirectoriesResponse_sharedDirectories,
    describeSharedDirectoriesResponse_nextToken,
    describeSharedDirectoriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { -- | A list of identifiers of all shared directories in your account.
    sharedDirectoryIds :: Prelude.Maybe [Prelude.Text],
    -- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
    -- call to DescribeSharedDirectories. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of shared directories to return in the response object.
    limit :: Prelude.Maybe Prelude.Natural,
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
-- 'sharedDirectoryIds', 'describeSharedDirectories_sharedDirectoryIds' - A list of identifiers of all shared directories in your account.
--
-- 'nextToken', 'describeSharedDirectories_nextToken' - The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
--
-- 'limit', 'describeSharedDirectories_limit' - The number of shared directories to return in the response object.
--
-- 'ownerDirectoryId', 'describeSharedDirectories_ownerDirectoryId' - Returns the identifier of the directory in the directory owner account.
newDescribeSharedDirectories ::
  -- | 'ownerDirectoryId'
  Prelude.Text ->
  DescribeSharedDirectories
newDescribeSharedDirectories pOwnerDirectoryId_ =
  DescribeSharedDirectories'
    { sharedDirectoryIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      ownerDirectoryId = pOwnerDirectoryId_
    }

-- | A list of identifiers of all shared directories in your account.
describeSharedDirectories_sharedDirectoryIds :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe [Prelude.Text])
describeSharedDirectories_sharedDirectoryIds = Lens.lens (\DescribeSharedDirectories' {sharedDirectoryIds} -> sharedDirectoryIds) (\s@DescribeSharedDirectories' {} a -> s {sharedDirectoryIds = a} :: DescribeSharedDirectories) Prelude.. Lens.mapping Lens.coerced

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
describeSharedDirectories_nextToken :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe Prelude.Text)
describeSharedDirectories_nextToken = Lens.lens (\DescribeSharedDirectories' {nextToken} -> nextToken) (\s@DescribeSharedDirectories' {} a -> s {nextToken = a} :: DescribeSharedDirectories)

-- | The number of shared directories to return in the response object.
describeSharedDirectories_limit :: Lens.Lens' DescribeSharedDirectories (Prelude.Maybe Prelude.Natural)
describeSharedDirectories_limit = Lens.lens (\DescribeSharedDirectories' {limit} -> limit) (\s@DescribeSharedDirectories' {} a -> s {limit = a} :: DescribeSharedDirectories)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSharedDirectoriesResponse'
            Prelude.<$> ( x Core..?> "SharedDirectories"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSharedDirectories

instance Prelude.NFData DescribeSharedDirectories

instance Core.ToHeaders DescribeSharedDirectories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeSharedDirectories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSharedDirectories where
  toJSON DescribeSharedDirectories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SharedDirectoryIds" Core..=)
              Prelude.<$> sharedDirectoryIds,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("OwnerDirectoryId" Core..= ownerDirectoryId)
          ]
      )

instance Core.ToPath DescribeSharedDirectories where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSharedDirectories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { -- | A list of all shared directories in your account.
    sharedDirectories :: Prelude.Maybe [SharedDirectory],
    -- | If not null, token that indicates that more results are available. Pass
    -- this value for the @NextToken@ parameter in a subsequent call to
    -- DescribeSharedDirectories to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'sharedDirectories', 'describeSharedDirectoriesResponse_sharedDirectories' - A list of all shared directories in your account.
--
-- 'nextToken', 'describeSharedDirectoriesResponse_nextToken' - If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- DescribeSharedDirectories to retrieve the next set of items.
--
-- 'httpStatus', 'describeSharedDirectoriesResponse_httpStatus' - The response's http status code.
newDescribeSharedDirectoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSharedDirectoriesResponse
newDescribeSharedDirectoriesResponse pHttpStatus_ =
  DescribeSharedDirectoriesResponse'
    { sharedDirectories =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all shared directories in your account.
describeSharedDirectoriesResponse_sharedDirectories :: Lens.Lens' DescribeSharedDirectoriesResponse (Prelude.Maybe [SharedDirectory])
describeSharedDirectoriesResponse_sharedDirectories = Lens.lens (\DescribeSharedDirectoriesResponse' {sharedDirectories} -> sharedDirectories) (\s@DescribeSharedDirectoriesResponse' {} a -> s {sharedDirectories = a} :: DescribeSharedDirectoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- DescribeSharedDirectories to retrieve the next set of items.
describeSharedDirectoriesResponse_nextToken :: Lens.Lens' DescribeSharedDirectoriesResponse (Prelude.Maybe Prelude.Text)
describeSharedDirectoriesResponse_nextToken = Lens.lens (\DescribeSharedDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeSharedDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeSharedDirectoriesResponse)

-- | The response's http status code.
describeSharedDirectoriesResponse_httpStatus :: Lens.Lens' DescribeSharedDirectoriesResponse Prelude.Int
describeSharedDirectoriesResponse_httpStatus = Lens.lens (\DescribeSharedDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeSharedDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeSharedDirectoriesResponse)

instance
  Prelude.NFData
    DescribeSharedDirectoriesResponse
