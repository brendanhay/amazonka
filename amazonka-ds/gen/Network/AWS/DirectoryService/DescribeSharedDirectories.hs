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
    describeSharedDirectories_nextToken,
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_limit,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { -- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
    -- call to DescribeSharedDirectories. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of identifiers of all shared directories in your account.
    sharedDirectoryIds :: Core.Maybe [Core.Text],
    -- | The number of shared directories to return in the response object.
    limit :: Core.Maybe Core.Natural,
    -- | Returns the identifier of the directory in the directory owner account.
    ownerDirectoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSharedDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSharedDirectories_nextToken' - The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
--
-- 'sharedDirectoryIds', 'describeSharedDirectories_sharedDirectoryIds' - A list of identifiers of all shared directories in your account.
--
-- 'limit', 'describeSharedDirectories_limit' - The number of shared directories to return in the response object.
--
-- 'ownerDirectoryId', 'describeSharedDirectories_ownerDirectoryId' - Returns the identifier of the directory in the directory owner account.
newDescribeSharedDirectories ::
  -- | 'ownerDirectoryId'
  Core.Text ->
  DescribeSharedDirectories
newDescribeSharedDirectories pOwnerDirectoryId_ =
  DescribeSharedDirectories'
    { nextToken =
        Core.Nothing,
      sharedDirectoryIds = Core.Nothing,
      limit = Core.Nothing,
      ownerDirectoryId = pOwnerDirectoryId_
    }

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous
-- call to DescribeSharedDirectories. Pass null if this is the first call.
describeSharedDirectories_nextToken :: Lens.Lens' DescribeSharedDirectories (Core.Maybe Core.Text)
describeSharedDirectories_nextToken = Lens.lens (\DescribeSharedDirectories' {nextToken} -> nextToken) (\s@DescribeSharedDirectories' {} a -> s {nextToken = a} :: DescribeSharedDirectories)

-- | A list of identifiers of all shared directories in your account.
describeSharedDirectories_sharedDirectoryIds :: Lens.Lens' DescribeSharedDirectories (Core.Maybe [Core.Text])
describeSharedDirectories_sharedDirectoryIds = Lens.lens (\DescribeSharedDirectories' {sharedDirectoryIds} -> sharedDirectoryIds) (\s@DescribeSharedDirectories' {} a -> s {sharedDirectoryIds = a} :: DescribeSharedDirectories) Core.. Lens.mapping Lens._Coerce

-- | The number of shared directories to return in the response object.
describeSharedDirectories_limit :: Lens.Lens' DescribeSharedDirectories (Core.Maybe Core.Natural)
describeSharedDirectories_limit = Lens.lens (\DescribeSharedDirectories' {limit} -> limit) (\s@DescribeSharedDirectories' {} a -> s {limit = a} :: DescribeSharedDirectories)

-- | Returns the identifier of the directory in the directory owner account.
describeSharedDirectories_ownerDirectoryId :: Lens.Lens' DescribeSharedDirectories Core.Text
describeSharedDirectories_ownerDirectoryId = Lens.lens (\DescribeSharedDirectories' {ownerDirectoryId} -> ownerDirectoryId) (\s@DescribeSharedDirectories' {} a -> s {ownerDirectoryId = a} :: DescribeSharedDirectories)

instance Core.AWSPager DescribeSharedDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSharedDirectoriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSharedDirectoriesResponse_sharedDirectories
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSharedDirectories_nextToken
          Lens..~ rs
          Lens.^? describeSharedDirectoriesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeSharedDirectories where
  type
    AWSResponse DescribeSharedDirectories =
      DescribeSharedDirectoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSharedDirectoriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "SharedDirectories" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSharedDirectories

instance Core.NFData DescribeSharedDirectories

instance Core.ToHeaders DescribeSharedDirectories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeSharedDirectories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSharedDirectories where
  toJSON DescribeSharedDirectories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SharedDirectoryIds" Core..=)
              Core.<$> sharedDirectoryIds,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ("OwnerDirectoryId" Core..= ownerDirectoryId)
          ]
      )

instance Core.ToPath DescribeSharedDirectories where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSharedDirectories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { -- | If not null, token that indicates that more results are available. Pass
    -- this value for the @NextToken@ parameter in a subsequent call to
    -- DescribeSharedDirectories to retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of all shared directories in your account.
    sharedDirectories :: Core.Maybe [SharedDirectory],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeSharedDirectoriesResponse
newDescribeSharedDirectoriesResponse pHttpStatus_ =
  DescribeSharedDirectoriesResponse'
    { nextToken =
        Core.Nothing,
      sharedDirectories = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- DescribeSharedDirectories to retrieve the next set of items.
describeSharedDirectoriesResponse_nextToken :: Lens.Lens' DescribeSharedDirectoriesResponse (Core.Maybe Core.Text)
describeSharedDirectoriesResponse_nextToken = Lens.lens (\DescribeSharedDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeSharedDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeSharedDirectoriesResponse)

-- | A list of all shared directories in your account.
describeSharedDirectoriesResponse_sharedDirectories :: Lens.Lens' DescribeSharedDirectoriesResponse (Core.Maybe [SharedDirectory])
describeSharedDirectoriesResponse_sharedDirectories = Lens.lens (\DescribeSharedDirectoriesResponse' {sharedDirectories} -> sharedDirectories) (\s@DescribeSharedDirectoriesResponse' {} a -> s {sharedDirectories = a} :: DescribeSharedDirectoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSharedDirectoriesResponse_httpStatus :: Lens.Lens' DescribeSharedDirectoriesResponse Core.Int
describeSharedDirectoriesResponse_httpStatus = Lens.lens (\DescribeSharedDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeSharedDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeSharedDirectoriesResponse)

instance
  Core.NFData
    DescribeSharedDirectoriesResponse
