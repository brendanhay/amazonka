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
-- Module      : Network.AWS.DirectoryService.DescribeRegions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the Regions that are configured for
-- multi-Region replication.
module Network.AWS.DirectoryService.DescribeRegions
  ( -- * Creating a Request
    DescribeRegions (..),
    newDescribeRegions,

    -- * Request Lenses
    describeRegions_regionName,
    describeRegions_nextToken,
    describeRegions_directoryId,

    -- * Destructuring the Response
    DescribeRegionsResponse (..),
    newDescribeRegionsResponse,

    -- * Response Lenses
    describeRegionsResponse_nextToken,
    describeRegionsResponse_regionsDescription,
    describeRegionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | The name of the Region. For example, @us-east-1@.
    regionName :: Core.Maybe Core.Text,
    -- | The @DescribeRegionsResult.NextToken@ value from a previous call to
    -- DescribeRegions. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifier of the directory.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'describeRegions_regionName' - The name of the Region. For example, @us-east-1@.
--
-- 'nextToken', 'describeRegions_nextToken' - The @DescribeRegionsResult.NextToken@ value from a previous call to
-- DescribeRegions. Pass null if this is the first call.
--
-- 'directoryId', 'describeRegions_directoryId' - The identifier of the directory.
newDescribeRegions ::
  -- | 'directoryId'
  Core.Text ->
  DescribeRegions
newDescribeRegions pDirectoryId_ =
  DescribeRegions'
    { regionName = Core.Nothing,
      nextToken = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The name of the Region. For example, @us-east-1@.
describeRegions_regionName :: Lens.Lens' DescribeRegions (Core.Maybe Core.Text)
describeRegions_regionName = Lens.lens (\DescribeRegions' {regionName} -> regionName) (\s@DescribeRegions' {} a -> s {regionName = a} :: DescribeRegions)

-- | The @DescribeRegionsResult.NextToken@ value from a previous call to
-- DescribeRegions. Pass null if this is the first call.
describeRegions_nextToken :: Lens.Lens' DescribeRegions (Core.Maybe Core.Text)
describeRegions_nextToken = Lens.lens (\DescribeRegions' {nextToken} -> nextToken) (\s@DescribeRegions' {} a -> s {nextToken = a} :: DescribeRegions)

-- | The identifier of the directory.
describeRegions_directoryId :: Lens.Lens' DescribeRegions Core.Text
describeRegions_directoryId = Lens.lens (\DescribeRegions' {directoryId} -> directoryId) (\s@DescribeRegions' {} a -> s {directoryId = a} :: DescribeRegions)

instance Core.AWSRequest DescribeRegions where
  type
    AWSResponse DescribeRegions =
      DescribeRegionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "RegionsDescription"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRegions

instance Core.NFData DescribeRegions

instance Core.ToHeaders DescribeRegions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeRegions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRegions where
  toJSON DescribeRegions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RegionName" Core..=) Core.<$> regionName,
            ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeRegions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRegions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter in a subsequent call to DescribeRegions to
    -- retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | List of Region information related to the directory for each replicated
    -- Region.
    regionsDescription :: Core.Maybe [RegionDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRegionsResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeRegions to
-- retrieve the next set of items.
--
-- 'regionsDescription', 'describeRegionsResponse_regionsDescription' - List of Region information related to the directory for each replicated
-- Region.
--
-- 'httpStatus', 'describeRegionsResponse_httpStatus' - The response's http status code.
newDescribeRegionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRegionsResponse
newDescribeRegionsResponse pHttpStatus_ =
  DescribeRegionsResponse'
    { nextToken = Core.Nothing,
      regionsDescription = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeRegions to
-- retrieve the next set of items.
describeRegionsResponse_nextToken :: Lens.Lens' DescribeRegionsResponse (Core.Maybe Core.Text)
describeRegionsResponse_nextToken = Lens.lens (\DescribeRegionsResponse' {nextToken} -> nextToken) (\s@DescribeRegionsResponse' {} a -> s {nextToken = a} :: DescribeRegionsResponse)

-- | List of Region information related to the directory for each replicated
-- Region.
describeRegionsResponse_regionsDescription :: Lens.Lens' DescribeRegionsResponse (Core.Maybe [RegionDescription])
describeRegionsResponse_regionsDescription = Lens.lens (\DescribeRegionsResponse' {regionsDescription} -> regionsDescription) (\s@DescribeRegionsResponse' {} a -> s {regionsDescription = a} :: DescribeRegionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRegionsResponse_httpStatus :: Lens.Lens' DescribeRegionsResponse Core.Int
describeRegionsResponse_httpStatus = Lens.lens (\DescribeRegionsResponse' {httpStatus} -> httpStatus) (\s@DescribeRegionsResponse' {} a -> s {httpStatus = a} :: DescribeRegionsResponse)

instance Core.NFData DescribeRegionsResponse
