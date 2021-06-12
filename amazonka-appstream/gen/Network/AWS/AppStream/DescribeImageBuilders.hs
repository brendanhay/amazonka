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
-- Module      : Network.AWS.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified image builders, if
-- the image builder names are provided. Otherwise, all image builders in
-- the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImageBuilders
  ( -- * Creating a Request
    DescribeImageBuilders (..),
    newDescribeImageBuilders,

    -- * Request Lenses
    describeImageBuilders_names,
    describeImageBuilders_nextToken,
    describeImageBuilders_maxResults,

    -- * Destructuring the Response
    DescribeImageBuildersResponse (..),
    newDescribeImageBuildersResponse,

    -- * Response Lenses
    describeImageBuildersResponse_nextToken,
    describeImageBuildersResponse_imageBuilders,
    describeImageBuildersResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { -- | The names of the image builders to describe.
    names :: Core.Maybe [Core.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImageBuilders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'describeImageBuilders_names' - The names of the image builders to describe.
--
-- 'nextToken', 'describeImageBuilders_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'describeImageBuilders_maxResults' - The maximum size of each page of results.
newDescribeImageBuilders ::
  DescribeImageBuilders
newDescribeImageBuilders =
  DescribeImageBuilders'
    { names = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The names of the image builders to describe.
describeImageBuilders_names :: Lens.Lens' DescribeImageBuilders (Core.Maybe [Core.Text])
describeImageBuilders_names = Lens.lens (\DescribeImageBuilders' {names} -> names) (\s@DescribeImageBuilders' {} a -> s {names = a} :: DescribeImageBuilders) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImageBuilders_nextToken :: Lens.Lens' DescribeImageBuilders (Core.Maybe Core.Text)
describeImageBuilders_nextToken = Lens.lens (\DescribeImageBuilders' {nextToken} -> nextToken) (\s@DescribeImageBuilders' {} a -> s {nextToken = a} :: DescribeImageBuilders)

-- | The maximum size of each page of results.
describeImageBuilders_maxResults :: Lens.Lens' DescribeImageBuilders (Core.Maybe Core.Int)
describeImageBuilders_maxResults = Lens.lens (\DescribeImageBuilders' {maxResults} -> maxResults) (\s@DescribeImageBuilders' {} a -> s {maxResults = a} :: DescribeImageBuilders)

instance Core.AWSPager DescribeImageBuilders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImageBuildersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageBuildersResponse_imageBuilders
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeImageBuilders_nextToken
          Lens..~ rs
          Lens.^? describeImageBuildersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeImageBuilders where
  type
    AWSResponse DescribeImageBuilders =
      DescribeImageBuildersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageBuildersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ImageBuilders" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImageBuilders

instance Core.NFData DescribeImageBuilders

instance Core.ToHeaders DescribeImageBuilders where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeImageBuilders" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImageBuilders where
  toJSON DescribeImageBuilders' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeImageBuilders where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImageBuilders where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the image builders.
    imageBuilders :: Core.Maybe [ImageBuilder],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImageBuildersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageBuildersResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'imageBuilders', 'describeImageBuildersResponse_imageBuilders' - Information about the image builders.
--
-- 'httpStatus', 'describeImageBuildersResponse_httpStatus' - The response's http status code.
newDescribeImageBuildersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImageBuildersResponse
newDescribeImageBuildersResponse pHttpStatus_ =
  DescribeImageBuildersResponse'
    { nextToken =
        Core.Nothing,
      imageBuilders = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImageBuildersResponse_nextToken :: Lens.Lens' DescribeImageBuildersResponse (Core.Maybe Core.Text)
describeImageBuildersResponse_nextToken = Lens.lens (\DescribeImageBuildersResponse' {nextToken} -> nextToken) (\s@DescribeImageBuildersResponse' {} a -> s {nextToken = a} :: DescribeImageBuildersResponse)

-- | Information about the image builders.
describeImageBuildersResponse_imageBuilders :: Lens.Lens' DescribeImageBuildersResponse (Core.Maybe [ImageBuilder])
describeImageBuildersResponse_imageBuilders = Lens.lens (\DescribeImageBuildersResponse' {imageBuilders} -> imageBuilders) (\s@DescribeImageBuildersResponse' {} a -> s {imageBuilders = a} :: DescribeImageBuildersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeImageBuildersResponse_httpStatus :: Lens.Lens' DescribeImageBuildersResponse Core.Int
describeImageBuildersResponse_httpStatus = Lens.lens (\DescribeImageBuildersResponse' {httpStatus} -> httpStatus) (\s@DescribeImageBuildersResponse' {} a -> s {httpStatus = a} :: DescribeImageBuildersResponse)

instance Core.NFData DescribeImageBuildersResponse
