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
-- Module      : Amazonka.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.AppStream.DescribeImageBuilders
  ( -- * Creating a Request
    DescribeImageBuilders (..),
    newDescribeImageBuilders,

    -- * Request Lenses
    describeImageBuilders_nextToken,
    describeImageBuilders_names,
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of the image builders to describe.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageBuilders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageBuilders_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'names', 'describeImageBuilders_names' - The names of the image builders to describe.
--
-- 'maxResults', 'describeImageBuilders_maxResults' - The maximum size of each page of results.
newDescribeImageBuilders ::
  DescribeImageBuilders
newDescribeImageBuilders =
  DescribeImageBuilders'
    { nextToken = Prelude.Nothing,
      names = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImageBuilders_nextToken :: Lens.Lens' DescribeImageBuilders (Prelude.Maybe Prelude.Text)
describeImageBuilders_nextToken = Lens.lens (\DescribeImageBuilders' {nextToken} -> nextToken) (\s@DescribeImageBuilders' {} a -> s {nextToken = a} :: DescribeImageBuilders)

-- | The names of the image builders to describe.
describeImageBuilders_names :: Lens.Lens' DescribeImageBuilders (Prelude.Maybe [Prelude.Text])
describeImageBuilders_names = Lens.lens (\DescribeImageBuilders' {names} -> names) (\s@DescribeImageBuilders' {} a -> s {names = a} :: DescribeImageBuilders) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of each page of results.
describeImageBuilders_maxResults :: Lens.Lens' DescribeImageBuilders (Prelude.Maybe Prelude.Int)
describeImageBuilders_maxResults = Lens.lens (\DescribeImageBuilders' {maxResults} -> maxResults) (\s@DescribeImageBuilders' {} a -> s {maxResults = a} :: DescribeImageBuilders)

instance Core.AWSPager DescribeImageBuilders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImageBuildersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageBuildersResponse_imageBuilders
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImageBuilders_nextToken
          Lens..~ rs
          Lens.^? describeImageBuildersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeImageBuilders where
  type
    AWSResponse DescribeImageBuilders =
      DescribeImageBuildersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageBuildersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ImageBuilders" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageBuilders where
  hashWithSalt _salt DescribeImageBuilders' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeImageBuilders where
  rnf DescribeImageBuilders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeImageBuilders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeImageBuilders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImageBuilders where
  toJSON DescribeImageBuilders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Names" Core..=) Prelude.<$> names,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeImageBuilders where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImageBuilders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the image builders.
    imageBuilders :: Prelude.Maybe [ImageBuilder],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeImageBuildersResponse
newDescribeImageBuildersResponse pHttpStatus_ =
  DescribeImageBuildersResponse'
    { nextToken =
        Prelude.Nothing,
      imageBuilders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImageBuildersResponse_nextToken :: Lens.Lens' DescribeImageBuildersResponse (Prelude.Maybe Prelude.Text)
describeImageBuildersResponse_nextToken = Lens.lens (\DescribeImageBuildersResponse' {nextToken} -> nextToken) (\s@DescribeImageBuildersResponse' {} a -> s {nextToken = a} :: DescribeImageBuildersResponse)

-- | Information about the image builders.
describeImageBuildersResponse_imageBuilders :: Lens.Lens' DescribeImageBuildersResponse (Prelude.Maybe [ImageBuilder])
describeImageBuildersResponse_imageBuilders = Lens.lens (\DescribeImageBuildersResponse' {imageBuilders} -> imageBuilders) (\s@DescribeImageBuildersResponse' {} a -> s {imageBuilders = a} :: DescribeImageBuildersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImageBuildersResponse_httpStatus :: Lens.Lens' DescribeImageBuildersResponse Prelude.Int
describeImageBuildersResponse_httpStatus = Lens.lens (\DescribeImageBuildersResponse' {httpStatus} -> httpStatus) (\s@DescribeImageBuildersResponse' {} a -> s {httpStatus = a} :: DescribeImageBuildersResponse)

instance Prelude.NFData DescribeImageBuildersResponse where
  rnf DescribeImageBuildersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageBuilders
      `Prelude.seq` Prelude.rnf httpStatus
