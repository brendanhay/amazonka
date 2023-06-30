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
-- Module      : Amazonka.FSx.DescribeFileSystems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of specific Amazon FSx file systems, if a
-- @FileSystemIds@ value is provided for that file system. Otherwise, it
-- returns descriptions of all file systems owned by your Amazon Web
-- Services account in the Amazon Web Services Region of the endpoint that
-- you\'re calling.
--
-- When retrieving all file system descriptions, you can optionally specify
-- the @MaxResults@ parameter to limit the number of descriptions in a
-- response. If more file system descriptions remain, Amazon FSx returns a
-- @NextToken@ value in the response. In this case, send a later request
-- with the @NextToken@ request parameter set to the value of @NextToken@
-- from the last response.
--
-- This operation is used in an iterative process to retrieve a list of
-- your file system descriptions. @DescribeFileSystems@ is called first
-- without a @NextToken@value. Then the operation continues to be called
-- with the @NextToken@ parameter set to the value of the last @NextToken@
-- value until a response has no @NextToken@.
--
-- When using this operation, keep the following in mind:
--
-- -   The implementation might return fewer than @MaxResults@ file system
--     descriptions while still including a @NextToken@ value.
--
-- -   The order of file systems returned in the response of one
--     @DescribeFileSystems@ call and the order of file systems returned
--     across the responses of a multicall iteration is unspecified.
--
-- This operation returns paginated results.
module Amazonka.FSx.DescribeFileSystems
  ( -- * Creating a Request
    DescribeFileSystems (..),
    newDescribeFileSystems,

    -- * Request Lenses
    describeFileSystems_fileSystemIds,
    describeFileSystems_maxResults,
    describeFileSystems_nextToken,

    -- * Destructuring the Response
    DescribeFileSystemsResponse (..),
    newDescribeFileSystemsResponse,

    -- * Response Lenses
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_nextToken,
    describeFileSystemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for @DescribeFileSystems@ operation.
--
-- /See:/ 'newDescribeFileSystems' smart constructor.
data DescribeFileSystems = DescribeFileSystems'
  { -- | IDs of the file systems whose descriptions you want to retrieve
    -- (String).
    fileSystemIds :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of file systems to return in the response (integer). This
    -- parameter value must be greater than 0. The number of items that Amazon
    -- FSx returns is the minimum of the @MaxResults@ parameter specified in
    -- the request and the service\'s internal maximum number of items per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Opaque pagination token returned from a previous @DescribeFileSystems@
    -- operation (String). If a token present, the operation continues the list
    -- from where the returning call left off.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemIds', 'describeFileSystems_fileSystemIds' - IDs of the file systems whose descriptions you want to retrieve
-- (String).
--
-- 'maxResults', 'describeFileSystems_maxResults' - Maximum number of file systems to return in the response (integer). This
-- parameter value must be greater than 0. The number of items that Amazon
-- FSx returns is the minimum of the @MaxResults@ parameter specified in
-- the request and the service\'s internal maximum number of items per
-- page.
--
-- 'nextToken', 'describeFileSystems_nextToken' - Opaque pagination token returned from a previous @DescribeFileSystems@
-- operation (String). If a token present, the operation continues the list
-- from where the returning call left off.
newDescribeFileSystems ::
  DescribeFileSystems
newDescribeFileSystems =
  DescribeFileSystems'
    { fileSystemIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | IDs of the file systems whose descriptions you want to retrieve
-- (String).
describeFileSystems_fileSystemIds :: Lens.Lens' DescribeFileSystems (Prelude.Maybe [Prelude.Text])
describeFileSystems_fileSystemIds = Lens.lens (\DescribeFileSystems' {fileSystemIds} -> fileSystemIds) (\s@DescribeFileSystems' {} a -> s {fileSystemIds = a} :: DescribeFileSystems) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of file systems to return in the response (integer). This
-- parameter value must be greater than 0. The number of items that Amazon
-- FSx returns is the minimum of the @MaxResults@ parameter specified in
-- the request and the service\'s internal maximum number of items per
-- page.
describeFileSystems_maxResults :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Natural)
describeFileSystems_maxResults = Lens.lens (\DescribeFileSystems' {maxResults} -> maxResults) (\s@DescribeFileSystems' {} a -> s {maxResults = a} :: DescribeFileSystems)

-- | Opaque pagination token returned from a previous @DescribeFileSystems@
-- operation (String). If a token present, the operation continues the list
-- from where the returning call left off.
describeFileSystems_nextToken :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Text)
describeFileSystems_nextToken = Lens.lens (\DescribeFileSystems' {nextToken} -> nextToken) (\s@DescribeFileSystems' {} a -> s {nextToken = a} :: DescribeFileSystems)

instance Core.AWSPager DescribeFileSystems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFileSystemsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFileSystemsResponse_fileSystems
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeFileSystems_nextToken
          Lens..~ rs
          Lens.^? describeFileSystemsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeFileSystems where
  type
    AWSResponse DescribeFileSystems =
      DescribeFileSystemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFileSystemsResponse'
            Prelude.<$> (x Data..?> "FileSystems" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFileSystems where
  hashWithSalt _salt DescribeFileSystems' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFileSystems where
  rnf DescribeFileSystems' {..} =
    Prelude.rnf fileSystemIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFileSystems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DescribeFileSystems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFileSystems where
  toJSON DescribeFileSystems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FileSystemIds" Data..=) Prelude.<$> fileSystemIds,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFileSystems where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFileSystems where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for @DescribeFileSystems@ operation.
--
-- /See:/ 'newDescribeFileSystemsResponse' smart constructor.
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'
  { -- | An array of file system descriptions.
    fileSystems :: Prelude.Maybe [FileSystem],
    -- | Present if there are more file systems than returned in the response
    -- (String). You can use the @NextToken@ value in the later request to
    -- fetch the descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystems', 'describeFileSystemsResponse_fileSystems' - An array of file system descriptions.
--
-- 'nextToken', 'describeFileSystemsResponse_nextToken' - Present if there are more file systems than returned in the response
-- (String). You can use the @NextToken@ value in the later request to
-- fetch the descriptions.
--
-- 'httpStatus', 'describeFileSystemsResponse_httpStatus' - The response's http status code.
newDescribeFileSystemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFileSystemsResponse
newDescribeFileSystemsResponse pHttpStatus_ =
  DescribeFileSystemsResponse'
    { fileSystems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of file system descriptions.
describeFileSystemsResponse_fileSystems :: Lens.Lens' DescribeFileSystemsResponse (Prelude.Maybe [FileSystem])
describeFileSystemsResponse_fileSystems = Lens.lens (\DescribeFileSystemsResponse' {fileSystems} -> fileSystems) (\s@DescribeFileSystemsResponse' {} a -> s {fileSystems = a} :: DescribeFileSystemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Present if there are more file systems than returned in the response
-- (String). You can use the @NextToken@ value in the later request to
-- fetch the descriptions.
describeFileSystemsResponse_nextToken :: Lens.Lens' DescribeFileSystemsResponse (Prelude.Maybe Prelude.Text)
describeFileSystemsResponse_nextToken = Lens.lens (\DescribeFileSystemsResponse' {nextToken} -> nextToken) (\s@DescribeFileSystemsResponse' {} a -> s {nextToken = a} :: DescribeFileSystemsResponse)

-- | The response's http status code.
describeFileSystemsResponse_httpStatus :: Lens.Lens' DescribeFileSystemsResponse Prelude.Int
describeFileSystemsResponse_httpStatus = Lens.lens (\DescribeFileSystemsResponse' {httpStatus} -> httpStatus) (\s@DescribeFileSystemsResponse' {} a -> s {httpStatus = a} :: DescribeFileSystemsResponse)

instance Prelude.NFData DescribeFileSystemsResponse where
  rnf DescribeFileSystemsResponse' {..} =
    Prelude.rnf fileSystems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
