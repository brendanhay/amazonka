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
-- Module      : Amazonka.DirectoryService.DescribeDirectories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directories that belong to this account.
--
-- You can retrieve information about specific directories by passing the
-- directory identifiers in the @DirectoryIds@ parameter. Otherwise, all
-- directories that belong to the current account are returned.
--
-- This operation supports pagination with the use of the @NextToken@
-- request and response parameters. If more results are available, the
-- @DescribeDirectoriesResult.NextToken@ member contains a token that you
-- pass in the next call to DescribeDirectories to retrieve the next set of
-- items.
--
-- You can also specify a maximum number of return results with the @Limit@
-- parameter.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.DescribeDirectories
  ( -- * Creating a Request
    DescribeDirectories (..),
    newDescribeDirectories,

    -- * Request Lenses
    describeDirectories_nextToken,
    describeDirectories_directoryIds,
    describeDirectories_limit,

    -- * Destructuring the Response
    DescribeDirectoriesResponse (..),
    newDescribeDirectoriesResponse,

    -- * Response Lenses
    describeDirectoriesResponse_nextToken,
    describeDirectoriesResponse_directoryDescriptions,
    describeDirectoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DescribeDirectories operation.
--
-- /See:/ 'newDescribeDirectories' smart constructor.
data DescribeDirectories = DescribeDirectories'
  { -- | The @DescribeDirectoriesResult.NextToken@ value from a previous call to
    -- DescribeDirectories. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers of the directories for which to obtain the
    -- information. If this member is null, all directories that belong to the
    -- current account are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    directoryIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return. If this value is zero, the
    -- maximum number of items is specified by the limitations of the
    -- operation.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectories_nextToken' - The @DescribeDirectoriesResult.NextToken@ value from a previous call to
-- DescribeDirectories. Pass null if this is the first call.
--
-- 'directoryIds', 'describeDirectories_directoryIds' - A list of identifiers of the directories for which to obtain the
-- information. If this member is null, all directories that belong to the
-- current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- 'limit', 'describeDirectories_limit' - The maximum number of items to return. If this value is zero, the
-- maximum number of items is specified by the limitations of the
-- operation.
newDescribeDirectories ::
  DescribeDirectories
newDescribeDirectories =
  DescribeDirectories'
    { nextToken = Prelude.Nothing,
      directoryIds = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @DescribeDirectoriesResult.NextToken@ value from a previous call to
-- DescribeDirectories. Pass null if this is the first call.
describeDirectories_nextToken :: Lens.Lens' DescribeDirectories (Prelude.Maybe Prelude.Text)
describeDirectories_nextToken = Lens.lens (\DescribeDirectories' {nextToken} -> nextToken) (\s@DescribeDirectories' {} a -> s {nextToken = a} :: DescribeDirectories)

-- | A list of identifiers of the directories for which to obtain the
-- information. If this member is null, all directories that belong to the
-- current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeDirectories_directoryIds :: Lens.Lens' DescribeDirectories (Prelude.Maybe [Prelude.Text])
describeDirectories_directoryIds = Lens.lens (\DescribeDirectories' {directoryIds} -> directoryIds) (\s@DescribeDirectories' {} a -> s {directoryIds = a} :: DescribeDirectories) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return. If this value is zero, the
-- maximum number of items is specified by the limitations of the
-- operation.
describeDirectories_limit :: Lens.Lens' DescribeDirectories (Prelude.Maybe Prelude.Natural)
describeDirectories_limit = Lens.lens (\DescribeDirectories' {limit} -> limit) (\s@DescribeDirectories' {} a -> s {limit = a} :: DescribeDirectories)

instance Core.AWSPager DescribeDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectoriesResponse_directoryDescriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDirectories_nextToken
          Lens..~ rs
          Lens.^? describeDirectoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDirectories where
  type
    AWSResponse DescribeDirectories =
      DescribeDirectoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectoriesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DirectoryDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDirectories where
  hashWithSalt _salt DescribeDirectories' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryIds
      `Prelude.hashWithSalt` limit

instance Prelude.NFData DescribeDirectories where
  rnf DescribeDirectories' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryIds
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders DescribeDirectories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeDirectories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDirectories where
  toJSON DescribeDirectories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("DirectoryIds" Core..=) Prelude.<$> directoryIds,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeDirectories where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDirectories where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DescribeDirectories operation.
--
-- /See:/ 'newDescribeDirectoriesResponse' smart constructor.
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter in a subsequent call to DescribeDirectories to
    -- retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of DirectoryDescription objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the @Limit@ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
    directoryDescriptions :: Prelude.Maybe [DirectoryDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectoriesResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDirectories to
-- retrieve the next set of items.
--
-- 'directoryDescriptions', 'describeDirectoriesResponse_directoryDescriptions' - The list of DirectoryDescription objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the @Limit@ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
--
-- 'httpStatus', 'describeDirectoriesResponse_httpStatus' - The response's http status code.
newDescribeDirectoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectoriesResponse
newDescribeDirectoriesResponse pHttpStatus_ =
  DescribeDirectoriesResponse'
    { nextToken =
        Prelude.Nothing,
      directoryDescriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDirectories to
-- retrieve the next set of items.
describeDirectoriesResponse_nextToken :: Lens.Lens' DescribeDirectoriesResponse (Prelude.Maybe Prelude.Text)
describeDirectoriesResponse_nextToken = Lens.lens (\DescribeDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeDirectoriesResponse)

-- | The list of DirectoryDescription objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the @Limit@ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
describeDirectoriesResponse_directoryDescriptions :: Lens.Lens' DescribeDirectoriesResponse (Prelude.Maybe [DirectoryDescription])
describeDirectoriesResponse_directoryDescriptions = Lens.lens (\DescribeDirectoriesResponse' {directoryDescriptions} -> directoryDescriptions) (\s@DescribeDirectoriesResponse' {} a -> s {directoryDescriptions = a} :: DescribeDirectoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDirectoriesResponse_httpStatus :: Lens.Lens' DescribeDirectoriesResponse Prelude.Int
describeDirectoriesResponse_httpStatus = Lens.lens (\DescribeDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeDirectoriesResponse)

instance Prelude.NFData DescribeDirectoriesResponse where
  rnf DescribeDirectoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
