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
-- Module      : Amazonka.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available directories that are registered with Amazon
-- WorkSpaces.
--
-- This operation returns paginated results.
module Amazonka.WorkSpaces.DescribeWorkspaceDirectories
  ( -- * Creating a Request
    DescribeWorkspaceDirectories (..),
    newDescribeWorkspaceDirectories,

    -- * Request Lenses
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,
    describeWorkspaceDirectories_nextToken,

    -- * Destructuring the Response
    DescribeWorkspaceDirectoriesResponse (..),
    newDescribeWorkspaceDirectoriesResponse,

    -- * Response Lenses
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceDirectories' smart constructor.
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
  { -- | The identifiers of the directories. If the value is null, all
    -- directories are retrieved.
    directoryIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of directories to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryIds', 'describeWorkspaceDirectories_directoryIds' - The identifiers of the directories. If the value is null, all
-- directories are retrieved.
--
-- 'limit', 'describeWorkspaceDirectories_limit' - The maximum number of directories to return.
--
-- 'nextToken', 'describeWorkspaceDirectories_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
newDescribeWorkspaceDirectories ::
  DescribeWorkspaceDirectories
newDescribeWorkspaceDirectories =
  DescribeWorkspaceDirectories'
    { directoryIds =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The identifiers of the directories. If the value is null, all
-- directories are retrieved.
describeWorkspaceDirectories_directoryIds :: Lens.Lens' DescribeWorkspaceDirectories (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspaceDirectories_directoryIds = Lens.lens (\DescribeWorkspaceDirectories' {directoryIds} -> directoryIds) (\s@DescribeWorkspaceDirectories' {} a -> s {directoryIds = a} :: DescribeWorkspaceDirectories) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of directories to return.
describeWorkspaceDirectories_limit :: Lens.Lens' DescribeWorkspaceDirectories (Prelude.Maybe Prelude.Natural)
describeWorkspaceDirectories_limit = Lens.lens (\DescribeWorkspaceDirectories' {limit} -> limit) (\s@DescribeWorkspaceDirectories' {} a -> s {limit = a} :: DescribeWorkspaceDirectories)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceDirectories_nextToken :: Lens.Lens' DescribeWorkspaceDirectories (Prelude.Maybe Prelude.Text)
describeWorkspaceDirectories_nextToken = Lens.lens (\DescribeWorkspaceDirectories' {nextToken} -> nextToken) (\s@DescribeWorkspaceDirectories' {} a -> s {nextToken = a} :: DescribeWorkspaceDirectories)

instance Core.AWSPager DescribeWorkspaceDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceDirectoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceDirectoriesResponse_directories
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeWorkspaceDirectories_nextToken
          Lens..~ rs
          Lens.^? describeWorkspaceDirectoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeWorkspaceDirectories where
  type
    AWSResponse DescribeWorkspaceDirectories =
      DescribeWorkspaceDirectoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceDirectoriesResponse'
            Prelude.<$> (x Data..?> "Directories" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeWorkspaceDirectories
  where
  hashWithSalt _salt DescribeWorkspaceDirectories' {..} =
    _salt `Prelude.hashWithSalt` directoryIds
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeWorkspaceDirectories where
  rnf DescribeWorkspaceDirectories' {..} =
    Prelude.rnf directoryIds
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeWorkspaceDirectories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeWorkspaceDirectories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWorkspaceDirectories where
  toJSON DescribeWorkspaceDirectories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryIds" Data..=) Prelude.<$> directoryIds,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeWorkspaceDirectories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWorkspaceDirectories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceDirectoriesResponse' smart constructor.
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
  { -- | Information about the directories.
    directories :: Prelude.Maybe [WorkspaceDirectory],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceDirectoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directories', 'describeWorkspaceDirectoriesResponse_directories' - Information about the directories.
--
-- 'nextToken', 'describeWorkspaceDirectoriesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeWorkspaceDirectoriesResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceDirectoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspaceDirectoriesResponse
newDescribeWorkspaceDirectoriesResponse pHttpStatus_ =
  DescribeWorkspaceDirectoriesResponse'
    { directories =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the directories.
describeWorkspaceDirectoriesResponse_directories :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Prelude.Maybe [WorkspaceDirectory])
describeWorkspaceDirectoriesResponse_directories = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {directories} -> directories) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {directories = a} :: DescribeWorkspaceDirectoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeWorkspaceDirectoriesResponse_nextToken :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Prelude.Maybe Prelude.Text)
describeWorkspaceDirectoriesResponse_nextToken = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceDirectoriesResponse)

-- | The response's http status code.
describeWorkspaceDirectoriesResponse_httpStatus :: Lens.Lens' DescribeWorkspaceDirectoriesResponse Prelude.Int
describeWorkspaceDirectoriesResponse_httpStatus = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceDirectoriesResponse)

instance
  Prelude.NFData
    DescribeWorkspaceDirectoriesResponse
  where
  rnf DescribeWorkspaceDirectoriesResponse' {..} =
    Prelude.rnf directories
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
