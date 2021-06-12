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
-- Module      : Network.AWS.CodeCommit.GetDifferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the differences in a valid commit specifier
-- (such as a branch, tag, HEAD, commit ID, or other fully qualified
-- reference). Results can be limited to a specified path.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetDifferences
  ( -- * Creating a Request
    GetDifferences (..),
    newGetDifferences,

    -- * Request Lenses
    getDifferences_nextToken,
    getDifferences_beforeCommitSpecifier,
    getDifferences_maxResults,
    getDifferences_beforePath,
    getDifferences_afterPath,
    getDifferences_repositoryName,
    getDifferences_afterCommitSpecifier,

    -- * Destructuring the Response
    GetDifferencesResponse (..),
    newGetDifferencesResponse,

    -- * Response Lenses
    getDifferencesResponse_nextToken,
    getDifferencesResponse_differences,
    getDifferencesResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDifferences' smart constructor.
data GetDifferences = GetDifferences'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, the full commit ID). Optional. If not
    -- specified, all changes before the @afterCommitSpecifier@ value are
    -- shown. If you do not use @beforeCommitSpecifier@ in your request,
    -- consider limiting the results with @maxResults@.
    beforeCommitSpecifier :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Core.Maybe Core.Int,
    -- | The file path in which to check for differences. Limits the results to
    -- this path. Can also be used to specify the previous name of a directory
    -- or folder. If @beforePath@ and @afterPath@ are not specified,
    -- differences are shown for all paths.
    beforePath :: Core.Maybe Core.Text,
    -- | The file path in which to check differences. Limits the results to this
    -- path. Can also be used to specify the changed name of a directory or
    -- folder, if it has changed. If not specified, differences are shown for
    -- all paths.
    afterPath :: Core.Maybe Core.Text,
    -- | The name of the repository where you want to get differences.
    repositoryName :: Core.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit.
    afterCommitSpecifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDifferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDifferences_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'beforeCommitSpecifier', 'getDifferences_beforeCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, the full commit ID). Optional. If not
-- specified, all changes before the @afterCommitSpecifier@ value are
-- shown. If you do not use @beforeCommitSpecifier@ in your request,
-- consider limiting the results with @maxResults@.
--
-- 'maxResults', 'getDifferences_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'beforePath', 'getDifferences_beforePath' - The file path in which to check for differences. Limits the results to
-- this path. Can also be used to specify the previous name of a directory
-- or folder. If @beforePath@ and @afterPath@ are not specified,
-- differences are shown for all paths.
--
-- 'afterPath', 'getDifferences_afterPath' - The file path in which to check differences. Limits the results to this
-- path. Can also be used to specify the changed name of a directory or
-- folder, if it has changed. If not specified, differences are shown for
-- all paths.
--
-- 'repositoryName', 'getDifferences_repositoryName' - The name of the repository where you want to get differences.
--
-- 'afterCommitSpecifier', 'getDifferences_afterCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit.
newGetDifferences ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'afterCommitSpecifier'
  Core.Text ->
  GetDifferences
newGetDifferences
  pRepositoryName_
  pAfterCommitSpecifier_ =
    GetDifferences'
      { nextToken = Core.Nothing,
        beforeCommitSpecifier = Core.Nothing,
        maxResults = Core.Nothing,
        beforePath = Core.Nothing,
        afterPath = Core.Nothing,
        repositoryName = pRepositoryName_,
        afterCommitSpecifier = pAfterCommitSpecifier_
      }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getDifferences_nextToken :: Lens.Lens' GetDifferences (Core.Maybe Core.Text)
getDifferences_nextToken = Lens.lens (\GetDifferences' {nextToken} -> nextToken) (\s@GetDifferences' {} a -> s {nextToken = a} :: GetDifferences)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, the full commit ID). Optional. If not
-- specified, all changes before the @afterCommitSpecifier@ value are
-- shown. If you do not use @beforeCommitSpecifier@ in your request,
-- consider limiting the results with @maxResults@.
getDifferences_beforeCommitSpecifier :: Lens.Lens' GetDifferences (Core.Maybe Core.Text)
getDifferences_beforeCommitSpecifier = Lens.lens (\GetDifferences' {beforeCommitSpecifier} -> beforeCommitSpecifier) (\s@GetDifferences' {} a -> s {beforeCommitSpecifier = a} :: GetDifferences)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
getDifferences_maxResults :: Lens.Lens' GetDifferences (Core.Maybe Core.Int)
getDifferences_maxResults = Lens.lens (\GetDifferences' {maxResults} -> maxResults) (\s@GetDifferences' {} a -> s {maxResults = a} :: GetDifferences)

-- | The file path in which to check for differences. Limits the results to
-- this path. Can also be used to specify the previous name of a directory
-- or folder. If @beforePath@ and @afterPath@ are not specified,
-- differences are shown for all paths.
getDifferences_beforePath :: Lens.Lens' GetDifferences (Core.Maybe Core.Text)
getDifferences_beforePath = Lens.lens (\GetDifferences' {beforePath} -> beforePath) (\s@GetDifferences' {} a -> s {beforePath = a} :: GetDifferences)

-- | The file path in which to check differences. Limits the results to this
-- path. Can also be used to specify the changed name of a directory or
-- folder, if it has changed. If not specified, differences are shown for
-- all paths.
getDifferences_afterPath :: Lens.Lens' GetDifferences (Core.Maybe Core.Text)
getDifferences_afterPath = Lens.lens (\GetDifferences' {afterPath} -> afterPath) (\s@GetDifferences' {} a -> s {afterPath = a} :: GetDifferences)

-- | The name of the repository where you want to get differences.
getDifferences_repositoryName :: Lens.Lens' GetDifferences Core.Text
getDifferences_repositoryName = Lens.lens (\GetDifferences' {repositoryName} -> repositoryName) (\s@GetDifferences' {} a -> s {repositoryName = a} :: GetDifferences)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit.
getDifferences_afterCommitSpecifier :: Lens.Lens' GetDifferences Core.Text
getDifferences_afterCommitSpecifier = Lens.lens (\GetDifferences' {afterCommitSpecifier} -> afterCommitSpecifier) (\s@GetDifferences' {} a -> s {afterCommitSpecifier = a} :: GetDifferences)

instance Core.AWSPager GetDifferences where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDifferencesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDifferencesResponse_differences Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDifferences_nextToken
          Lens..~ rs
          Lens.^? getDifferencesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetDifferences where
  type
    AWSResponse GetDifferences =
      GetDifferencesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDifferencesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "differences" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDifferences

instance Core.NFData GetDifferences

instance Core.ToHeaders GetDifferences where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetDifferences" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDifferences where
  toJSON GetDifferences' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("beforeCommitSpecifier" Core..=)
              Core.<$> beforeCommitSpecifier,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("beforePath" Core..=) Core.<$> beforePath,
            ("afterPath" Core..=) Core.<$> afterPath,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ( "afterCommitSpecifier"
                  Core..= afterCommitSpecifier
              )
          ]
      )

instance Core.ToPath GetDifferences where
  toPath = Core.const "/"

instance Core.ToQuery GetDifferences where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDifferencesResponse' smart constructor.
data GetDifferencesResponse = GetDifferencesResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A data type object that contains information about the differences,
    -- including whether the difference is added, modified, or deleted (A, D,
    -- M).
    differences :: Core.Maybe [Difference],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDifferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDifferencesResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'differences', 'getDifferencesResponse_differences' - A data type object that contains information about the differences,
-- including whether the difference is added, modified, or deleted (A, D,
-- M).
--
-- 'httpStatus', 'getDifferencesResponse_httpStatus' - The response's http status code.
newGetDifferencesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDifferencesResponse
newGetDifferencesResponse pHttpStatus_ =
  GetDifferencesResponse'
    { nextToken = Core.Nothing,
      differences = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getDifferencesResponse_nextToken :: Lens.Lens' GetDifferencesResponse (Core.Maybe Core.Text)
getDifferencesResponse_nextToken = Lens.lens (\GetDifferencesResponse' {nextToken} -> nextToken) (\s@GetDifferencesResponse' {} a -> s {nextToken = a} :: GetDifferencesResponse)

-- | A data type object that contains information about the differences,
-- including whether the difference is added, modified, or deleted (A, D,
-- M).
getDifferencesResponse_differences :: Lens.Lens' GetDifferencesResponse (Core.Maybe [Difference])
getDifferencesResponse_differences = Lens.lens (\GetDifferencesResponse' {differences} -> differences) (\s@GetDifferencesResponse' {} a -> s {differences = a} :: GetDifferencesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDifferencesResponse_httpStatus :: Lens.Lens' GetDifferencesResponse Core.Int
getDifferencesResponse_httpStatus = Lens.lens (\GetDifferencesResponse' {httpStatus} -> httpStatus) (\s@GetDifferencesResponse' {} a -> s {httpStatus = a} :: GetDifferencesResponse)

instance Core.NFData GetDifferencesResponse
