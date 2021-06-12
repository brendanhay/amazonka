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
-- Module      : Network.AWS.CodeDeploy.ListApplicationRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about revisions for an application.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplicationRevisions
  ( -- * Creating a Request
    ListApplicationRevisions (..),
    newListApplicationRevisions,

    -- * Request Lenses
    listApplicationRevisions_sortOrder,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_s3Bucket,
    listApplicationRevisions_deployed,
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_sortBy,
    listApplicationRevisions_applicationName,

    -- * Destructuring the Response
    ListApplicationRevisionsResponse (..),
    newListApplicationRevisionsResponse,

    -- * Response Lenses
    listApplicationRevisionsResponse_nextToken,
    listApplicationRevisionsResponse_revisions,
    listApplicationRevisionsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'newListApplicationRevisions' smart constructor.
data ListApplicationRevisions = ListApplicationRevisions'
  { -- | The order in which to sort the list results:
    --
    -- -   @ascending@: ascending order.
    --
    -- -   @descending@: descending order.
    --
    -- If not specified, the results are sorted in ascending order.
    --
    -- If set to null, the results are sorted in an arbitrary order.
    sortOrder :: Core.Maybe SortOrder,
    -- | An identifier returned from the previous @ListApplicationRevisions@
    -- call. It can be used to return the next set of applications in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | An Amazon S3 bucket name to limit the search for revisions.
    --
    -- If set to null, all of the user\'s buckets are searched.
    s3Bucket :: Core.Maybe Core.Text,
    -- | Whether to list revisions based on whether the revision is the target
    -- revision of a deployment group:
    --
    -- -   @include@: List revisions that are target revisions of a deployment
    --     group.
    --
    -- -   @exclude@: Do not list revisions that are target revisions of a
    --     deployment group.
    --
    -- -   @ignore@: List all revisions.
    deployed :: Core.Maybe ListStateFilterAction,
    -- | A key prefix for the set of Amazon S3 objects to limit the search for
    -- revisions.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | The column name to use to sort the list results:
    --
    -- -   @registerTime@: Sort by the time the revisions were registered with
    --     AWS CodeDeploy.
    --
    -- -   @firstUsedTime@: Sort by the time the revisions were first used in a
    --     deployment.
    --
    -- -   @lastUsedTime@: Sort by the time the revisions were last used in a
    --     deployment.
    --
    -- If not specified or set to null, the results are returned in an
    -- arbitrary order.
    sortBy :: Core.Maybe ApplicationRevisionSortBy,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listApplicationRevisions_sortOrder' - The order in which to sort the list results:
--
-- -   @ascending@: ascending order.
--
-- -   @descending@: descending order.
--
-- If not specified, the results are sorted in ascending order.
--
-- If set to null, the results are sorted in an arbitrary order.
--
-- 'nextToken', 'listApplicationRevisions_nextToken' - An identifier returned from the previous @ListApplicationRevisions@
-- call. It can be used to return the next set of applications in the list.
--
-- 's3Bucket', 'listApplicationRevisions_s3Bucket' - An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user\'s buckets are searched.
--
-- 'deployed', 'listApplicationRevisions_deployed' - Whether to list revisions based on whether the revision is the target
-- revision of a deployment group:
--
-- -   @include@: List revisions that are target revisions of a deployment
--     group.
--
-- -   @exclude@: Do not list revisions that are target revisions of a
--     deployment group.
--
-- -   @ignore@: List all revisions.
--
-- 's3KeyPrefix', 'listApplicationRevisions_s3KeyPrefix' - A key prefix for the set of Amazon S3 objects to limit the search for
-- revisions.
--
-- 'sortBy', 'listApplicationRevisions_sortBy' - The column name to use to sort the list results:
--
-- -   @registerTime@: Sort by the time the revisions were registered with
--     AWS CodeDeploy.
--
-- -   @firstUsedTime@: Sort by the time the revisions were first used in a
--     deployment.
--
-- -   @lastUsedTime@: Sort by the time the revisions were last used in a
--     deployment.
--
-- If not specified or set to null, the results are returned in an
-- arbitrary order.
--
-- 'applicationName', 'listApplicationRevisions_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
newListApplicationRevisions ::
  -- | 'applicationName'
  Core.Text ->
  ListApplicationRevisions
newListApplicationRevisions pApplicationName_ =
  ListApplicationRevisions'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      s3Bucket = Core.Nothing,
      deployed = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      sortBy = Core.Nothing,
      applicationName = pApplicationName_
    }

-- | The order in which to sort the list results:
--
-- -   @ascending@: ascending order.
--
-- -   @descending@: descending order.
--
-- If not specified, the results are sorted in ascending order.
--
-- If set to null, the results are sorted in an arbitrary order.
listApplicationRevisions_sortOrder :: Lens.Lens' ListApplicationRevisions (Core.Maybe SortOrder)
listApplicationRevisions_sortOrder = Lens.lens (\ListApplicationRevisions' {sortOrder} -> sortOrder) (\s@ListApplicationRevisions' {} a -> s {sortOrder = a} :: ListApplicationRevisions)

-- | An identifier returned from the previous @ListApplicationRevisions@
-- call. It can be used to return the next set of applications in the list.
listApplicationRevisions_nextToken :: Lens.Lens' ListApplicationRevisions (Core.Maybe Core.Text)
listApplicationRevisions_nextToken = Lens.lens (\ListApplicationRevisions' {nextToken} -> nextToken) (\s@ListApplicationRevisions' {} a -> s {nextToken = a} :: ListApplicationRevisions)

-- | An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user\'s buckets are searched.
listApplicationRevisions_s3Bucket :: Lens.Lens' ListApplicationRevisions (Core.Maybe Core.Text)
listApplicationRevisions_s3Bucket = Lens.lens (\ListApplicationRevisions' {s3Bucket} -> s3Bucket) (\s@ListApplicationRevisions' {} a -> s {s3Bucket = a} :: ListApplicationRevisions)

-- | Whether to list revisions based on whether the revision is the target
-- revision of a deployment group:
--
-- -   @include@: List revisions that are target revisions of a deployment
--     group.
--
-- -   @exclude@: Do not list revisions that are target revisions of a
--     deployment group.
--
-- -   @ignore@: List all revisions.
listApplicationRevisions_deployed :: Lens.Lens' ListApplicationRevisions (Core.Maybe ListStateFilterAction)
listApplicationRevisions_deployed = Lens.lens (\ListApplicationRevisions' {deployed} -> deployed) (\s@ListApplicationRevisions' {} a -> s {deployed = a} :: ListApplicationRevisions)

-- | A key prefix for the set of Amazon S3 objects to limit the search for
-- revisions.
listApplicationRevisions_s3KeyPrefix :: Lens.Lens' ListApplicationRevisions (Core.Maybe Core.Text)
listApplicationRevisions_s3KeyPrefix = Lens.lens (\ListApplicationRevisions' {s3KeyPrefix} -> s3KeyPrefix) (\s@ListApplicationRevisions' {} a -> s {s3KeyPrefix = a} :: ListApplicationRevisions)

-- | The column name to use to sort the list results:
--
-- -   @registerTime@: Sort by the time the revisions were registered with
--     AWS CodeDeploy.
--
-- -   @firstUsedTime@: Sort by the time the revisions were first used in a
--     deployment.
--
-- -   @lastUsedTime@: Sort by the time the revisions were last used in a
--     deployment.
--
-- If not specified or set to null, the results are returned in an
-- arbitrary order.
listApplicationRevisions_sortBy :: Lens.Lens' ListApplicationRevisions (Core.Maybe ApplicationRevisionSortBy)
listApplicationRevisions_sortBy = Lens.lens (\ListApplicationRevisions' {sortBy} -> sortBy) (\s@ListApplicationRevisions' {} a -> s {sortBy = a} :: ListApplicationRevisions)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
listApplicationRevisions_applicationName :: Lens.Lens' ListApplicationRevisions Core.Text
listApplicationRevisions_applicationName = Lens.lens (\ListApplicationRevisions' {applicationName} -> applicationName) (\s@ListApplicationRevisions' {} a -> s {applicationName = a} :: ListApplicationRevisions)

instance Core.AWSPager ListApplicationRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_revisions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplicationRevisions_nextToken
          Lens..~ rs
          Lens.^? listApplicationRevisionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListApplicationRevisions where
  type
    AWSResponse ListApplicationRevisions =
      ListApplicationRevisionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationRevisionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "revisions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplicationRevisions

instance Core.NFData ListApplicationRevisions

instance Core.ToHeaders ListApplicationRevisions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListApplicationRevisions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApplicationRevisions where
  toJSON ListApplicationRevisions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("s3Bucket" Core..=) Core.<$> s3Bucket,
            ("deployed" Core..=) Core.<$> deployed,
            ("s3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("sortBy" Core..=) Core.<$> sortBy,
            Core.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath ListApplicationRevisions where
  toPath = Core.const "/"

instance Core.ToQuery ListApplicationRevisions where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'newListApplicationRevisionsResponse' smart constructor.
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list application revisions call
    -- to return the next set of application revisions in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of locations that contain the matching revisions.
    revisions :: Core.Maybe [RevisionLocation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationRevisionsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list application revisions call
-- to return the next set of application revisions in the list.
--
-- 'revisions', 'listApplicationRevisionsResponse_revisions' - A list of locations that contain the matching revisions.
--
-- 'httpStatus', 'listApplicationRevisionsResponse_httpStatus' - The response's http status code.
newListApplicationRevisionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApplicationRevisionsResponse
newListApplicationRevisionsResponse pHttpStatus_ =
  ListApplicationRevisionsResponse'
    { nextToken =
        Core.Nothing,
      revisions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list application revisions call
-- to return the next set of application revisions in the list.
listApplicationRevisionsResponse_nextToken :: Lens.Lens' ListApplicationRevisionsResponse (Core.Maybe Core.Text)
listApplicationRevisionsResponse_nextToken = Lens.lens (\ListApplicationRevisionsResponse' {nextToken} -> nextToken) (\s@ListApplicationRevisionsResponse' {} a -> s {nextToken = a} :: ListApplicationRevisionsResponse)

-- | A list of locations that contain the matching revisions.
listApplicationRevisionsResponse_revisions :: Lens.Lens' ListApplicationRevisionsResponse (Core.Maybe [RevisionLocation])
listApplicationRevisionsResponse_revisions = Lens.lens (\ListApplicationRevisionsResponse' {revisions} -> revisions) (\s@ListApplicationRevisionsResponse' {} a -> s {revisions = a} :: ListApplicationRevisionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApplicationRevisionsResponse_httpStatus :: Lens.Lens' ListApplicationRevisionsResponse Core.Int
listApplicationRevisionsResponse_httpStatus = Lens.lens (\ListApplicationRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationRevisionsResponse' {} a -> s {httpStatus = a} :: ListApplicationRevisionsResponse)

instance Core.NFData ListApplicationRevisionsResponse
