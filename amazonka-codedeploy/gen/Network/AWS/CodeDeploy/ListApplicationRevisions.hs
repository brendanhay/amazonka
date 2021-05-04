{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    sortOrder :: Prelude.Maybe SortOrder,
    -- | An identifier returned from the previous @ListApplicationRevisions@
    -- call. It can be used to return the next set of applications in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 bucket name to limit the search for revisions.
    --
    -- If set to null, all of the user\'s buckets are searched.
    s3Bucket :: Prelude.Maybe Prelude.Text,
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
    deployed :: Prelude.Maybe ListStateFilterAction,
    -- | A key prefix for the set of Amazon S3 objects to limit the search for
    -- revisions.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
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
    sortBy :: Prelude.Maybe ApplicationRevisionSortBy,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListApplicationRevisions
newListApplicationRevisions pApplicationName_ =
  ListApplicationRevisions'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      deployed = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      sortBy = Prelude.Nothing,
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
listApplicationRevisions_sortOrder :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe SortOrder)
listApplicationRevisions_sortOrder = Lens.lens (\ListApplicationRevisions' {sortOrder} -> sortOrder) (\s@ListApplicationRevisions' {} a -> s {sortOrder = a} :: ListApplicationRevisions)

-- | An identifier returned from the previous @ListApplicationRevisions@
-- call. It can be used to return the next set of applications in the list.
listApplicationRevisions_nextToken :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
listApplicationRevisions_nextToken = Lens.lens (\ListApplicationRevisions' {nextToken} -> nextToken) (\s@ListApplicationRevisions' {} a -> s {nextToken = a} :: ListApplicationRevisions)

-- | An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user\'s buckets are searched.
listApplicationRevisions_s3Bucket :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
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
listApplicationRevisions_deployed :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe ListStateFilterAction)
listApplicationRevisions_deployed = Lens.lens (\ListApplicationRevisions' {deployed} -> deployed) (\s@ListApplicationRevisions' {} a -> s {deployed = a} :: ListApplicationRevisions)

-- | A key prefix for the set of Amazon S3 objects to limit the search for
-- revisions.
listApplicationRevisions_s3KeyPrefix :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
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
listApplicationRevisions_sortBy :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe ApplicationRevisionSortBy)
listApplicationRevisions_sortBy = Lens.lens (\ListApplicationRevisions' {sortBy} -> sortBy) (\s@ListApplicationRevisions' {} a -> s {sortBy = a} :: ListApplicationRevisions)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
listApplicationRevisions_applicationName :: Lens.Lens' ListApplicationRevisions Prelude.Text
listApplicationRevisions_applicationName = Lens.lens (\ListApplicationRevisions' {applicationName} -> applicationName) (\s@ListApplicationRevisions' {} a -> s {applicationName = a} :: ListApplicationRevisions)

instance Pager.AWSPager ListApplicationRevisions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_revisions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listApplicationRevisions_nextToken
          Lens..~ rs
          Lens.^? listApplicationRevisionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListApplicationRevisions where
  type
    Rs ListApplicationRevisions =
      ListApplicationRevisionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationRevisionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "revisions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationRevisions

instance Prelude.NFData ListApplicationRevisions

instance Prelude.ToHeaders ListApplicationRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListApplicationRevisions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListApplicationRevisions where
  toJSON ListApplicationRevisions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("s3Bucket" Prelude..=) Prelude.<$> s3Bucket,
            ("deployed" Prelude..=) Prelude.<$> deployed,
            ("s3KeyPrefix" Prelude..=) Prelude.<$> s3KeyPrefix,
            ("sortBy" Prelude..=) Prelude.<$> sortBy,
            Prelude.Just
              ("applicationName" Prelude..= applicationName)
          ]
      )

instance Prelude.ToPath ListApplicationRevisions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListApplicationRevisions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'newListApplicationRevisionsResponse' smart constructor.
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list application revisions call
    -- to return the next set of application revisions in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of locations that contain the matching revisions.
    revisions :: Prelude.Maybe [RevisionLocation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListApplicationRevisionsResponse
newListApplicationRevisionsResponse pHttpStatus_ =
  ListApplicationRevisionsResponse'
    { nextToken =
        Prelude.Nothing,
      revisions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list application revisions call
-- to return the next set of application revisions in the list.
listApplicationRevisionsResponse_nextToken :: Lens.Lens' ListApplicationRevisionsResponse (Prelude.Maybe Prelude.Text)
listApplicationRevisionsResponse_nextToken = Lens.lens (\ListApplicationRevisionsResponse' {nextToken} -> nextToken) (\s@ListApplicationRevisionsResponse' {} a -> s {nextToken = a} :: ListApplicationRevisionsResponse)

-- | A list of locations that contain the matching revisions.
listApplicationRevisionsResponse_revisions :: Lens.Lens' ListApplicationRevisionsResponse (Prelude.Maybe [RevisionLocation])
listApplicationRevisionsResponse_revisions = Lens.lens (\ListApplicationRevisionsResponse' {revisions} -> revisions) (\s@ListApplicationRevisionsResponse' {} a -> s {revisions = a} :: ListApplicationRevisionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listApplicationRevisionsResponse_httpStatus :: Lens.Lens' ListApplicationRevisionsResponse Prelude.Int
listApplicationRevisionsResponse_httpStatus = Lens.lens (\ListApplicationRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationRevisionsResponse' {} a -> s {httpStatus = a} :: ListApplicationRevisionsResponse)

instance
  Prelude.NFData
    ListApplicationRevisionsResponse
