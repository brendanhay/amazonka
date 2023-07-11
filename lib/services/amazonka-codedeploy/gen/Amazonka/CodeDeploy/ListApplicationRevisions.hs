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
-- Module      : Amazonka.CodeDeploy.ListApplicationRevisions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about revisions for an application.
--
-- This operation returns paginated results.
module Amazonka.CodeDeploy.ListApplicationRevisions
  ( -- * Creating a Request
    ListApplicationRevisions (..),
    newListApplicationRevisions,

    -- * Request Lenses
    listApplicationRevisions_deployed,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_s3Bucket,
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_sortBy,
    listApplicationRevisions_sortOrder,
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'newListApplicationRevisions' smart constructor.
data ListApplicationRevisions = ListApplicationRevisions'
  { -- | Whether to list revisions based on whether the revision is the target
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
    -- | An identifier returned from the previous @ListApplicationRevisions@
    -- call. It can be used to return the next set of applications in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 bucket name to limit the search for revisions.
    --
    -- If set to null, all of the user\'s buckets are searched.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | A key prefix for the set of Amazon S3 objects to limit the search for
    -- revisions.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The column name to use to sort the list results:
    --
    -- -   @registerTime@: Sort by the time the revisions were registered with
    --     CodeDeploy.
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
    -- | The order in which to sort the list results:
    --
    -- -   @ascending@: ascending order.
    --
    -- -   @descending@: descending order.
    --
    -- If not specified, the results are sorted in ascending order.
    --
    -- If set to null, the results are sorted in an arbitrary order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'listApplicationRevisions_nextToken' - An identifier returned from the previous @ListApplicationRevisions@
-- call. It can be used to return the next set of applications in the list.
--
-- 's3Bucket', 'listApplicationRevisions_s3Bucket' - An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user\'s buckets are searched.
--
-- 's3KeyPrefix', 'listApplicationRevisions_s3KeyPrefix' - A key prefix for the set of Amazon S3 objects to limit the search for
-- revisions.
--
-- 'sortBy', 'listApplicationRevisions_sortBy' - The column name to use to sort the list results:
--
-- -   @registerTime@: Sort by the time the revisions were registered with
--     CodeDeploy.
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
-- 'applicationName', 'listApplicationRevisions_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
newListApplicationRevisions ::
  -- | 'applicationName'
  Prelude.Text ->
  ListApplicationRevisions
newListApplicationRevisions pApplicationName_ =
  ListApplicationRevisions'
    { deployed =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      applicationName = pApplicationName_
    }

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

-- | An identifier returned from the previous @ListApplicationRevisions@
-- call. It can be used to return the next set of applications in the list.
listApplicationRevisions_nextToken :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
listApplicationRevisions_nextToken = Lens.lens (\ListApplicationRevisions' {nextToken} -> nextToken) (\s@ListApplicationRevisions' {} a -> s {nextToken = a} :: ListApplicationRevisions)

-- | An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user\'s buckets are searched.
listApplicationRevisions_s3Bucket :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
listApplicationRevisions_s3Bucket = Lens.lens (\ListApplicationRevisions' {s3Bucket} -> s3Bucket) (\s@ListApplicationRevisions' {} a -> s {s3Bucket = a} :: ListApplicationRevisions)

-- | A key prefix for the set of Amazon S3 objects to limit the search for
-- revisions.
listApplicationRevisions_s3KeyPrefix :: Lens.Lens' ListApplicationRevisions (Prelude.Maybe Prelude.Text)
listApplicationRevisions_s3KeyPrefix = Lens.lens (\ListApplicationRevisions' {s3KeyPrefix} -> s3KeyPrefix) (\s@ListApplicationRevisions' {} a -> s {s3KeyPrefix = a} :: ListApplicationRevisions)

-- | The column name to use to sort the list results:
--
-- -   @registerTime@: Sort by the time the revisions were registered with
--     CodeDeploy.
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

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
listApplicationRevisions_applicationName :: Lens.Lens' ListApplicationRevisions Prelude.Text
listApplicationRevisions_applicationName = Lens.lens (\ListApplicationRevisions' {applicationName} -> applicationName) (\s@ListApplicationRevisions' {} a -> s {applicationName = a} :: ListApplicationRevisions)

instance Core.AWSPager ListApplicationRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationRevisionsResponse_revisions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApplicationRevisions_nextToken
          Lens..~ rs
          Lens.^? listApplicationRevisionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationRevisions where
  type
    AWSResponse ListApplicationRevisions =
      ListApplicationRevisionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationRevisionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "revisions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationRevisions where
  hashWithSalt _salt ListApplicationRevisions' {..} =
    _salt
      `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData ListApplicationRevisions where
  rnf ListApplicationRevisions' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders ListApplicationRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.ListApplicationRevisions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplicationRevisions where
  toJSON ListApplicationRevisions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deployed" Data..=) Prelude.<$> deployed,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("s3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("s3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just
              ("applicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath ListApplicationRevisions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApplicationRevisions where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listApplicationRevisionsResponse_revisions = Lens.lens (\ListApplicationRevisionsResponse' {revisions} -> revisions) (\s@ListApplicationRevisionsResponse' {} a -> s {revisions = a} :: ListApplicationRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApplicationRevisionsResponse_httpStatus :: Lens.Lens' ListApplicationRevisionsResponse Prelude.Int
listApplicationRevisionsResponse_httpStatus = Lens.lens (\ListApplicationRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationRevisionsResponse' {} a -> s {httpStatus = a} :: ListApplicationRevisionsResponse)

instance
  Prelude.NFData
    ListApplicationRevisionsResponse
  where
  rnf ListApplicationRevisionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf revisions
      `Prelude.seq` Prelude.rnf httpStatus
