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
-- Module      : Amazonka.RolesAnywhere.ListSubjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subjects in the authenticated account and Amazon Web Services
-- Region.
--
-- __Required permissions:__ @rolesanywhere:ListSubjects@.
--
-- This operation returns paginated results.
module Amazonka.RolesAnywhere.ListSubjects
  ( -- * Creating a Request
    ListSubjects (..),
    newListSubjects,

    -- * Request Lenses
    listSubjects_nextToken,
    listSubjects_pageSize,

    -- * Destructuring the Response
    ListSubjectsResponse (..),
    newListSubjectsResponse,

    -- * Response Lenses
    listSubjectsResponse_nextToken,
    listSubjectsResponse_subjects,
    listSubjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newListSubjects' smart constructor.
data ListSubjects = ListSubjects'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of resources in the paginated list.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubjects_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'pageSize', 'listSubjects_pageSize' - The number of resources in the paginated list.
newListSubjects ::
  ListSubjects
newListSubjects =
  ListSubjects'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listSubjects_nextToken :: Lens.Lens' ListSubjects (Prelude.Maybe Prelude.Text)
listSubjects_nextToken = Lens.lens (\ListSubjects' {nextToken} -> nextToken) (\s@ListSubjects' {} a -> s {nextToken = a} :: ListSubjects)

-- | The number of resources in the paginated list.
listSubjects_pageSize :: Lens.Lens' ListSubjects (Prelude.Maybe Prelude.Int)
listSubjects_pageSize = Lens.lens (\ListSubjects' {pageSize} -> pageSize) (\s@ListSubjects' {} a -> s {pageSize = a} :: ListSubjects)

instance Core.AWSPager ListSubjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubjectsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubjectsResponse_subjects
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSubjects_nextToken
          Lens..~ rs
          Lens.^? listSubjectsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSubjects where
  type AWSResponse ListSubjects = ListSubjectsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubjectsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "subjects" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubjects where
  hashWithSalt _salt ListSubjects' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListSubjects where
  rnf ListSubjects' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListSubjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSubjects where
  toPath = Prelude.const "/subjects"

instance Data.ToQuery ListSubjects where
  toQuery ListSubjects' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListSubjectsResponse' smart constructor.
data ListSubjectsResponse = ListSubjectsResponse'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subjects.
    subjects :: Prelude.Maybe [SubjectSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubjectsResponse_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'subjects', 'listSubjectsResponse_subjects' - A list of subjects.
--
-- 'httpStatus', 'listSubjectsResponse_httpStatus' - The response's http status code.
newListSubjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubjectsResponse
newListSubjectsResponse pHttpStatus_ =
  ListSubjectsResponse'
    { nextToken = Prelude.Nothing,
      subjects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listSubjectsResponse_nextToken :: Lens.Lens' ListSubjectsResponse (Prelude.Maybe Prelude.Text)
listSubjectsResponse_nextToken = Lens.lens (\ListSubjectsResponse' {nextToken} -> nextToken) (\s@ListSubjectsResponse' {} a -> s {nextToken = a} :: ListSubjectsResponse)

-- | A list of subjects.
listSubjectsResponse_subjects :: Lens.Lens' ListSubjectsResponse (Prelude.Maybe [SubjectSummary])
listSubjectsResponse_subjects = Lens.lens (\ListSubjectsResponse' {subjects} -> subjects) (\s@ListSubjectsResponse' {} a -> s {subjects = a} :: ListSubjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubjectsResponse_httpStatus :: Lens.Lens' ListSubjectsResponse Prelude.Int
listSubjectsResponse_httpStatus = Lens.lens (\ListSubjectsResponse' {httpStatus} -> httpStatus) (\s@ListSubjectsResponse' {} a -> s {httpStatus = a} :: ListSubjectsResponse)

instance Prelude.NFData ListSubjectsResponse where
  rnf ListSubjectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subjects
      `Prelude.seq` Prelude.rnf httpStatus
