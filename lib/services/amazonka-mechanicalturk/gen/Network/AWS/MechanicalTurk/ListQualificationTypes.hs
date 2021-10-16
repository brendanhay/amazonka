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
-- Module      : Network.AWS.MechanicalTurk.ListQualificationTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationTypes@ operation returns a list of Qualification
-- types, filtered by an optional search term.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationTypes
  ( -- * Creating a Request
    ListQualificationTypes (..),
    newListQualificationTypes,

    -- * Request Lenses
    listQualificationTypes_nextToken,
    listQualificationTypes_maxResults,
    listQualificationTypes_query,
    listQualificationTypes_mustBeOwnedByCaller,
    listQualificationTypes_mustBeRequestable,

    -- * Destructuring the Response
    ListQualificationTypesResponse (..),
    newListQualificationTypesResponse,

    -- * Response Lenses
    listQualificationTypesResponse_nextToken,
    listQualificationTypesResponse_qualificationTypes,
    listQualificationTypesResponse_numResults,
    listQualificationTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQualificationTypes' smart constructor.
data ListQualificationTypes = ListQualificationTypes'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A text query against all of the searchable attributes of Qualification
    -- types.
    query :: Prelude.Maybe Prelude.Text,
    -- | Specifies that only Qualification types that the Requester created are
    -- returned. If false, the operation returns all Qualification types.
    mustBeOwnedByCaller :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that only Qualification types that a user can request through
    -- the Amazon Mechanical Turk web site, such as by taking a Qualification
    -- test, are returned as results of the search. Some Qualification types,
    -- such as those assigned automatically by the system, cannot be requested
    -- directly by users. If false, all Qualification types, including those
    -- managed by the system, are considered. Valid values are True | False.
    mustBeRequestable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQualificationTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQualificationTypes_nextToken' - Undocumented member.
--
-- 'maxResults', 'listQualificationTypes_maxResults' - The maximum number of results to return in a single call.
--
-- 'query', 'listQualificationTypes_query' - A text query against all of the searchable attributes of Qualification
-- types.
--
-- 'mustBeOwnedByCaller', 'listQualificationTypes_mustBeOwnedByCaller' - Specifies that only Qualification types that the Requester created are
-- returned. If false, the operation returns all Qualification types.
--
-- 'mustBeRequestable', 'listQualificationTypes_mustBeRequestable' - Specifies that only Qualification types that a user can request through
-- the Amazon Mechanical Turk web site, such as by taking a Qualification
-- test, are returned as results of the search. Some Qualification types,
-- such as those assigned automatically by the system, cannot be requested
-- directly by users. If false, all Qualification types, including those
-- managed by the system, are considered. Valid values are True | False.
newListQualificationTypes ::
  -- | 'mustBeRequestable'
  Prelude.Bool ->
  ListQualificationTypes
newListQualificationTypes pMustBeRequestable_ =
  ListQualificationTypes'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      query = Prelude.Nothing,
      mustBeOwnedByCaller = Prelude.Nothing,
      mustBeRequestable = pMustBeRequestable_
    }

-- | Undocumented member.
listQualificationTypes_nextToken :: Lens.Lens' ListQualificationTypes (Prelude.Maybe Prelude.Text)
listQualificationTypes_nextToken = Lens.lens (\ListQualificationTypes' {nextToken} -> nextToken) (\s@ListQualificationTypes' {} a -> s {nextToken = a} :: ListQualificationTypes)

-- | The maximum number of results to return in a single call.
listQualificationTypes_maxResults :: Lens.Lens' ListQualificationTypes (Prelude.Maybe Prelude.Natural)
listQualificationTypes_maxResults = Lens.lens (\ListQualificationTypes' {maxResults} -> maxResults) (\s@ListQualificationTypes' {} a -> s {maxResults = a} :: ListQualificationTypes)

-- | A text query against all of the searchable attributes of Qualification
-- types.
listQualificationTypes_query :: Lens.Lens' ListQualificationTypes (Prelude.Maybe Prelude.Text)
listQualificationTypes_query = Lens.lens (\ListQualificationTypes' {query} -> query) (\s@ListQualificationTypes' {} a -> s {query = a} :: ListQualificationTypes)

-- | Specifies that only Qualification types that the Requester created are
-- returned. If false, the operation returns all Qualification types.
listQualificationTypes_mustBeOwnedByCaller :: Lens.Lens' ListQualificationTypes (Prelude.Maybe Prelude.Bool)
listQualificationTypes_mustBeOwnedByCaller = Lens.lens (\ListQualificationTypes' {mustBeOwnedByCaller} -> mustBeOwnedByCaller) (\s@ListQualificationTypes' {} a -> s {mustBeOwnedByCaller = a} :: ListQualificationTypes)

-- | Specifies that only Qualification types that a user can request through
-- the Amazon Mechanical Turk web site, such as by taking a Qualification
-- test, are returned as results of the search. Some Qualification types,
-- such as those assigned automatically by the system, cannot be requested
-- directly by users. If false, all Qualification types, including those
-- managed by the system, are considered. Valid values are True | False.
listQualificationTypes_mustBeRequestable :: Lens.Lens' ListQualificationTypes Prelude.Bool
listQualificationTypes_mustBeRequestable = Lens.lens (\ListQualificationTypes' {mustBeRequestable} -> mustBeRequestable) (\s@ListQualificationTypes' {} a -> s {mustBeRequestable = a} :: ListQualificationTypes)

instance Core.AWSPager ListQualificationTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQualificationTypesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listQualificationTypesResponse_qualificationTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listQualificationTypes_nextToken
          Lens..~ rs
          Lens.^? listQualificationTypesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListQualificationTypes where
  type
    AWSResponse ListQualificationTypes =
      ListQualificationTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQualificationTypesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "QualificationTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQualificationTypes

instance Prelude.NFData ListQualificationTypes

instance Core.ToHeaders ListQualificationTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListQualificationTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListQualificationTypes where
  toJSON ListQualificationTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Query" Core..=) Prelude.<$> query,
            ("MustBeOwnedByCaller" Core..=)
              Prelude.<$> mustBeOwnedByCaller,
            Prelude.Just
              ("MustBeRequestable" Core..= mustBeRequestable)
          ]
      )

instance Core.ToPath ListQualificationTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListQualificationTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListQualificationTypesResponse' smart constructor.
data ListQualificationTypesResponse = ListQualificationTypesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of QualificationType elements returned by the query.
    qualificationTypes :: Prelude.Maybe [QualificationType],
    -- | The number of Qualification types on this page in the filtered results
    -- list, equivalent to the number of types this operation returns.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQualificationTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQualificationTypesResponse_nextToken' - Undocumented member.
--
-- 'qualificationTypes', 'listQualificationTypesResponse_qualificationTypes' - The list of QualificationType elements returned by the query.
--
-- 'numResults', 'listQualificationTypesResponse_numResults' - The number of Qualification types on this page in the filtered results
-- list, equivalent to the number of types this operation returns.
--
-- 'httpStatus', 'listQualificationTypesResponse_httpStatus' - The response's http status code.
newListQualificationTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQualificationTypesResponse
newListQualificationTypesResponse pHttpStatus_ =
  ListQualificationTypesResponse'
    { nextToken =
        Prelude.Nothing,
      qualificationTypes = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listQualificationTypesResponse_nextToken :: Lens.Lens' ListQualificationTypesResponse (Prelude.Maybe Prelude.Text)
listQualificationTypesResponse_nextToken = Lens.lens (\ListQualificationTypesResponse' {nextToken} -> nextToken) (\s@ListQualificationTypesResponse' {} a -> s {nextToken = a} :: ListQualificationTypesResponse)

-- | The list of QualificationType elements returned by the query.
listQualificationTypesResponse_qualificationTypes :: Lens.Lens' ListQualificationTypesResponse (Prelude.Maybe [QualificationType])
listQualificationTypesResponse_qualificationTypes = Lens.lens (\ListQualificationTypesResponse' {qualificationTypes} -> qualificationTypes) (\s@ListQualificationTypesResponse' {} a -> s {qualificationTypes = a} :: ListQualificationTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The number of Qualification types on this page in the filtered results
-- list, equivalent to the number of types this operation returns.
listQualificationTypesResponse_numResults :: Lens.Lens' ListQualificationTypesResponse (Prelude.Maybe Prelude.Int)
listQualificationTypesResponse_numResults = Lens.lens (\ListQualificationTypesResponse' {numResults} -> numResults) (\s@ListQualificationTypesResponse' {} a -> s {numResults = a} :: ListQualificationTypesResponse)

-- | The response's http status code.
listQualificationTypesResponse_httpStatus :: Lens.Lens' ListQualificationTypesResponse Prelude.Int
listQualificationTypesResponse_httpStatus = Lens.lens (\ListQualificationTypesResponse' {httpStatus} -> httpStatus) (\s@ListQualificationTypesResponse' {} a -> s {httpStatus = a} :: ListQualificationTypesResponse)

instance
  Prelude.NFData
    ListQualificationTypesResponse
