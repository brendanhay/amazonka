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
-- Module      : Amazonka.MechanicalTurk.ListHITsForQualificationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITsForQualificationType@ operation returns the HITs that use
-- the given Qualification type for a Qualification requirement. The
-- operation returns HITs of any status, except for HITs that have been
-- deleted with the @DeleteHIT@ operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Amazonka.MechanicalTurk.ListHITsForQualificationType
  ( -- * Creating a Request
    ListHITsForQualificationType (..),
    newListHITsForQualificationType,

    -- * Request Lenses
    listHITsForQualificationType_maxResults,
    listHITsForQualificationType_nextToken,
    listHITsForQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    ListHITsForQualificationTypeResponse (..),
    newListHITsForQualificationTypeResponse,

    -- * Response Lenses
    listHITsForQualificationTypeResponse_hITs,
    listHITsForQualificationTypeResponse_nextToken,
    listHITsForQualificationTypeResponse_numResults,
    listHITsForQualificationTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHITsForQualificationType' smart constructor.
data ListHITsForQualificationType = ListHITsForQualificationType'
  { -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination Token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Qualification type to use when querying HITs.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHITsForQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listHITsForQualificationType_maxResults' - Limit the number of results returned.
--
-- 'nextToken', 'listHITsForQualificationType_nextToken' - Pagination Token
--
-- 'qualificationTypeId', 'listHITsForQualificationType_qualificationTypeId' - The ID of the Qualification type to use when querying HITs.
newListHITsForQualificationType ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  ListHITsForQualificationType
newListHITsForQualificationType pQualificationTypeId_ =
  ListHITsForQualificationType'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | Limit the number of results returned.
listHITsForQualificationType_maxResults :: Lens.Lens' ListHITsForQualificationType (Prelude.Maybe Prelude.Natural)
listHITsForQualificationType_maxResults = Lens.lens (\ListHITsForQualificationType' {maxResults} -> maxResults) (\s@ListHITsForQualificationType' {} a -> s {maxResults = a} :: ListHITsForQualificationType)

-- | Pagination Token
listHITsForQualificationType_nextToken :: Lens.Lens' ListHITsForQualificationType (Prelude.Maybe Prelude.Text)
listHITsForQualificationType_nextToken = Lens.lens (\ListHITsForQualificationType' {nextToken} -> nextToken) (\s@ListHITsForQualificationType' {} a -> s {nextToken = a} :: ListHITsForQualificationType)

-- | The ID of the Qualification type to use when querying HITs.
listHITsForQualificationType_qualificationTypeId :: Lens.Lens' ListHITsForQualificationType Prelude.Text
listHITsForQualificationType_qualificationTypeId = Lens.lens (\ListHITsForQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@ListHITsForQualificationType' {} a -> s {qualificationTypeId = a} :: ListHITsForQualificationType)

instance Core.AWSPager ListHITsForQualificationType where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_hITs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listHITsForQualificationType_nextToken
          Lens..~ rs
          Lens.^? listHITsForQualificationTypeResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListHITsForQualificationType where
  type
    AWSResponse ListHITsForQualificationType =
      ListHITsForQualificationTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsForQualificationTypeResponse'
            Prelude.<$> (x Data..?> "HITs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListHITsForQualificationType
  where
  hashWithSalt _salt ListHITsForQualificationType' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` qualificationTypeId

instance Prelude.NFData ListHITsForQualificationType where
  rnf ListHITsForQualificationType' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf qualificationTypeId

instance Data.ToHeaders ListHITsForQualificationType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.ListHITsForQualificationType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHITsForQualificationType where
  toJSON ListHITsForQualificationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId)
          ]
      )

instance Data.ToPath ListHITsForQualificationType where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHITsForQualificationType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHITsForQualificationTypeResponse' smart constructor.
data ListHITsForQualificationTypeResponse = ListHITsForQualificationTypeResponse'
  { -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHITsForQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITs', 'listHITsForQualificationTypeResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'nextToken', 'listHITsForQualificationTypeResponse_nextToken' - Undocumented member.
--
-- 'numResults', 'listHITsForQualificationTypeResponse_numResults' - The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
--
-- 'httpStatus', 'listHITsForQualificationTypeResponse_httpStatus' - The response's http status code.
newListHITsForQualificationTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHITsForQualificationTypeResponse
newListHITsForQualificationTypeResponse pHttpStatus_ =
  ListHITsForQualificationTypeResponse'
    { hITs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of HIT elements returned by the query.
listHITsForQualificationTypeResponse_hITs :: Lens.Lens' ListHITsForQualificationTypeResponse (Prelude.Maybe [HIT])
listHITsForQualificationTypeResponse_hITs = Lens.lens (\ListHITsForQualificationTypeResponse' {hITs} -> hITs) (\s@ListHITsForQualificationTypeResponse' {} a -> s {hITs = a} :: ListHITsForQualificationTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listHITsForQualificationTypeResponse_nextToken :: Lens.Lens' ListHITsForQualificationTypeResponse (Prelude.Maybe Prelude.Text)
listHITsForQualificationTypeResponse_nextToken = Lens.lens (\ListHITsForQualificationTypeResponse' {nextToken} -> nextToken) (\s@ListHITsForQualificationTypeResponse' {} a -> s {nextToken = a} :: ListHITsForQualificationTypeResponse)

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listHITsForQualificationTypeResponse_numResults :: Lens.Lens' ListHITsForQualificationTypeResponse (Prelude.Maybe Prelude.Int)
listHITsForQualificationTypeResponse_numResults = Lens.lens (\ListHITsForQualificationTypeResponse' {numResults} -> numResults) (\s@ListHITsForQualificationTypeResponse' {} a -> s {numResults = a} :: ListHITsForQualificationTypeResponse)

-- | The response's http status code.
listHITsForQualificationTypeResponse_httpStatus :: Lens.Lens' ListHITsForQualificationTypeResponse Prelude.Int
listHITsForQualificationTypeResponse_httpStatus = Lens.lens (\ListHITsForQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@ListHITsForQualificationTypeResponse' {} a -> s {httpStatus = a} :: ListHITsForQualificationTypeResponse)

instance
  Prelude.NFData
    ListHITsForQualificationTypeResponse
  where
  rnf ListHITsForQualificationTypeResponse' {..} =
    Prelude.rnf hITs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf httpStatus
