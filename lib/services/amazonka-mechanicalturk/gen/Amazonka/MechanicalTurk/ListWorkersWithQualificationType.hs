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
-- Module      : Amazonka.MechanicalTurk.ListWorkersWithQualificationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersWithQualificationType@ operation returns all of the
-- Workers that have been associated with a given Qualification type.
--
-- This operation returns paginated results.
module Amazonka.MechanicalTurk.ListWorkersWithQualificationType
  ( -- * Creating a Request
    ListWorkersWithQualificationType (..),
    newListWorkersWithQualificationType,

    -- * Request Lenses
    listWorkersWithQualificationType_maxResults,
    listWorkersWithQualificationType_nextToken,
    listWorkersWithQualificationType_status,
    listWorkersWithQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    ListWorkersWithQualificationTypeResponse (..),
    newListWorkersWithQualificationTypeResponse,

    -- * Response Lenses
    listWorkersWithQualificationTypeResponse_nextToken,
    listWorkersWithQualificationTypeResponse_numResults,
    listWorkersWithQualificationTypeResponse_qualifications,
    listWorkersWithQualificationTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkersWithQualificationType' smart constructor.
data ListWorkersWithQualificationType = ListWorkersWithQualificationType'
  { -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination Token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the Qualifications to return. Can be @Granted | Revoked@.
    status :: Prelude.Maybe QualificationStatus,
    -- | The ID of the Qualification type of the Qualifications to return.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkersWithQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkersWithQualificationType_maxResults' - Limit the number of results returned.
--
-- 'nextToken', 'listWorkersWithQualificationType_nextToken' - Pagination Token
--
-- 'status', 'listWorkersWithQualificationType_status' - The status of the Qualifications to return. Can be @Granted | Revoked@.
--
-- 'qualificationTypeId', 'listWorkersWithQualificationType_qualificationTypeId' - The ID of the Qualification type of the Qualifications to return.
newListWorkersWithQualificationType ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  ListWorkersWithQualificationType
newListWorkersWithQualificationType
  pQualificationTypeId_ =
    ListWorkersWithQualificationType'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        status = Prelude.Nothing,
        qualificationTypeId =
          pQualificationTypeId_
      }

-- | Limit the number of results returned.
listWorkersWithQualificationType_maxResults :: Lens.Lens' ListWorkersWithQualificationType (Prelude.Maybe Prelude.Natural)
listWorkersWithQualificationType_maxResults = Lens.lens (\ListWorkersWithQualificationType' {maxResults} -> maxResults) (\s@ListWorkersWithQualificationType' {} a -> s {maxResults = a} :: ListWorkersWithQualificationType)

-- | Pagination Token
listWorkersWithQualificationType_nextToken :: Lens.Lens' ListWorkersWithQualificationType (Prelude.Maybe Prelude.Text)
listWorkersWithQualificationType_nextToken = Lens.lens (\ListWorkersWithQualificationType' {nextToken} -> nextToken) (\s@ListWorkersWithQualificationType' {} a -> s {nextToken = a} :: ListWorkersWithQualificationType)

-- | The status of the Qualifications to return. Can be @Granted | Revoked@.
listWorkersWithQualificationType_status :: Lens.Lens' ListWorkersWithQualificationType (Prelude.Maybe QualificationStatus)
listWorkersWithQualificationType_status = Lens.lens (\ListWorkersWithQualificationType' {status} -> status) (\s@ListWorkersWithQualificationType' {} a -> s {status = a} :: ListWorkersWithQualificationType)

-- | The ID of the Qualification type of the Qualifications to return.
listWorkersWithQualificationType_qualificationTypeId :: Lens.Lens' ListWorkersWithQualificationType Prelude.Text
listWorkersWithQualificationType_qualificationTypeId = Lens.lens (\ListWorkersWithQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@ListWorkersWithQualificationType' {} a -> s {qualificationTypeId = a} :: ListWorkersWithQualificationType)

instance
  Core.AWSPager
    ListWorkersWithQualificationType
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkersWithQualificationTypeResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkersWithQualificationTypeResponse_qualifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkersWithQualificationType_nextToken
          Lens..~ rs
          Lens.^? listWorkersWithQualificationTypeResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListWorkersWithQualificationType
  where
  type
    AWSResponse ListWorkersWithQualificationType =
      ListWorkersWithQualificationTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkersWithQualificationTypeResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "NumResults")
            Prelude.<*> (x Data..?> "Qualifications" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListWorkersWithQualificationType
  where
  hashWithSalt
    _salt
    ListWorkersWithQualificationType' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` qualificationTypeId

instance
  Prelude.NFData
    ListWorkersWithQualificationType
  where
  rnf ListWorkersWithQualificationType' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf qualificationTypeId

instance
  Data.ToHeaders
    ListWorkersWithQualificationType
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.ListWorkersWithQualificationType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorkersWithQualificationType where
  toJSON ListWorkersWithQualificationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId)
          ]
      )

instance Data.ToPath ListWorkersWithQualificationType where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListWorkersWithQualificationType
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkersWithQualificationTypeResponse' smart constructor.
data ListWorkersWithQualificationTypeResponse = ListWorkersWithQualificationTypeResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of Qualifications on this page in the filtered results list,
    -- equivalent to the number of Qualifications being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The list of Qualification elements returned by this call.
    qualifications :: Prelude.Maybe [Qualification],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkersWithQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkersWithQualificationTypeResponse_nextToken' - Undocumented member.
--
-- 'numResults', 'listWorkersWithQualificationTypeResponse_numResults' - The number of Qualifications on this page in the filtered results list,
-- equivalent to the number of Qualifications being returned by this call.
--
-- 'qualifications', 'listWorkersWithQualificationTypeResponse_qualifications' - The list of Qualification elements returned by this call.
--
-- 'httpStatus', 'listWorkersWithQualificationTypeResponse_httpStatus' - The response's http status code.
newListWorkersWithQualificationTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkersWithQualificationTypeResponse
newListWorkersWithQualificationTypeResponse
  pHttpStatus_ =
    ListWorkersWithQualificationTypeResponse'
      { nextToken =
          Prelude.Nothing,
        numResults = Prelude.Nothing,
        qualifications = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
listWorkersWithQualificationTypeResponse_nextToken :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Prelude.Maybe Prelude.Text)
listWorkersWithQualificationTypeResponse_nextToken = Lens.lens (\ListWorkersWithQualificationTypeResponse' {nextToken} -> nextToken) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {nextToken = a} :: ListWorkersWithQualificationTypeResponse)

-- | The number of Qualifications on this page in the filtered results list,
-- equivalent to the number of Qualifications being returned by this call.
listWorkersWithQualificationTypeResponse_numResults :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Prelude.Maybe Prelude.Int)
listWorkersWithQualificationTypeResponse_numResults = Lens.lens (\ListWorkersWithQualificationTypeResponse' {numResults} -> numResults) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {numResults = a} :: ListWorkersWithQualificationTypeResponse)

-- | The list of Qualification elements returned by this call.
listWorkersWithQualificationTypeResponse_qualifications :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Prelude.Maybe [Qualification])
listWorkersWithQualificationTypeResponse_qualifications = Lens.lens (\ListWorkersWithQualificationTypeResponse' {qualifications} -> qualifications) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {qualifications = a} :: ListWorkersWithQualificationTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkersWithQualificationTypeResponse_httpStatus :: Lens.Lens' ListWorkersWithQualificationTypeResponse Prelude.Int
listWorkersWithQualificationTypeResponse_httpStatus = Lens.lens (\ListWorkersWithQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {httpStatus = a} :: ListWorkersWithQualificationTypeResponse)

instance
  Prelude.NFData
    ListWorkersWithQualificationTypeResponse
  where
  rnf ListWorkersWithQualificationTypeResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf qualifications
      `Prelude.seq` Prelude.rnf httpStatus
