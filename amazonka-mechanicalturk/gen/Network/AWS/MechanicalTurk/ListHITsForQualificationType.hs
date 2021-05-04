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
-- Module      : Network.AWS.MechanicalTurk.ListHITsForQualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MechanicalTurk.ListHITsForQualificationType
  ( -- * Creating a Request
    ListHITsForQualificationType (..),
    newListHITsForQualificationType,

    -- * Request Lenses
    listHITsForQualificationType_nextToken,
    listHITsForQualificationType_maxResults,
    listHITsForQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    ListHITsForQualificationTypeResponse (..),
    newListHITsForQualificationTypeResponse,

    -- * Response Lenses
    listHITsForQualificationTypeResponse_nextToken,
    listHITsForQualificationTypeResponse_hITs,
    listHITsForQualificationTypeResponse_numResults,
    listHITsForQualificationTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHITsForQualificationType' smart constructor.
data ListHITsForQualificationType = ListHITsForQualificationType'
  { -- | Pagination Token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Qualification type to use when querying HITs.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListHITsForQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITsForQualificationType_nextToken' - Pagination Token
--
-- 'maxResults', 'listHITsForQualificationType_maxResults' - Limit the number of results returned.
--
-- 'qualificationTypeId', 'listHITsForQualificationType_qualificationTypeId' - The ID of the Qualification type to use when querying HITs.
newListHITsForQualificationType ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  ListHITsForQualificationType
newListHITsForQualificationType pQualificationTypeId_ =
  ListHITsForQualificationType'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | Pagination Token
listHITsForQualificationType_nextToken :: Lens.Lens' ListHITsForQualificationType (Prelude.Maybe Prelude.Text)
listHITsForQualificationType_nextToken = Lens.lens (\ListHITsForQualificationType' {nextToken} -> nextToken) (\s@ListHITsForQualificationType' {} a -> s {nextToken = a} :: ListHITsForQualificationType)

-- | Limit the number of results returned.
listHITsForQualificationType_maxResults :: Lens.Lens' ListHITsForQualificationType (Prelude.Maybe Prelude.Natural)
listHITsForQualificationType_maxResults = Lens.lens (\ListHITsForQualificationType' {maxResults} -> maxResults) (\s@ListHITsForQualificationType' {} a -> s {maxResults = a} :: ListHITsForQualificationType)

-- | The ID of the Qualification type to use when querying HITs.
listHITsForQualificationType_qualificationTypeId :: Lens.Lens' ListHITsForQualificationType Prelude.Text
listHITsForQualificationType_qualificationTypeId = Lens.lens (\ListHITsForQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@ListHITsForQualificationType' {} a -> s {qualificationTypeId = a} :: ListHITsForQualificationType)

instance Pager.AWSPager ListHITsForQualificationType where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_hITs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listHITsForQualificationType_nextToken
          Lens..~ rs
          Lens.^? listHITsForQualificationTypeResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListHITsForQualificationType
  where
  type
    Rs ListHITsForQualificationType =
      ListHITsForQualificationTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsForQualificationTypeResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "HITs" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListHITsForQualificationType

instance Prelude.NFData ListHITsForQualificationType

instance
  Prelude.ToHeaders
    ListHITsForQualificationType
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.ListHITsForQualificationType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListHITsForQualificationType where
  toJSON ListHITsForQualificationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "QualificationTypeId"
                  Prelude..= qualificationTypeId
              )
          ]
      )

instance Prelude.ToPath ListHITsForQualificationType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListHITsForQualificationType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHITsForQualificationTypeResponse' smart constructor.
data ListHITsForQualificationTypeResponse = ListHITsForQualificationTypeResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListHITsForQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITsForQualificationTypeResponse_nextToken' - Undocumented member.
--
-- 'hITs', 'listHITsForQualificationTypeResponse_hITs' - The list of HIT elements returned by the query.
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
    { nextToken =
        Prelude.Nothing,
      hITs = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listHITsForQualificationTypeResponse_nextToken :: Lens.Lens' ListHITsForQualificationTypeResponse (Prelude.Maybe Prelude.Text)
listHITsForQualificationTypeResponse_nextToken = Lens.lens (\ListHITsForQualificationTypeResponse' {nextToken} -> nextToken) (\s@ListHITsForQualificationTypeResponse' {} a -> s {nextToken = a} :: ListHITsForQualificationTypeResponse)

-- | The list of HIT elements returned by the query.
listHITsForQualificationTypeResponse_hITs :: Lens.Lens' ListHITsForQualificationTypeResponse (Prelude.Maybe [HIT])
listHITsForQualificationTypeResponse_hITs = Lens.lens (\ListHITsForQualificationTypeResponse' {hITs} -> hITs) (\s@ListHITsForQualificationTypeResponse' {} a -> s {hITs = a} :: ListHITsForQualificationTypeResponse) Prelude.. Lens.mapping Prelude._Coerce

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
