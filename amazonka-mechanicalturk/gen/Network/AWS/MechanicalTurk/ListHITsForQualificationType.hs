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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHITsForQualificationType' smart constructor.
data ListHITsForQualificationType = ListHITsForQualificationType'
  { -- | Pagination Token
    nextToken :: Core.Maybe Core.Text,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the Qualification type to use when querying HITs.
    qualificationTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListHITsForQualificationType
newListHITsForQualificationType pQualificationTypeId_ =
  ListHITsForQualificationType'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | Pagination Token
listHITsForQualificationType_nextToken :: Lens.Lens' ListHITsForQualificationType (Core.Maybe Core.Text)
listHITsForQualificationType_nextToken = Lens.lens (\ListHITsForQualificationType' {nextToken} -> nextToken) (\s@ListHITsForQualificationType' {} a -> s {nextToken = a} :: ListHITsForQualificationType)

-- | Limit the number of results returned.
listHITsForQualificationType_maxResults :: Lens.Lens' ListHITsForQualificationType (Core.Maybe Core.Natural)
listHITsForQualificationType_maxResults = Lens.lens (\ListHITsForQualificationType' {maxResults} -> maxResults) (\s@ListHITsForQualificationType' {} a -> s {maxResults = a} :: ListHITsForQualificationType)

-- | The ID of the Qualification type to use when querying HITs.
listHITsForQualificationType_qualificationTypeId :: Lens.Lens' ListHITsForQualificationType Core.Text
listHITsForQualificationType_qualificationTypeId = Lens.lens (\ListHITsForQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@ListHITsForQualificationType' {} a -> s {qualificationTypeId = a} :: ListHITsForQualificationType)

instance Core.AWSPager ListHITsForQualificationType where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listHITsForQualificationTypeResponse_hITs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHITsForQualificationType_nextToken
          Lens..~ rs
          Lens.^? listHITsForQualificationTypeResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListHITsForQualificationType where
  type
    AWSResponse ListHITsForQualificationType =
      ListHITsForQualificationTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsForQualificationTypeResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "HITs" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListHITsForQualificationType

instance Core.NFData ListHITsForQualificationType

instance Core.ToHeaders ListHITsForQualificationType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListHITsForQualificationType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListHITsForQualificationType where
  toJSON ListHITsForQualificationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance Core.ToPath ListHITsForQualificationType where
  toPath = Core.const "/"

instance Core.ToQuery ListHITsForQualificationType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListHITsForQualificationTypeResponse' smart constructor.
data ListHITsForQualificationTypeResponse = ListHITsForQualificationTypeResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The list of HIT elements returned by the query.
    hITs :: Core.Maybe [HIT],
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListHITsForQualificationTypeResponse
newListHITsForQualificationTypeResponse pHttpStatus_ =
  ListHITsForQualificationTypeResponse'
    { nextToken =
        Core.Nothing,
      hITs = Core.Nothing,
      numResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listHITsForQualificationTypeResponse_nextToken :: Lens.Lens' ListHITsForQualificationTypeResponse (Core.Maybe Core.Text)
listHITsForQualificationTypeResponse_nextToken = Lens.lens (\ListHITsForQualificationTypeResponse' {nextToken} -> nextToken) (\s@ListHITsForQualificationTypeResponse' {} a -> s {nextToken = a} :: ListHITsForQualificationTypeResponse)

-- | The list of HIT elements returned by the query.
listHITsForQualificationTypeResponse_hITs :: Lens.Lens' ListHITsForQualificationTypeResponse (Core.Maybe [HIT])
listHITsForQualificationTypeResponse_hITs = Lens.lens (\ListHITsForQualificationTypeResponse' {hITs} -> hITs) (\s@ListHITsForQualificationTypeResponse' {} a -> s {hITs = a} :: ListHITsForQualificationTypeResponse) Core.. Lens.mapping Lens._Coerce

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listHITsForQualificationTypeResponse_numResults :: Lens.Lens' ListHITsForQualificationTypeResponse (Core.Maybe Core.Int)
listHITsForQualificationTypeResponse_numResults = Lens.lens (\ListHITsForQualificationTypeResponse' {numResults} -> numResults) (\s@ListHITsForQualificationTypeResponse' {} a -> s {numResults = a} :: ListHITsForQualificationTypeResponse)

-- | The response's http status code.
listHITsForQualificationTypeResponse_httpStatus :: Lens.Lens' ListHITsForQualificationTypeResponse Core.Int
listHITsForQualificationTypeResponse_httpStatus = Lens.lens (\ListHITsForQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@ListHITsForQualificationTypeResponse' {} a -> s {httpStatus = a} :: ListHITsForQualificationTypeResponse)

instance
  Core.NFData
    ListHITsForQualificationTypeResponse
