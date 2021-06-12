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
-- Module      : Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersWithQualificationType@ operation returns all of the
-- Workers that have been associated with a given Qualification type.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
  ( -- * Creating a Request
    ListWorkersWithQualificationType (..),
    newListWorkersWithQualificationType,

    -- * Request Lenses
    listWorkersWithQualificationType_status,
    listWorkersWithQualificationType_nextToken,
    listWorkersWithQualificationType_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListWorkersWithQualificationType' smart constructor.
data ListWorkersWithQualificationType = ListWorkersWithQualificationType'
  { -- | The status of the Qualifications to return. Can be @Granted | Revoked@.
    status :: Core.Maybe QualificationStatus,
    -- | Pagination Token
    nextToken :: Core.Maybe Core.Text,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the Qualification type of the Qualifications to return.
    qualificationTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkersWithQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listWorkersWithQualificationType_status' - The status of the Qualifications to return. Can be @Granted | Revoked@.
--
-- 'nextToken', 'listWorkersWithQualificationType_nextToken' - Pagination Token
--
-- 'maxResults', 'listWorkersWithQualificationType_maxResults' - Limit the number of results returned.
--
-- 'qualificationTypeId', 'listWorkersWithQualificationType_qualificationTypeId' - The ID of the Qualification type of the Qualifications to return.
newListWorkersWithQualificationType ::
  -- | 'qualificationTypeId'
  Core.Text ->
  ListWorkersWithQualificationType
newListWorkersWithQualificationType
  pQualificationTypeId_ =
    ListWorkersWithQualificationType'
      { status =
          Core.Nothing,
        nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        qualificationTypeId =
          pQualificationTypeId_
      }

-- | The status of the Qualifications to return. Can be @Granted | Revoked@.
listWorkersWithQualificationType_status :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe QualificationStatus)
listWorkersWithQualificationType_status = Lens.lens (\ListWorkersWithQualificationType' {status} -> status) (\s@ListWorkersWithQualificationType' {} a -> s {status = a} :: ListWorkersWithQualificationType)

-- | Pagination Token
listWorkersWithQualificationType_nextToken :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe Core.Text)
listWorkersWithQualificationType_nextToken = Lens.lens (\ListWorkersWithQualificationType' {nextToken} -> nextToken) (\s@ListWorkersWithQualificationType' {} a -> s {nextToken = a} :: ListWorkersWithQualificationType)

-- | Limit the number of results returned.
listWorkersWithQualificationType_maxResults :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe Core.Natural)
listWorkersWithQualificationType_maxResults = Lens.lens (\ListWorkersWithQualificationType' {maxResults} -> maxResults) (\s@ListWorkersWithQualificationType' {} a -> s {maxResults = a} :: ListWorkersWithQualificationType)

-- | The ID of the Qualification type of the Qualifications to return.
listWorkersWithQualificationType_qualificationTypeId :: Lens.Lens' ListWorkersWithQualificationType Core.Text
listWorkersWithQualificationType_qualificationTypeId = Lens.lens (\ListWorkersWithQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@ListWorkersWithQualificationType' {} a -> s {qualificationTypeId = a} :: ListWorkersWithQualificationType)

instance
  Core.AWSPager
    ListWorkersWithQualificationType
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkersWithQualificationTypeResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkersWithQualificationTypeResponse_qualifications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listWorkersWithQualificationType_nextToken
          Lens..~ rs
          Lens.^? listWorkersWithQualificationTypeResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListWorkersWithQualificationType
  where
  type
    AWSResponse ListWorkersWithQualificationType =
      ListWorkersWithQualificationTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkersWithQualificationTypeResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "NumResults")
            Core.<*> (x Core..?> "Qualifications" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListWorkersWithQualificationType

instance Core.NFData ListWorkersWithQualificationType

instance
  Core.ToHeaders
    ListWorkersWithQualificationType
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListWorkersWithQualificationType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListWorkersWithQualificationType where
  toJSON ListWorkersWithQualificationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance Core.ToPath ListWorkersWithQualificationType where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListWorkersWithQualificationType
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListWorkersWithQualificationTypeResponse' smart constructor.
data ListWorkersWithQualificationTypeResponse = ListWorkersWithQualificationTypeResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The number of Qualifications on this page in the filtered results list,
    -- equivalent to the number of Qualifications being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The list of Qualification elements returned by this call.
    qualifications :: Core.Maybe [Qualification],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListWorkersWithQualificationTypeResponse
newListWorkersWithQualificationTypeResponse
  pHttpStatus_ =
    ListWorkersWithQualificationTypeResponse'
      { nextToken =
          Core.Nothing,
        numResults = Core.Nothing,
        qualifications = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
listWorkersWithQualificationTypeResponse_nextToken :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe Core.Text)
listWorkersWithQualificationTypeResponse_nextToken = Lens.lens (\ListWorkersWithQualificationTypeResponse' {nextToken} -> nextToken) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {nextToken = a} :: ListWorkersWithQualificationTypeResponse)

-- | The number of Qualifications on this page in the filtered results list,
-- equivalent to the number of Qualifications being returned by this call.
listWorkersWithQualificationTypeResponse_numResults :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe Core.Int)
listWorkersWithQualificationTypeResponse_numResults = Lens.lens (\ListWorkersWithQualificationTypeResponse' {numResults} -> numResults) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {numResults = a} :: ListWorkersWithQualificationTypeResponse)

-- | The list of Qualification elements returned by this call.
listWorkersWithQualificationTypeResponse_qualifications :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe [Qualification])
listWorkersWithQualificationTypeResponse_qualifications = Lens.lens (\ListWorkersWithQualificationTypeResponse' {qualifications} -> qualifications) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {qualifications = a} :: ListWorkersWithQualificationTypeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listWorkersWithQualificationTypeResponse_httpStatus :: Lens.Lens' ListWorkersWithQualificationTypeResponse Core.Int
listWorkersWithQualificationTypeResponse_httpStatus = Lens.lens (\ListWorkersWithQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@ListWorkersWithQualificationTypeResponse' {} a -> s {httpStatus = a} :: ListWorkersWithQualificationTypeResponse)

instance
  Core.NFData
    ListWorkersWithQualificationTypeResponse
