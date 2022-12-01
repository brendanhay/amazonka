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
-- Module      : Amazonka.Kendra.ListExperiences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more Amazon Kendra experiences. You can create an Amazon
-- Kendra experience such as a search application. For more information on
-- creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.ListExperiences
  ( -- * Creating a Request
    ListExperiences (..),
    newListExperiences,

    -- * Request Lenses
    listExperiences_nextToken,
    listExperiences_maxResults,
    listExperiences_indexId,

    -- * Destructuring the Response
    ListExperiencesResponse (..),
    newListExperiencesResponse,

    -- * Response Lenses
    listExperiencesResponse_nextToken,
    listExperiencesResponse_summaryItems,
    listExperiencesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExperiences' smart constructor.
data ListExperiences = ListExperiences'
  { -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of Amazon Kendra
    -- experiences.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned Amazon Kendra experiences.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperiences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperiences_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of Amazon Kendra
-- experiences.
--
-- 'maxResults', 'listExperiences_maxResults' - The maximum number of returned Amazon Kendra experiences.
--
-- 'indexId', 'listExperiences_indexId' - The identifier of the index for your Amazon Kendra experience.
newListExperiences ::
  -- | 'indexId'
  Prelude.Text ->
  ListExperiences
newListExperiences pIndexId_ =
  ListExperiences'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of Amazon Kendra
-- experiences.
listExperiences_nextToken :: Lens.Lens' ListExperiences (Prelude.Maybe Prelude.Text)
listExperiences_nextToken = Lens.lens (\ListExperiences' {nextToken} -> nextToken) (\s@ListExperiences' {} a -> s {nextToken = a} :: ListExperiences)

-- | The maximum number of returned Amazon Kendra experiences.
listExperiences_maxResults :: Lens.Lens' ListExperiences (Prelude.Maybe Prelude.Natural)
listExperiences_maxResults = Lens.lens (\ListExperiences' {maxResults} -> maxResults) (\s@ListExperiences' {} a -> s {maxResults = a} :: ListExperiences)

-- | The identifier of the index for your Amazon Kendra experience.
listExperiences_indexId :: Lens.Lens' ListExperiences Prelude.Text
listExperiences_indexId = Lens.lens (\ListExperiences' {indexId} -> indexId) (\s@ListExperiences' {} a -> s {indexId = a} :: ListExperiences)

instance Core.AWSRequest ListExperiences where
  type
    AWSResponse ListExperiences =
      ListExperiencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperiencesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperiences where
  hashWithSalt _salt ListExperiences' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListExperiences where
  rnf ListExperiences' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf indexId

instance Core.ToHeaders ListExperiences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.ListExperiences" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListExperiences where
  toJSON ListExperiences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath ListExperiences where
  toPath = Prelude.const "/"

instance Core.ToQuery ListExperiences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExperiencesResponse' smart constructor.
data ListExperiencesResponse = ListExperiencesResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token, which
    -- you can use in a later request to retrieve the next set of Amazon Kendra
    -- experiences.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more Amazon Kendra
    -- experiences.
    summaryItems :: Prelude.Maybe [ExperiencesSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperiencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperiencesResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of Amazon Kendra
-- experiences.
--
-- 'summaryItems', 'listExperiencesResponse_summaryItems' - An array of summary information for one or more Amazon Kendra
-- experiences.
--
-- 'httpStatus', 'listExperiencesResponse_httpStatus' - The response's http status code.
newListExperiencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExperiencesResponse
newListExperiencesResponse pHttpStatus_ =
  ListExperiencesResponse'
    { nextToken =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of Amazon Kendra
-- experiences.
listExperiencesResponse_nextToken :: Lens.Lens' ListExperiencesResponse (Prelude.Maybe Prelude.Text)
listExperiencesResponse_nextToken = Lens.lens (\ListExperiencesResponse' {nextToken} -> nextToken) (\s@ListExperiencesResponse' {} a -> s {nextToken = a} :: ListExperiencesResponse)

-- | An array of summary information for one or more Amazon Kendra
-- experiences.
listExperiencesResponse_summaryItems :: Lens.Lens' ListExperiencesResponse (Prelude.Maybe [ExperiencesSummary])
listExperiencesResponse_summaryItems = Lens.lens (\ListExperiencesResponse' {summaryItems} -> summaryItems) (\s@ListExperiencesResponse' {} a -> s {summaryItems = a} :: ListExperiencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listExperiencesResponse_httpStatus :: Lens.Lens' ListExperiencesResponse Prelude.Int
listExperiencesResponse_httpStatus = Lens.lens (\ListExperiencesResponse' {httpStatus} -> httpStatus) (\s@ListExperiencesResponse' {} a -> s {httpStatus = a} :: ListExperiencesResponse)

instance Prelude.NFData ListExperiencesResponse where
  rnf ListExperiencesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaryItems
      `Prelude.seq` Prelude.rnf httpStatus
