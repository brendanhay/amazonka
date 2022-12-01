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
-- Module      : Amazonka.Rekognition.ListProjectPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the project policies attached to a project.
--
-- To attach a project policy to a project, call PutProjectPolicy. To
-- remove a project policy from a project, call DeleteProjectPolicy.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.ListProjectPolicies
  ( -- * Creating a Request
    ListProjectPolicies (..),
    newListProjectPolicies,

    -- * Request Lenses
    listProjectPolicies_nextToken,
    listProjectPolicies_maxResults,
    listProjectPolicies_projectArn,

    -- * Destructuring the Response
    ListProjectPoliciesResponse (..),
    newListProjectPoliciesResponse,

    -- * Response Lenses
    listProjectPoliciesResponse_projectPolicies,
    listProjectPoliciesResponse_nextToken,
    listProjectPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProjectPolicies' smart constructor.
data ListProjectPolicies = ListProjectPolicies'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 5. If you specify a value greater than 5, a
    -- ValidationException error occurs. The default value is 5.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the project for which you want to list the project policies.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectPolicies_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'maxResults', 'listProjectPolicies_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 5. If you specify a value greater than 5, a
-- ValidationException error occurs. The default value is 5.
--
-- 'projectArn', 'listProjectPolicies_projectArn' - The ARN of the project for which you want to list the project policies.
newListProjectPolicies ::
  -- | 'projectArn'
  Prelude.Text ->
  ListProjectPolicies
newListProjectPolicies pProjectArn_ =
  ListProjectPolicies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      projectArn = pProjectArn_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
listProjectPolicies_nextToken :: Lens.Lens' ListProjectPolicies (Prelude.Maybe Prelude.Text)
listProjectPolicies_nextToken = Lens.lens (\ListProjectPolicies' {nextToken} -> nextToken) (\s@ListProjectPolicies' {} a -> s {nextToken = a} :: ListProjectPolicies)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 5. If you specify a value greater than 5, a
-- ValidationException error occurs. The default value is 5.
listProjectPolicies_maxResults :: Lens.Lens' ListProjectPolicies (Prelude.Maybe Prelude.Natural)
listProjectPolicies_maxResults = Lens.lens (\ListProjectPolicies' {maxResults} -> maxResults) (\s@ListProjectPolicies' {} a -> s {maxResults = a} :: ListProjectPolicies)

-- | The ARN of the project for which you want to list the project policies.
listProjectPolicies_projectArn :: Lens.Lens' ListProjectPolicies Prelude.Text
listProjectPolicies_projectArn = Lens.lens (\ListProjectPolicies' {projectArn} -> projectArn) (\s@ListProjectPolicies' {} a -> s {projectArn = a} :: ListProjectPolicies)

instance Core.AWSPager ListProjectPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProjectPoliciesResponse_projectPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProjectPolicies_nextToken
          Lens..~ rs
          Lens.^? listProjectPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProjectPolicies where
  type
    AWSResponse ListProjectPolicies =
      ListProjectPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectPoliciesResponse'
            Prelude.<$> ( x Core..?> "ProjectPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProjectPolicies where
  hashWithSalt _salt ListProjectPolicies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ListProjectPolicies where
  rnf ListProjectPolicies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf projectArn

instance Core.ToHeaders ListProjectPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListProjectPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProjectPolicies where
  toJSON ListProjectPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ProjectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath ListProjectPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProjectPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProjectPoliciesResponse' smart constructor.
data ListProjectPoliciesResponse = ListProjectPoliciesResponse'
  { -- | A list of project policies attached to the project.
    projectPolicies :: Prelude.Maybe [ProjectPolicy],
    -- | If the response is truncated, Amazon Rekognition returns this token that
    -- you can use in the subsequent request to retrieve the next set of
    -- project policies.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectPolicies', 'listProjectPoliciesResponse_projectPolicies' - A list of project policies attached to the project.
--
-- 'nextToken', 'listProjectPoliciesResponse_nextToken' - If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of
-- project policies.
--
-- 'httpStatus', 'listProjectPoliciesResponse_httpStatus' - The response's http status code.
newListProjectPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProjectPoliciesResponse
newListProjectPoliciesResponse pHttpStatus_ =
  ListProjectPoliciesResponse'
    { projectPolicies =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of project policies attached to the project.
listProjectPoliciesResponse_projectPolicies :: Lens.Lens' ListProjectPoliciesResponse (Prelude.Maybe [ProjectPolicy])
listProjectPoliciesResponse_projectPolicies = Lens.lens (\ListProjectPoliciesResponse' {projectPolicies} -> projectPolicies) (\s@ListProjectPoliciesResponse' {} a -> s {projectPolicies = a} :: ListProjectPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of
-- project policies.
listProjectPoliciesResponse_nextToken :: Lens.Lens' ListProjectPoliciesResponse (Prelude.Maybe Prelude.Text)
listProjectPoliciesResponse_nextToken = Lens.lens (\ListProjectPoliciesResponse' {nextToken} -> nextToken) (\s@ListProjectPoliciesResponse' {} a -> s {nextToken = a} :: ListProjectPoliciesResponse)

-- | The response's http status code.
listProjectPoliciesResponse_httpStatus :: Lens.Lens' ListProjectPoliciesResponse Prelude.Int
listProjectPoliciesResponse_httpStatus = Lens.lens (\ListProjectPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListProjectPoliciesResponse' {} a -> s {httpStatus = a} :: ListProjectPoliciesResponse)

instance Prelude.NFData ListProjectPoliciesResponse where
  rnf ListProjectPoliciesResponse' {..} =
    Prelude.rnf projectPolicies
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
