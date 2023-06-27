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
-- Module      : Amazonka.AuditManager.ListAssessmentFrameworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the frameworks that are available in the Audit Manager
-- framework library.
module Amazonka.AuditManager.ListAssessmentFrameworks
  ( -- * Creating a Request
    ListAssessmentFrameworks (..),
    newListAssessmentFrameworks,

    -- * Request Lenses
    listAssessmentFrameworks_maxResults,
    listAssessmentFrameworks_nextToken,
    listAssessmentFrameworks_frameworkType,

    -- * Destructuring the Response
    ListAssessmentFrameworksResponse (..),
    newListAssessmentFrameworksResponse,

    -- * Response Lenses
    listAssessmentFrameworksResponse_frameworkMetadataList,
    listAssessmentFrameworksResponse_nextToken,
    listAssessmentFrameworksResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentFrameworks' smart constructor.
data ListAssessmentFrameworks = ListAssessmentFrameworks'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of framework, such as a standard framework or a custom
    -- framework.
    frameworkType :: FrameworkType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentFrameworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssessmentFrameworks_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listAssessmentFrameworks_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'frameworkType', 'listAssessmentFrameworks_frameworkType' - The type of framework, such as a standard framework or a custom
-- framework.
newListAssessmentFrameworks ::
  -- | 'frameworkType'
  FrameworkType ->
  ListAssessmentFrameworks
newListAssessmentFrameworks pFrameworkType_ =
  ListAssessmentFrameworks'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      frameworkType = pFrameworkType_
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listAssessmentFrameworks_maxResults :: Lens.Lens' ListAssessmentFrameworks (Prelude.Maybe Prelude.Natural)
listAssessmentFrameworks_maxResults = Lens.lens (\ListAssessmentFrameworks' {maxResults} -> maxResults) (\s@ListAssessmentFrameworks' {} a -> s {maxResults = a} :: ListAssessmentFrameworks)

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentFrameworks_nextToken :: Lens.Lens' ListAssessmentFrameworks (Prelude.Maybe Prelude.Text)
listAssessmentFrameworks_nextToken = Lens.lens (\ListAssessmentFrameworks' {nextToken} -> nextToken) (\s@ListAssessmentFrameworks' {} a -> s {nextToken = a} :: ListAssessmentFrameworks)

-- | The type of framework, such as a standard framework or a custom
-- framework.
listAssessmentFrameworks_frameworkType :: Lens.Lens' ListAssessmentFrameworks FrameworkType
listAssessmentFrameworks_frameworkType = Lens.lens (\ListAssessmentFrameworks' {frameworkType} -> frameworkType) (\s@ListAssessmentFrameworks' {} a -> s {frameworkType = a} :: ListAssessmentFrameworks)

instance Core.AWSRequest ListAssessmentFrameworks where
  type
    AWSResponse ListAssessmentFrameworks =
      ListAssessmentFrameworksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentFrameworksResponse'
            Prelude.<$> ( x
                            Data..?> "frameworkMetadataList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssessmentFrameworks where
  hashWithSalt _salt ListAssessmentFrameworks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` frameworkType

instance Prelude.NFData ListAssessmentFrameworks where
  rnf ListAssessmentFrameworks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf frameworkType

instance Data.ToHeaders ListAssessmentFrameworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssessmentFrameworks where
  toPath = Prelude.const "/assessmentFrameworks"

instance Data.ToQuery ListAssessmentFrameworks where
  toQuery ListAssessmentFrameworks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "frameworkType" Data.=: frameworkType
      ]

-- | /See:/ 'newListAssessmentFrameworksResponse' smart constructor.
data ListAssessmentFrameworksResponse = ListAssessmentFrameworksResponse'
  { -- | A list of metadata that the @ListAssessmentFrameworks@ API returns for
    -- each framework.
    frameworkMetadataList :: Prelude.Maybe [AssessmentFrameworkMetadata],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentFrameworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkMetadataList', 'listAssessmentFrameworksResponse_frameworkMetadataList' - A list of metadata that the @ListAssessmentFrameworks@ API returns for
-- each framework.
--
-- 'nextToken', 'listAssessmentFrameworksResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listAssessmentFrameworksResponse_httpStatus' - The response's http status code.
newListAssessmentFrameworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentFrameworksResponse
newListAssessmentFrameworksResponse pHttpStatus_ =
  ListAssessmentFrameworksResponse'
    { frameworkMetadataList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of metadata that the @ListAssessmentFrameworks@ API returns for
-- each framework.
listAssessmentFrameworksResponse_frameworkMetadataList :: Lens.Lens' ListAssessmentFrameworksResponse (Prelude.Maybe [AssessmentFrameworkMetadata])
listAssessmentFrameworksResponse_frameworkMetadataList = Lens.lens (\ListAssessmentFrameworksResponse' {frameworkMetadataList} -> frameworkMetadataList) (\s@ListAssessmentFrameworksResponse' {} a -> s {frameworkMetadataList = a} :: ListAssessmentFrameworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentFrameworksResponse_nextToken :: Lens.Lens' ListAssessmentFrameworksResponse (Prelude.Maybe Prelude.Text)
listAssessmentFrameworksResponse_nextToken = Lens.lens (\ListAssessmentFrameworksResponse' {nextToken} -> nextToken) (\s@ListAssessmentFrameworksResponse' {} a -> s {nextToken = a} :: ListAssessmentFrameworksResponse)

-- | The response's http status code.
listAssessmentFrameworksResponse_httpStatus :: Lens.Lens' ListAssessmentFrameworksResponse Prelude.Int
listAssessmentFrameworksResponse_httpStatus = Lens.lens (\ListAssessmentFrameworksResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentFrameworksResponse' {} a -> s {httpStatus = a} :: ListAssessmentFrameworksResponse)

instance
  Prelude.NFData
    ListAssessmentFrameworksResponse
  where
  rnf ListAssessmentFrameworksResponse' {..} =
    Prelude.rnf frameworkMetadataList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
