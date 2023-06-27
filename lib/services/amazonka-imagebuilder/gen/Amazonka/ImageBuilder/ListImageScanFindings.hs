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
-- Module      : Amazonka.ImageBuilder.ListImageScanFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of image scan findings for your account.
module Amazonka.ImageBuilder.ListImageScanFindings
  ( -- * Creating a Request
    ListImageScanFindings (..),
    newListImageScanFindings,

    -- * Request Lenses
    listImageScanFindings_filters,
    listImageScanFindings_maxResults,
    listImageScanFindings_nextToken,

    -- * Destructuring the Response
    ListImageScanFindingsResponse (..),
    newListImageScanFindingsResponse,

    -- * Response Lenses
    listImageScanFindingsResponse_findings,
    listImageScanFindingsResponse_nextToken,
    listImageScanFindingsResponse_requestId,
    listImageScanFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImageScanFindings' smart constructor.
data ListImageScanFindings = ListImageScanFindings'
  { -- | An array of name value pairs that you can use to filter your results.
    -- You can use the following filters to streamline results:
    --
    -- -   @imageBuildVersionArn@
    --
    -- -   @imagePipelineArn@
    --
    -- -   @vulnerabilityId@
    --
    -- -   @severity@
    --
    -- If you don\'t request a filter, then all findings in your account are
    -- listed.
    filters :: Prelude.Maybe (Prelude.NonEmpty ImageScanFindingsFilter),
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageScanFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listImageScanFindings_filters' - An array of name value pairs that you can use to filter your results.
-- You can use the following filters to streamline results:
--
-- -   @imageBuildVersionArn@
--
-- -   @imagePipelineArn@
--
-- -   @vulnerabilityId@
--
-- -   @severity@
--
-- If you don\'t request a filter, then all findings in your account are
-- listed.
--
-- 'maxResults', 'listImageScanFindings_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listImageScanFindings_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
newListImageScanFindings ::
  ListImageScanFindings
newListImageScanFindings =
  ListImageScanFindings'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of name value pairs that you can use to filter your results.
-- You can use the following filters to streamline results:
--
-- -   @imageBuildVersionArn@
--
-- -   @imagePipelineArn@
--
-- -   @vulnerabilityId@
--
-- -   @severity@
--
-- If you don\'t request a filter, then all findings in your account are
-- listed.
listImageScanFindings_filters :: Lens.Lens' ListImageScanFindings (Prelude.Maybe (Prelude.NonEmpty ImageScanFindingsFilter))
listImageScanFindings_filters = Lens.lens (\ListImageScanFindings' {filters} -> filters) (\s@ListImageScanFindings' {} a -> s {filters = a} :: ListImageScanFindings) Prelude.. Lens.mapping Lens.coerced

-- | The maximum items to return in a request.
listImageScanFindings_maxResults :: Lens.Lens' ListImageScanFindings (Prelude.Maybe Prelude.Natural)
listImageScanFindings_maxResults = Lens.lens (\ListImageScanFindings' {maxResults} -> maxResults) (\s@ListImageScanFindings' {} a -> s {maxResults = a} :: ListImageScanFindings)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImageScanFindings_nextToken :: Lens.Lens' ListImageScanFindings (Prelude.Maybe Prelude.Text)
listImageScanFindings_nextToken = Lens.lens (\ListImageScanFindings' {nextToken} -> nextToken) (\s@ListImageScanFindings' {} a -> s {nextToken = a} :: ListImageScanFindings)

instance Core.AWSRequest ListImageScanFindings where
  type
    AWSResponse ListImageScanFindings =
      ListImageScanFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageScanFindingsResponse'
            Prelude.<$> (x Data..?> "findings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImageScanFindings where
  hashWithSalt _salt ListImageScanFindings' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListImageScanFindings where
  rnf ListImageScanFindings' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListImageScanFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImageScanFindings where
  toJSON ListImageScanFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListImageScanFindings where
  toPath = Prelude.const "/ListImageScanFindings"

instance Data.ToQuery ListImageScanFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImageScanFindingsResponse' smart constructor.
data ListImageScanFindingsResponse = ListImageScanFindingsResponse'
  { -- | The image scan findings for your account that meet your request filter
    -- criteria.
    findings :: Prelude.Maybe [ImageScanFinding],
    -- | The next token used for paginated responses. When this field isn\'t
    -- empty, there are additional elements that the service has\'ot included
    -- in this request. Use this token with the next request to retrieve
    -- additional objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageScanFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findings', 'listImageScanFindingsResponse_findings' - The image scan findings for your account that meet your request filter
-- criteria.
--
-- 'nextToken', 'listImageScanFindingsResponse_nextToken' - The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
--
-- 'requestId', 'listImageScanFindingsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listImageScanFindingsResponse_httpStatus' - The response's http status code.
newListImageScanFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImageScanFindingsResponse
newListImageScanFindingsResponse pHttpStatus_ =
  ListImageScanFindingsResponse'
    { findings =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image scan findings for your account that meet your request filter
-- criteria.
listImageScanFindingsResponse_findings :: Lens.Lens' ListImageScanFindingsResponse (Prelude.Maybe [ImageScanFinding])
listImageScanFindingsResponse_findings = Lens.lens (\ListImageScanFindingsResponse' {findings} -> findings) (\s@ListImageScanFindingsResponse' {} a -> s {findings = a} :: ListImageScanFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
listImageScanFindingsResponse_nextToken :: Lens.Lens' ListImageScanFindingsResponse (Prelude.Maybe Prelude.Text)
listImageScanFindingsResponse_nextToken = Lens.lens (\ListImageScanFindingsResponse' {nextToken} -> nextToken) (\s@ListImageScanFindingsResponse' {} a -> s {nextToken = a} :: ListImageScanFindingsResponse)

-- | The request ID that uniquely identifies this request.
listImageScanFindingsResponse_requestId :: Lens.Lens' ListImageScanFindingsResponse (Prelude.Maybe Prelude.Text)
listImageScanFindingsResponse_requestId = Lens.lens (\ListImageScanFindingsResponse' {requestId} -> requestId) (\s@ListImageScanFindingsResponse' {} a -> s {requestId = a} :: ListImageScanFindingsResponse)

-- | The response's http status code.
listImageScanFindingsResponse_httpStatus :: Lens.Lens' ListImageScanFindingsResponse Prelude.Int
listImageScanFindingsResponse_httpStatus = Lens.lens (\ListImageScanFindingsResponse' {httpStatus} -> httpStatus) (\s@ListImageScanFindingsResponse' {} a -> s {httpStatus = a} :: ListImageScanFindingsResponse)

instance Prelude.NFData ListImageScanFindingsResponse where
  rnf ListImageScanFindingsResponse' {..} =
    Prelude.rnf findings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
