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
-- Module      : Amazonka.ImageBuilder.ListComponentBuildVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of component build versions for the specified semantic
-- version.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
module Amazonka.ImageBuilder.ListComponentBuildVersions
  ( -- * Creating a Request
    ListComponentBuildVersions (..),
    newListComponentBuildVersions,

    -- * Request Lenses
    listComponentBuildVersions_maxResults,
    listComponentBuildVersions_nextToken,
    listComponentBuildVersions_componentVersionArn,

    -- * Destructuring the Response
    ListComponentBuildVersionsResponse (..),
    newListComponentBuildVersionsResponse,

    -- * Response Lenses
    listComponentBuildVersionsResponse_componentSummaryList,
    listComponentBuildVersionsResponse_nextToken,
    listComponentBuildVersionsResponse_requestId,
    listComponentBuildVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponentBuildVersions' smart constructor.
data ListComponentBuildVersions = ListComponentBuildVersions'
  { -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The component version Amazon Resource Name (ARN) whose versions you want
    -- to list.
    componentVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentBuildVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listComponentBuildVersions_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listComponentBuildVersions_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'componentVersionArn', 'listComponentBuildVersions_componentVersionArn' - The component version Amazon Resource Name (ARN) whose versions you want
-- to list.
newListComponentBuildVersions ::
  -- | 'componentVersionArn'
  Prelude.Text ->
  ListComponentBuildVersions
newListComponentBuildVersions pComponentVersionArn_ =
  ListComponentBuildVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      componentVersionArn = pComponentVersionArn_
    }

-- | The maximum items to return in a request.
listComponentBuildVersions_maxResults :: Lens.Lens' ListComponentBuildVersions (Prelude.Maybe Prelude.Natural)
listComponentBuildVersions_maxResults = Lens.lens (\ListComponentBuildVersions' {maxResults} -> maxResults) (\s@ListComponentBuildVersions' {} a -> s {maxResults = a} :: ListComponentBuildVersions)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listComponentBuildVersions_nextToken :: Lens.Lens' ListComponentBuildVersions (Prelude.Maybe Prelude.Text)
listComponentBuildVersions_nextToken = Lens.lens (\ListComponentBuildVersions' {nextToken} -> nextToken) (\s@ListComponentBuildVersions' {} a -> s {nextToken = a} :: ListComponentBuildVersions)

-- | The component version Amazon Resource Name (ARN) whose versions you want
-- to list.
listComponentBuildVersions_componentVersionArn :: Lens.Lens' ListComponentBuildVersions Prelude.Text
listComponentBuildVersions_componentVersionArn = Lens.lens (\ListComponentBuildVersions' {componentVersionArn} -> componentVersionArn) (\s@ListComponentBuildVersions' {} a -> s {componentVersionArn = a} :: ListComponentBuildVersions)

instance Core.AWSRequest ListComponentBuildVersions where
  type
    AWSResponse ListComponentBuildVersions =
      ListComponentBuildVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentBuildVersionsResponse'
            Prelude.<$> ( x Data..?> "componentSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComponentBuildVersions where
  hashWithSalt _salt ListComponentBuildVersions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` componentVersionArn

instance Prelude.NFData ListComponentBuildVersions where
  rnf ListComponentBuildVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf componentVersionArn

instance Data.ToHeaders ListComponentBuildVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListComponentBuildVersions where
  toJSON ListComponentBuildVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("componentVersionArn" Data..= componentVersionArn)
          ]
      )

instance Data.ToPath ListComponentBuildVersions where
  toPath = Prelude.const "/ListComponentBuildVersions"

instance Data.ToQuery ListComponentBuildVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentBuildVersionsResponse' smart constructor.
data ListComponentBuildVersionsResponse = ListComponentBuildVersionsResponse'
  { -- | The list of component summaries for the specified semantic version.
    componentSummaryList :: Prelude.Maybe [ComponentSummary],
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentBuildVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentSummaryList', 'listComponentBuildVersionsResponse_componentSummaryList' - The list of component summaries for the specified semantic version.
--
-- 'nextToken', 'listComponentBuildVersionsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'requestId', 'listComponentBuildVersionsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listComponentBuildVersionsResponse_httpStatus' - The response's http status code.
newListComponentBuildVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentBuildVersionsResponse
newListComponentBuildVersionsResponse pHttpStatus_ =
  ListComponentBuildVersionsResponse'
    { componentSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of component summaries for the specified semantic version.
listComponentBuildVersionsResponse_componentSummaryList :: Lens.Lens' ListComponentBuildVersionsResponse (Prelude.Maybe [ComponentSummary])
listComponentBuildVersionsResponse_componentSummaryList = Lens.lens (\ListComponentBuildVersionsResponse' {componentSummaryList} -> componentSummaryList) (\s@ListComponentBuildVersionsResponse' {} a -> s {componentSummaryList = a} :: ListComponentBuildVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listComponentBuildVersionsResponse_nextToken :: Lens.Lens' ListComponentBuildVersionsResponse (Prelude.Maybe Prelude.Text)
listComponentBuildVersionsResponse_nextToken = Lens.lens (\ListComponentBuildVersionsResponse' {nextToken} -> nextToken) (\s@ListComponentBuildVersionsResponse' {} a -> s {nextToken = a} :: ListComponentBuildVersionsResponse)

-- | The request ID that uniquely identifies this request.
listComponentBuildVersionsResponse_requestId :: Lens.Lens' ListComponentBuildVersionsResponse (Prelude.Maybe Prelude.Text)
listComponentBuildVersionsResponse_requestId = Lens.lens (\ListComponentBuildVersionsResponse' {requestId} -> requestId) (\s@ListComponentBuildVersionsResponse' {} a -> s {requestId = a} :: ListComponentBuildVersionsResponse)

-- | The response's http status code.
listComponentBuildVersionsResponse_httpStatus :: Lens.Lens' ListComponentBuildVersionsResponse Prelude.Int
listComponentBuildVersionsResponse_httpStatus = Lens.lens (\ListComponentBuildVersionsResponse' {httpStatus} -> httpStatus) (\s@ListComponentBuildVersionsResponse' {} a -> s {httpStatus = a} :: ListComponentBuildVersionsResponse)

instance
  Prelude.NFData
    ListComponentBuildVersionsResponse
  where
  rnf ListComponentBuildVersionsResponse' {..} =
    Prelude.rnf componentSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
