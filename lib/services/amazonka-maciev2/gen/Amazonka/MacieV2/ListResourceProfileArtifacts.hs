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
-- Module      : Amazonka.MacieV2.ListResourceProfileArtifacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about objects that were selected from an S3 bucket
-- for automated sensitive data discovery.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListResourceProfileArtifacts
  ( -- * Creating a Request
    ListResourceProfileArtifacts (..),
    newListResourceProfileArtifacts,

    -- * Request Lenses
    listResourceProfileArtifacts_nextToken,
    listResourceProfileArtifacts_resourceArn,

    -- * Destructuring the Response
    ListResourceProfileArtifactsResponse (..),
    newListResourceProfileArtifactsResponse,

    -- * Response Lenses
    listResourceProfileArtifactsResponse_artifacts,
    listResourceProfileArtifactsResponse_nextToken,
    listResourceProfileArtifactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceProfileArtifacts' smart constructor.
data ListResourceProfileArtifacts = ListResourceProfileArtifacts'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
    -- to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceProfileArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceProfileArtifacts_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'resourceArn', 'listResourceProfileArtifacts_resourceArn' - The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
newListResourceProfileArtifacts ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListResourceProfileArtifacts
newListResourceProfileArtifacts pResourceArn_ =
  ListResourceProfileArtifacts'
    { nextToken =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listResourceProfileArtifacts_nextToken :: Lens.Lens' ListResourceProfileArtifacts (Prelude.Maybe Prelude.Text)
listResourceProfileArtifacts_nextToken = Lens.lens (\ListResourceProfileArtifacts' {nextToken} -> nextToken) (\s@ListResourceProfileArtifacts' {} a -> s {nextToken = a} :: ListResourceProfileArtifacts)

-- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
listResourceProfileArtifacts_resourceArn :: Lens.Lens' ListResourceProfileArtifacts Prelude.Text
listResourceProfileArtifacts_resourceArn = Lens.lens (\ListResourceProfileArtifacts' {resourceArn} -> resourceArn) (\s@ListResourceProfileArtifacts' {} a -> s {resourceArn = a} :: ListResourceProfileArtifacts)

instance Core.AWSPager ListResourceProfileArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceProfileArtifactsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceProfileArtifactsResponse_artifacts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceProfileArtifacts_nextToken
          Lens..~ rs
          Lens.^? listResourceProfileArtifactsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceProfileArtifacts where
  type
    AWSResponse ListResourceProfileArtifacts =
      ListResourceProfileArtifactsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceProfileArtifactsResponse'
            Prelude.<$> (x Data..?> "artifacts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceProfileArtifacts
  where
  hashWithSalt _salt ListResourceProfileArtifacts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListResourceProfileArtifacts where
  rnf ListResourceProfileArtifacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders ListResourceProfileArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListResourceProfileArtifacts where
  toPath = Prelude.const "/resource-profiles/artifacts"

instance Data.ToQuery ListResourceProfileArtifacts where
  toQuery ListResourceProfileArtifacts' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "resourceArn" Data.=: resourceArn
      ]

-- | /See:/ 'newListResourceProfileArtifactsResponse' smart constructor.
data ListResourceProfileArtifactsResponse = ListResourceProfileArtifactsResponse'
  { -- | An array of objects, one for each S3 object that Amazon Macie selected
    -- for analysis.
    artifacts :: Prelude.Maybe [ResourceProfileArtifact],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceProfileArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifacts', 'listResourceProfileArtifactsResponse_artifacts' - An array of objects, one for each S3 object that Amazon Macie selected
-- for analysis.
--
-- 'nextToken', 'listResourceProfileArtifactsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listResourceProfileArtifactsResponse_httpStatus' - The response's http status code.
newListResourceProfileArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceProfileArtifactsResponse
newListResourceProfileArtifactsResponse pHttpStatus_ =
  ListResourceProfileArtifactsResponse'
    { artifacts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each S3 object that Amazon Macie selected
-- for analysis.
listResourceProfileArtifactsResponse_artifacts :: Lens.Lens' ListResourceProfileArtifactsResponse (Prelude.Maybe [ResourceProfileArtifact])
listResourceProfileArtifactsResponse_artifacts = Lens.lens (\ListResourceProfileArtifactsResponse' {artifacts} -> artifacts) (\s@ListResourceProfileArtifactsResponse' {} a -> s {artifacts = a} :: ListResourceProfileArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listResourceProfileArtifactsResponse_nextToken :: Lens.Lens' ListResourceProfileArtifactsResponse (Prelude.Maybe Prelude.Text)
listResourceProfileArtifactsResponse_nextToken = Lens.lens (\ListResourceProfileArtifactsResponse' {nextToken} -> nextToken) (\s@ListResourceProfileArtifactsResponse' {} a -> s {nextToken = a} :: ListResourceProfileArtifactsResponse)

-- | The response's http status code.
listResourceProfileArtifactsResponse_httpStatus :: Lens.Lens' ListResourceProfileArtifactsResponse Prelude.Int
listResourceProfileArtifactsResponse_httpStatus = Lens.lens (\ListResourceProfileArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListResourceProfileArtifactsResponse' {} a -> s {httpStatus = a} :: ListResourceProfileArtifactsResponse)

instance
  Prelude.NFData
    ListResourceProfileArtifactsResponse
  where
  rnf ListResourceProfileArtifactsResponse' {..} =
    Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
