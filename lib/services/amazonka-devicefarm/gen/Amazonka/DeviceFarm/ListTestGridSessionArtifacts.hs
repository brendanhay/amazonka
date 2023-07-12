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
-- Module      : Amazonka.DeviceFarm.ListTestGridSessionArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of artifacts created during the session.
module Amazonka.DeviceFarm.ListTestGridSessionArtifacts
  ( -- * Creating a Request
    ListTestGridSessionArtifacts (..),
    newListTestGridSessionArtifacts,

    -- * Request Lenses
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_sessionArn,

    -- * Destructuring the Response
    ListTestGridSessionArtifactsResponse (..),
    newListTestGridSessionArtifactsResponse,

    -- * Response Lenses
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestGridSessionArtifacts' smart constructor.
data ListTestGridSessionArtifacts = ListTestGridSessionArtifacts'
  { -- | The maximum number of results to be returned by a request.
    maxResult :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit results to a specified type of artifact.
    type' :: Prelude.Maybe TestGridSessionArtifactCategory,
    -- | The ARN of a TestGridSession.
    sessionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridSessionArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResult', 'listTestGridSessionArtifacts_maxResult' - The maximum number of results to be returned by a request.
--
-- 'nextToken', 'listTestGridSessionArtifacts_nextToken' - Pagination token.
--
-- 'type'', 'listTestGridSessionArtifacts_type' - Limit results to a specified type of artifact.
--
-- 'sessionArn', 'listTestGridSessionArtifacts_sessionArn' - The ARN of a TestGridSession.
newListTestGridSessionArtifacts ::
  -- | 'sessionArn'
  Prelude.Text ->
  ListTestGridSessionArtifacts
newListTestGridSessionArtifacts pSessionArn_ =
  ListTestGridSessionArtifacts'
    { maxResult =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      sessionArn = pSessionArn_
    }

-- | The maximum number of results to be returned by a request.
listTestGridSessionArtifacts_maxResult :: Lens.Lens' ListTestGridSessionArtifacts (Prelude.Maybe Prelude.Natural)
listTestGridSessionArtifacts_maxResult = Lens.lens (\ListTestGridSessionArtifacts' {maxResult} -> maxResult) (\s@ListTestGridSessionArtifacts' {} a -> s {maxResult = a} :: ListTestGridSessionArtifacts)

-- | Pagination token.
listTestGridSessionArtifacts_nextToken :: Lens.Lens' ListTestGridSessionArtifacts (Prelude.Maybe Prelude.Text)
listTestGridSessionArtifacts_nextToken = Lens.lens (\ListTestGridSessionArtifacts' {nextToken} -> nextToken) (\s@ListTestGridSessionArtifacts' {} a -> s {nextToken = a} :: ListTestGridSessionArtifacts)

-- | Limit results to a specified type of artifact.
listTestGridSessionArtifacts_type :: Lens.Lens' ListTestGridSessionArtifacts (Prelude.Maybe TestGridSessionArtifactCategory)
listTestGridSessionArtifacts_type = Lens.lens (\ListTestGridSessionArtifacts' {type'} -> type') (\s@ListTestGridSessionArtifacts' {} a -> s {type' = a} :: ListTestGridSessionArtifacts)

-- | The ARN of a TestGridSession.
listTestGridSessionArtifacts_sessionArn :: Lens.Lens' ListTestGridSessionArtifacts Prelude.Text
listTestGridSessionArtifacts_sessionArn = Lens.lens (\ListTestGridSessionArtifacts' {sessionArn} -> sessionArn) (\s@ListTestGridSessionArtifacts' {} a -> s {sessionArn = a} :: ListTestGridSessionArtifacts)

instance Core.AWSRequest ListTestGridSessionArtifacts where
  type
    AWSResponse ListTestGridSessionArtifacts =
      ListTestGridSessionArtifactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionArtifactsResponse'
            Prelude.<$> (x Data..?> "artifacts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTestGridSessionArtifacts
  where
  hashWithSalt _salt ListTestGridSessionArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResult
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` sessionArn

instance Prelude.NFData ListTestGridSessionArtifacts where
  rnf ListTestGridSessionArtifacts' {..} =
    Prelude.rnf maxResult
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf sessionArn

instance Data.ToHeaders ListTestGridSessionArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListTestGridSessionArtifacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestGridSessionArtifacts where
  toJSON ListTestGridSessionArtifacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResult" Data..=) Prelude.<$> maxResult,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("sessionArn" Data..= sessionArn)
          ]
      )

instance Data.ToPath ListTestGridSessionArtifacts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTestGridSessionArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestGridSessionArtifactsResponse' smart constructor.
data ListTestGridSessionArtifactsResponse = ListTestGridSessionArtifactsResponse'
  { -- | A list of test grid session artifacts for a TestGridSession.
    artifacts :: Prelude.Maybe [TestGridSessionArtifact],
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridSessionArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifacts', 'listTestGridSessionArtifactsResponse_artifacts' - A list of test grid session artifacts for a TestGridSession.
--
-- 'nextToken', 'listTestGridSessionArtifactsResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'listTestGridSessionArtifactsResponse_httpStatus' - The response's http status code.
newListTestGridSessionArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestGridSessionArtifactsResponse
newListTestGridSessionArtifactsResponse pHttpStatus_ =
  ListTestGridSessionArtifactsResponse'
    { artifacts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of test grid session artifacts for a TestGridSession.
listTestGridSessionArtifactsResponse_artifacts :: Lens.Lens' ListTestGridSessionArtifactsResponse (Prelude.Maybe [TestGridSessionArtifact])
listTestGridSessionArtifactsResponse_artifacts = Lens.lens (\ListTestGridSessionArtifactsResponse' {artifacts} -> artifacts) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {artifacts = a} :: ListTestGridSessionArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token.
listTestGridSessionArtifactsResponse_nextToken :: Lens.Lens' ListTestGridSessionArtifactsResponse (Prelude.Maybe Prelude.Text)
listTestGridSessionArtifactsResponse_nextToken = Lens.lens (\ListTestGridSessionArtifactsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionArtifactsResponse)

-- | The response's http status code.
listTestGridSessionArtifactsResponse_httpStatus :: Lens.Lens' ListTestGridSessionArtifactsResponse Prelude.Int
listTestGridSessionArtifactsResponse_httpStatus = Lens.lens (\ListTestGridSessionArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionArtifactsResponse)

instance
  Prelude.NFData
    ListTestGridSessionArtifactsResponse
  where
  rnf ListTestGridSessionArtifactsResponse' {..} =
    Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
