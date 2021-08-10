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
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of artifacts created during the session.
module Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
  ( -- * Creating a Request
    ListTestGridSessionArtifacts (..),
    newListTestGridSessionArtifacts,

    -- * Request Lenses
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_sessionArn,

    -- * Destructuring the Response
    ListTestGridSessionArtifactsResponse (..),
    newListTestGridSessionArtifactsResponse,

    -- * Response Lenses
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTestGridSessionArtifacts' smart constructor.
data ListTestGridSessionArtifacts = ListTestGridSessionArtifacts'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned by a request.
    maxResult :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listTestGridSessionArtifacts_nextToken' - Pagination token.
--
-- 'maxResult', 'listTestGridSessionArtifacts_maxResult' - The maximum number of results to be returned by a request.
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
    { nextToken =
        Prelude.Nothing,
      maxResult = Prelude.Nothing,
      type' = Prelude.Nothing,
      sessionArn = pSessionArn_
    }

-- | Pagination token.
listTestGridSessionArtifacts_nextToken :: Lens.Lens' ListTestGridSessionArtifacts (Prelude.Maybe Prelude.Text)
listTestGridSessionArtifacts_nextToken = Lens.lens (\ListTestGridSessionArtifacts' {nextToken} -> nextToken) (\s@ListTestGridSessionArtifacts' {} a -> s {nextToken = a} :: ListTestGridSessionArtifacts)

-- | The maximum number of results to be returned by a request.
listTestGridSessionArtifacts_maxResult :: Lens.Lens' ListTestGridSessionArtifacts (Prelude.Maybe Prelude.Natural)
listTestGridSessionArtifacts_maxResult = Lens.lens (\ListTestGridSessionArtifacts' {maxResult} -> maxResult) (\s@ListTestGridSessionArtifacts' {} a -> s {maxResult = a} :: ListTestGridSessionArtifacts)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionArtifactsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "artifacts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTestGridSessionArtifacts

instance Prelude.NFData ListTestGridSessionArtifacts

instance Core.ToHeaders ListTestGridSessionArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridSessionArtifacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTestGridSessionArtifacts where
  toJSON ListTestGridSessionArtifacts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResult" Core..=) Prelude.<$> maxResult,
            ("type" Core..=) Prelude.<$> type',
            Prelude.Just ("sessionArn" Core..= sessionArn)
          ]
      )

instance Core.ToPath ListTestGridSessionArtifacts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTestGridSessionArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestGridSessionArtifactsResponse' smart constructor.
data ListTestGridSessionArtifactsResponse = ListTestGridSessionArtifactsResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of test grid session artifacts for a TestGridSession.
    artifacts :: Prelude.Maybe [TestGridSessionArtifact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridSessionArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridSessionArtifactsResponse_nextToken' - Pagination token.
--
-- 'artifacts', 'listTestGridSessionArtifactsResponse_artifacts' - A list of test grid session artifacts for a TestGridSession.
--
-- 'httpStatus', 'listTestGridSessionArtifactsResponse_httpStatus' - The response's http status code.
newListTestGridSessionArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestGridSessionArtifactsResponse
newListTestGridSessionArtifactsResponse pHttpStatus_ =
  ListTestGridSessionArtifactsResponse'
    { nextToken =
        Prelude.Nothing,
      artifacts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listTestGridSessionArtifactsResponse_nextToken :: Lens.Lens' ListTestGridSessionArtifactsResponse (Prelude.Maybe Prelude.Text)
listTestGridSessionArtifactsResponse_nextToken = Lens.lens (\ListTestGridSessionArtifactsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionArtifactsResponse)

-- | A list of test grid session artifacts for a TestGridSession.
listTestGridSessionArtifactsResponse_artifacts :: Lens.Lens' ListTestGridSessionArtifactsResponse (Prelude.Maybe [TestGridSessionArtifact])
listTestGridSessionArtifactsResponse_artifacts = Lens.lens (\ListTestGridSessionArtifactsResponse' {artifacts} -> artifacts) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {artifacts = a} :: ListTestGridSessionArtifactsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestGridSessionArtifactsResponse_httpStatus :: Lens.Lens' ListTestGridSessionArtifactsResponse Prelude.Int
listTestGridSessionArtifactsResponse_httpStatus = Lens.lens (\ListTestGridSessionArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionArtifactsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionArtifactsResponse)

instance
  Prelude.NFData
    ListTestGridSessionArtifactsResponse
