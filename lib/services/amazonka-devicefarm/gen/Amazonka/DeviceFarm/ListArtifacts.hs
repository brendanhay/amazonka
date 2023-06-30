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
-- Module      : Amazonka.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListArtifacts
  ( -- * Creating a Request
    ListArtifacts (..),
    newListArtifacts,

    -- * Request Lenses
    listArtifacts_nextToken,
    listArtifacts_arn,
    listArtifacts_type,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_artifacts,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the list artifacts operation.
--
-- /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The run, job, suite, or test ARN.
    arn :: Prelude.Text,
    -- | The artifacts\' type.
    --
    -- Allowed values include:
    --
    -- -   FILE
    --
    -- -   LOG
    --
    -- -   SCREENSHOT
    type' :: ArtifactCategory
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifacts_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listArtifacts_arn' - The run, job, suite, or test ARN.
--
-- 'type'', 'listArtifacts_type' - The artifacts\' type.
--
-- Allowed values include:
--
-- -   FILE
--
-- -   LOG
--
-- -   SCREENSHOT
newListArtifacts ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'type''
  ArtifactCategory ->
  ListArtifacts
newListArtifacts pArn_ pType_ =
  ListArtifacts'
    { nextToken = Prelude.Nothing,
      arn = pArn_,
      type' = pType_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | The run, job, suite, or test ARN.
listArtifacts_arn :: Lens.Lens' ListArtifacts Prelude.Text
listArtifacts_arn = Lens.lens (\ListArtifacts' {arn} -> arn) (\s@ListArtifacts' {} a -> s {arn = a} :: ListArtifacts)

-- | The artifacts\' type.
--
-- Allowed values include:
--
-- -   FILE
--
-- -   LOG
--
-- -   SCREENSHOT
listArtifacts_type :: Lens.Lens' ListArtifacts ArtifactCategory
listArtifacts_type = Lens.lens (\ListArtifacts' {type'} -> type') (\s@ListArtifacts' {} a -> s {type' = a} :: ListArtifacts)

instance Core.AWSPager ListArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_artifacts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListArtifacts where
  type
    AWSResponse ListArtifacts =
      ListArtifactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Prelude.<$> (x Data..?> "artifacts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListArtifacts where
  hashWithSalt _salt ListArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListArtifacts where
  rnf ListArtifacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListArtifacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Data..= arn),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath ListArtifacts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list artifacts operation.
--
-- /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | Information about the artifacts.
    artifacts :: Prelude.Maybe [Artifact],
    -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifacts', 'listArtifactsResponse_artifacts' - Information about the artifacts.
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { artifacts = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the artifacts.
listArtifactsResponse_artifacts :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe [Artifact])
listArtifactsResponse_artifacts = Lens.lens (\ListArtifactsResponse' {artifacts} -> artifacts) (\s@ListArtifactsResponse' {} a -> s {artifacts = a} :: ListArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe Prelude.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Prelude.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Prelude.NFData ListArtifactsResponse where
  rnf ListArtifactsResponse' {..} =
    Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
