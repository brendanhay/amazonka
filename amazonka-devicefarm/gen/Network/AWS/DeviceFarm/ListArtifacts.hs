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
-- Module      : Network.AWS.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListArtifacts
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
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifacts,
    listArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list artifacts operation.
--
-- /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The run, job, suite, or test ARN.
    arn :: Core.Text,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'type''
  ArtifactCategory ->
  ListArtifacts
newListArtifacts pArn_ pType_ =
  ListArtifacts'
    { nextToken = Core.Nothing,
      arn = pArn_,
      type' = pType_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Core.Maybe Core.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | The run, job, suite, or test ARN.
listArtifacts_arn :: Lens.Lens' ListArtifacts Core.Text
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
            Lens.^? listArtifactsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_artifacts Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListArtifacts where
  type
    AWSResponse ListArtifacts =
      ListArtifactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "artifacts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListArtifacts

instance Core.NFData ListArtifacts

instance Core.ToHeaders ListArtifacts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListArtifacts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn),
            Core.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath ListArtifacts where
  toPath = Core.const "/"

instance Core.ToQuery ListArtifacts where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list artifacts operation.
--
-- /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the artifacts.
    artifacts :: Core.Maybe [Artifact],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'artifacts', 'listArtifactsResponse_artifacts' - Information about the artifacts.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { nextToken = Core.Nothing,
      artifacts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Core.Maybe Core.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | Information about the artifacts.
listArtifactsResponse_artifacts :: Lens.Lens' ListArtifactsResponse (Core.Maybe [Artifact])
listArtifactsResponse_artifacts = Lens.lens (\ListArtifactsResponse' {artifacts} -> artifacts) (\s@ListArtifactsResponse' {} a -> s {artifacts = a} :: ListArtifactsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Core.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Core.NFData ListArtifactsResponse
