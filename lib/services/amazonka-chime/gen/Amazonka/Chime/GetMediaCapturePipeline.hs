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
-- Module      : Amazonka.Chime.GetMediaCapturePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an existing media capture pipeline.
module Amazonka.Chime.GetMediaCapturePipeline
  ( -- * Creating a Request
    GetMediaCapturePipeline (..),
    newGetMediaCapturePipeline,

    -- * Request Lenses
    getMediaCapturePipeline_mediaPipelineId,

    -- * Destructuring the Response
    GetMediaCapturePipelineResponse (..),
    newGetMediaCapturePipelineResponse,

    -- * Response Lenses
    getMediaCapturePipelineResponse_mediaCapturePipeline,
    getMediaCapturePipelineResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMediaCapturePipeline' smart constructor.
data GetMediaCapturePipeline = GetMediaCapturePipeline'
  { -- | The ID of the pipeline that you want to get.
    mediaPipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaCapturePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelineId', 'getMediaCapturePipeline_mediaPipelineId' - The ID of the pipeline that you want to get.
newGetMediaCapturePipeline ::
  -- | 'mediaPipelineId'
  Prelude.Text ->
  GetMediaCapturePipeline
newGetMediaCapturePipeline pMediaPipelineId_ =
  GetMediaCapturePipeline'
    { mediaPipelineId =
        pMediaPipelineId_
    }

-- | The ID of the pipeline that you want to get.
getMediaCapturePipeline_mediaPipelineId :: Lens.Lens' GetMediaCapturePipeline Prelude.Text
getMediaCapturePipeline_mediaPipelineId = Lens.lens (\GetMediaCapturePipeline' {mediaPipelineId} -> mediaPipelineId) (\s@GetMediaCapturePipeline' {} a -> s {mediaPipelineId = a} :: GetMediaCapturePipeline)

instance Core.AWSRequest GetMediaCapturePipeline where
  type
    AWSResponse GetMediaCapturePipeline =
      GetMediaCapturePipelineResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMediaCapturePipelineResponse'
            Prelude.<$> (x Data..?> "MediaCapturePipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMediaCapturePipeline where
  hashWithSalt _salt GetMediaCapturePipeline' {..} =
    _salt `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData GetMediaCapturePipeline where
  rnf GetMediaCapturePipeline' {..} =
    Prelude.rnf mediaPipelineId

instance Data.ToHeaders GetMediaCapturePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMediaCapturePipeline where
  toPath GetMediaCapturePipeline' {..} =
    Prelude.mconcat
      [ "/media-capture-pipelines/",
        Data.toBS mediaPipelineId
      ]

instance Data.ToQuery GetMediaCapturePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMediaCapturePipelineResponse' smart constructor.
data GetMediaCapturePipelineResponse = GetMediaCapturePipelineResponse'
  { -- | The media capture pipeline object.
    mediaCapturePipeline :: Prelude.Maybe MediaCapturePipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaCapturePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaCapturePipeline', 'getMediaCapturePipelineResponse_mediaCapturePipeline' - The media capture pipeline object.
--
-- 'httpStatus', 'getMediaCapturePipelineResponse_httpStatus' - The response's http status code.
newGetMediaCapturePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMediaCapturePipelineResponse
newGetMediaCapturePipelineResponse pHttpStatus_ =
  GetMediaCapturePipelineResponse'
    { mediaCapturePipeline =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The media capture pipeline object.
getMediaCapturePipelineResponse_mediaCapturePipeline :: Lens.Lens' GetMediaCapturePipelineResponse (Prelude.Maybe MediaCapturePipeline)
getMediaCapturePipelineResponse_mediaCapturePipeline = Lens.lens (\GetMediaCapturePipelineResponse' {mediaCapturePipeline} -> mediaCapturePipeline) (\s@GetMediaCapturePipelineResponse' {} a -> s {mediaCapturePipeline = a} :: GetMediaCapturePipelineResponse)

-- | The response's http status code.
getMediaCapturePipelineResponse_httpStatus :: Lens.Lens' GetMediaCapturePipelineResponse Prelude.Int
getMediaCapturePipelineResponse_httpStatus = Lens.lens (\GetMediaCapturePipelineResponse' {httpStatus} -> httpStatus) (\s@GetMediaCapturePipelineResponse' {} a -> s {httpStatus = a} :: GetMediaCapturePipelineResponse)

instance
  Prelude.NFData
    GetMediaCapturePipelineResponse
  where
  rnf GetMediaCapturePipelineResponse' {..} =
    Prelude.rnf mediaCapturePipeline
      `Prelude.seq` Prelude.rnf httpStatus
