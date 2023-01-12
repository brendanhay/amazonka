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
-- Module      : Amazonka.ChimeSdkMediaPipelines.GetMediaPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an existing media pipeline.
module Amazonka.ChimeSdkMediaPipelines.GetMediaPipeline
  ( -- * Creating a Request
    GetMediaPipeline (..),
    newGetMediaPipeline,

    -- * Request Lenses
    getMediaPipeline_mediaPipelineId,

    -- * Destructuring the Response
    GetMediaPipelineResponse (..),
    newGetMediaPipelineResponse,

    -- * Response Lenses
    getMediaPipelineResponse_mediaPipeline,
    getMediaPipelineResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMediaPipeline' smart constructor.
data GetMediaPipeline = GetMediaPipeline'
  { -- | The ID of the pipeline that you want to get.
    mediaPipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelineId', 'getMediaPipeline_mediaPipelineId' - The ID of the pipeline that you want to get.
newGetMediaPipeline ::
  -- | 'mediaPipelineId'
  Prelude.Text ->
  GetMediaPipeline
newGetMediaPipeline pMediaPipelineId_ =
  GetMediaPipeline'
    { mediaPipelineId =
        pMediaPipelineId_
    }

-- | The ID of the pipeline that you want to get.
getMediaPipeline_mediaPipelineId :: Lens.Lens' GetMediaPipeline Prelude.Text
getMediaPipeline_mediaPipelineId = Lens.lens (\GetMediaPipeline' {mediaPipelineId} -> mediaPipelineId) (\s@GetMediaPipeline' {} a -> s {mediaPipelineId = a} :: GetMediaPipeline)

instance Core.AWSRequest GetMediaPipeline where
  type
    AWSResponse GetMediaPipeline =
      GetMediaPipelineResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMediaPipelineResponse'
            Prelude.<$> (x Data..?> "MediaPipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMediaPipeline where
  hashWithSalt _salt GetMediaPipeline' {..} =
    _salt `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData GetMediaPipeline where
  rnf GetMediaPipeline' {..} =
    Prelude.rnf mediaPipelineId

instance Data.ToHeaders GetMediaPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMediaPipeline where
  toPath GetMediaPipeline' {..} =
    Prelude.mconcat
      ["/sdk-media-pipelines/", Data.toBS mediaPipelineId]

instance Data.ToQuery GetMediaPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMediaPipelineResponse' smart constructor.
data GetMediaPipelineResponse = GetMediaPipelineResponse'
  { -- | The media pipeline object.
    mediaPipeline :: Prelude.Maybe MediaPipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipeline', 'getMediaPipelineResponse_mediaPipeline' - The media pipeline object.
--
-- 'httpStatus', 'getMediaPipelineResponse_httpStatus' - The response's http status code.
newGetMediaPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMediaPipelineResponse
newGetMediaPipelineResponse pHttpStatus_ =
  GetMediaPipelineResponse'
    { mediaPipeline =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The media pipeline object.
getMediaPipelineResponse_mediaPipeline :: Lens.Lens' GetMediaPipelineResponse (Prelude.Maybe MediaPipeline)
getMediaPipelineResponse_mediaPipeline = Lens.lens (\GetMediaPipelineResponse' {mediaPipeline} -> mediaPipeline) (\s@GetMediaPipelineResponse' {} a -> s {mediaPipeline = a} :: GetMediaPipelineResponse)

-- | The response's http status code.
getMediaPipelineResponse_httpStatus :: Lens.Lens' GetMediaPipelineResponse Prelude.Int
getMediaPipelineResponse_httpStatus = Lens.lens (\GetMediaPipelineResponse' {httpStatus} -> httpStatus) (\s@GetMediaPipelineResponse' {} a -> s {httpStatus = a} :: GetMediaPipelineResponse)

instance Prelude.NFData GetMediaPipelineResponse where
  rnf GetMediaPipelineResponse' {..} =
    Prelude.rnf mediaPipeline
      `Prelude.seq` Prelude.rnf httpStatus
