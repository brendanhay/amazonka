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
-- Module      : Amazonka.ChimeSdkMediaPipelines.CreateMediaLiveConnectorPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming media pipeline in an Amazon Chime SDK meeting.
module Amazonka.ChimeSdkMediaPipelines.CreateMediaLiveConnectorPipeline
  ( -- * Creating a Request
    CreateMediaLiveConnectorPipeline (..),
    newCreateMediaLiveConnectorPipeline,

    -- * Request Lenses
    createMediaLiveConnectorPipeline_tags,
    createMediaLiveConnectorPipeline_clientRequestToken,
    createMediaLiveConnectorPipeline_sources,
    createMediaLiveConnectorPipeline_sinks,

    -- * Destructuring the Response
    CreateMediaLiveConnectorPipelineResponse (..),
    newCreateMediaLiveConnectorPipelineResponse,

    -- * Response Lenses
    createMediaLiveConnectorPipelineResponse_mediaLiveConnectorPipeline,
    createMediaLiveConnectorPipelineResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMediaLiveConnectorPipeline' smart constructor.
data CreateMediaLiveConnectorPipeline = CreateMediaLiveConnectorPipeline'
  { -- | The tags associated with the media pipeline.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The token assigned to the client making the request.
    clientRequestToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The media pipeline\'s data sources.
    sources :: Prelude.NonEmpty LiveConnectorSourceConfiguration,
    -- | The media pipeline\'s data sinks.
    sinks :: Prelude.NonEmpty LiveConnectorSinkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaLiveConnectorPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMediaLiveConnectorPipeline_tags' - The tags associated with the media pipeline.
--
-- 'clientRequestToken', 'createMediaLiveConnectorPipeline_clientRequestToken' - The token assigned to the client making the request.
--
-- 'sources', 'createMediaLiveConnectorPipeline_sources' - The media pipeline\'s data sources.
--
-- 'sinks', 'createMediaLiveConnectorPipeline_sinks' - The media pipeline\'s data sinks.
newCreateMediaLiveConnectorPipeline ::
  -- | 'sources'
  Prelude.NonEmpty LiveConnectorSourceConfiguration ->
  -- | 'sinks'
  Prelude.NonEmpty LiveConnectorSinkConfiguration ->
  CreateMediaLiveConnectorPipeline
newCreateMediaLiveConnectorPipeline pSources_ pSinks_ =
  CreateMediaLiveConnectorPipeline'
    { tags =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      sources = Lens.coerced Lens.# pSources_,
      sinks = Lens.coerced Lens.# pSinks_
    }

-- | The tags associated with the media pipeline.
createMediaLiveConnectorPipeline_tags :: Lens.Lens' CreateMediaLiveConnectorPipeline (Prelude.Maybe (Prelude.NonEmpty Tag))
createMediaLiveConnectorPipeline_tags = Lens.lens (\CreateMediaLiveConnectorPipeline' {tags} -> tags) (\s@CreateMediaLiveConnectorPipeline' {} a -> s {tags = a} :: CreateMediaLiveConnectorPipeline) Prelude.. Lens.mapping Lens.coerced

-- | The token assigned to the client making the request.
createMediaLiveConnectorPipeline_clientRequestToken :: Lens.Lens' CreateMediaLiveConnectorPipeline (Prelude.Maybe Prelude.Text)
createMediaLiveConnectorPipeline_clientRequestToken = Lens.lens (\CreateMediaLiveConnectorPipeline' {clientRequestToken} -> clientRequestToken) (\s@CreateMediaLiveConnectorPipeline' {} a -> s {clientRequestToken = a} :: CreateMediaLiveConnectorPipeline) Prelude.. Lens.mapping Core._Sensitive

-- | The media pipeline\'s data sources.
createMediaLiveConnectorPipeline_sources :: Lens.Lens' CreateMediaLiveConnectorPipeline (Prelude.NonEmpty LiveConnectorSourceConfiguration)
createMediaLiveConnectorPipeline_sources = Lens.lens (\CreateMediaLiveConnectorPipeline' {sources} -> sources) (\s@CreateMediaLiveConnectorPipeline' {} a -> s {sources = a} :: CreateMediaLiveConnectorPipeline) Prelude.. Lens.coerced

-- | The media pipeline\'s data sinks.
createMediaLiveConnectorPipeline_sinks :: Lens.Lens' CreateMediaLiveConnectorPipeline (Prelude.NonEmpty LiveConnectorSinkConfiguration)
createMediaLiveConnectorPipeline_sinks = Lens.lens (\CreateMediaLiveConnectorPipeline' {sinks} -> sinks) (\s@CreateMediaLiveConnectorPipeline' {} a -> s {sinks = a} :: CreateMediaLiveConnectorPipeline) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateMediaLiveConnectorPipeline
  where
  type
    AWSResponse CreateMediaLiveConnectorPipeline =
      CreateMediaLiveConnectorPipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMediaLiveConnectorPipelineResponse'
            Prelude.<$> (x Core..?> "MediaLiveConnectorPipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMediaLiveConnectorPipeline
  where
  hashWithSalt
    _salt
    CreateMediaLiveConnectorPipeline' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` sources
        `Prelude.hashWithSalt` sinks

instance
  Prelude.NFData
    CreateMediaLiveConnectorPipeline
  where
  rnf CreateMediaLiveConnectorPipeline' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf sinks

instance
  Core.ToHeaders
    CreateMediaLiveConnectorPipeline
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateMediaLiveConnectorPipeline where
  toJSON CreateMediaLiveConnectorPipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Sources" Core..= sources),
            Prelude.Just ("Sinks" Core..= sinks)
          ]
      )

instance Core.ToPath CreateMediaLiveConnectorPipeline where
  toPath =
    Prelude.const "/sdk-media-live-connector-pipelines"

instance
  Core.ToQuery
    CreateMediaLiveConnectorPipeline
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMediaLiveConnectorPipelineResponse' smart constructor.
data CreateMediaLiveConnectorPipelineResponse = CreateMediaLiveConnectorPipelineResponse'
  { -- | The new media pipeline.
    mediaLiveConnectorPipeline :: Prelude.Maybe MediaLiveConnectorPipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaLiveConnectorPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaLiveConnectorPipeline', 'createMediaLiveConnectorPipelineResponse_mediaLiveConnectorPipeline' - The new media pipeline.
--
-- 'httpStatus', 'createMediaLiveConnectorPipelineResponse_httpStatus' - The response's http status code.
newCreateMediaLiveConnectorPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMediaLiveConnectorPipelineResponse
newCreateMediaLiveConnectorPipelineResponse
  pHttpStatus_ =
    CreateMediaLiveConnectorPipelineResponse'
      { mediaLiveConnectorPipeline =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The new media pipeline.
createMediaLiveConnectorPipelineResponse_mediaLiveConnectorPipeline :: Lens.Lens' CreateMediaLiveConnectorPipelineResponse (Prelude.Maybe MediaLiveConnectorPipeline)
createMediaLiveConnectorPipelineResponse_mediaLiveConnectorPipeline = Lens.lens (\CreateMediaLiveConnectorPipelineResponse' {mediaLiveConnectorPipeline} -> mediaLiveConnectorPipeline) (\s@CreateMediaLiveConnectorPipelineResponse' {} a -> s {mediaLiveConnectorPipeline = a} :: CreateMediaLiveConnectorPipelineResponse)

-- | The response's http status code.
createMediaLiveConnectorPipelineResponse_httpStatus :: Lens.Lens' CreateMediaLiveConnectorPipelineResponse Prelude.Int
createMediaLiveConnectorPipelineResponse_httpStatus = Lens.lens (\CreateMediaLiveConnectorPipelineResponse' {httpStatus} -> httpStatus) (\s@CreateMediaLiveConnectorPipelineResponse' {} a -> s {httpStatus = a} :: CreateMediaLiveConnectorPipelineResponse)

instance
  Prelude.NFData
    CreateMediaLiveConnectorPipelineResponse
  where
  rnf CreateMediaLiveConnectorPipelineResponse' {..} =
    Prelude.rnf mediaLiveConnectorPipeline
      `Prelude.seq` Prelude.rnf httpStatus
