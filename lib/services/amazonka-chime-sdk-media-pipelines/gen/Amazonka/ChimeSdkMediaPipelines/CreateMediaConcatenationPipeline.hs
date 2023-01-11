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
-- Module      : Amazonka.ChimeSdkMediaPipelines.CreateMediaConcatenationPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a media concatenation pipeline.
module Amazonka.ChimeSdkMediaPipelines.CreateMediaConcatenationPipeline
  ( -- * Creating a Request
    CreateMediaConcatenationPipeline (..),
    newCreateMediaConcatenationPipeline,

    -- * Request Lenses
    createMediaConcatenationPipeline_clientRequestToken,
    createMediaConcatenationPipeline_tags,
    createMediaConcatenationPipeline_sources,
    createMediaConcatenationPipeline_sinks,

    -- * Destructuring the Response
    CreateMediaConcatenationPipelineResponse (..),
    newCreateMediaConcatenationPipelineResponse,

    -- * Response Lenses
    createMediaConcatenationPipelineResponse_mediaConcatenationPipeline,
    createMediaConcatenationPipelineResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMediaConcatenationPipeline' smart constructor.
data CreateMediaConcatenationPipeline = CreateMediaConcatenationPipeline'
  { -- | The unique identifier for the client request. The token makes the API
    -- request idempotent. Use a unique token for each media concatenation
    -- pipeline request.
    clientRequestToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The tags associated with the media concatenation pipeline.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | An object that specifies the sources for the media concatenation
    -- pipeline.
    sources :: Prelude.NonEmpty ConcatenationSource,
    -- | An object that specifies the data sinks for the media concatenation
    -- pipeline.
    sinks :: Prelude.NonEmpty ConcatenationSink
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaConcatenationPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMediaConcatenationPipeline_clientRequestToken' - The unique identifier for the client request. The token makes the API
-- request idempotent. Use a unique token for each media concatenation
-- pipeline request.
--
-- 'tags', 'createMediaConcatenationPipeline_tags' - The tags associated with the media concatenation pipeline.
--
-- 'sources', 'createMediaConcatenationPipeline_sources' - An object that specifies the sources for the media concatenation
-- pipeline.
--
-- 'sinks', 'createMediaConcatenationPipeline_sinks' - An object that specifies the data sinks for the media concatenation
-- pipeline.
newCreateMediaConcatenationPipeline ::
  -- | 'sources'
  Prelude.NonEmpty ConcatenationSource ->
  -- | 'sinks'
  Prelude.NonEmpty ConcatenationSink ->
  CreateMediaConcatenationPipeline
newCreateMediaConcatenationPipeline pSources_ pSinks_ =
  CreateMediaConcatenationPipeline'
    { clientRequestToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      sources = Lens.coerced Lens.# pSources_,
      sinks = Lens.coerced Lens.# pSinks_
    }

-- | The unique identifier for the client request. The token makes the API
-- request idempotent. Use a unique token for each media concatenation
-- pipeline request.
createMediaConcatenationPipeline_clientRequestToken :: Lens.Lens' CreateMediaConcatenationPipeline (Prelude.Maybe Prelude.Text)
createMediaConcatenationPipeline_clientRequestToken = Lens.lens (\CreateMediaConcatenationPipeline' {clientRequestToken} -> clientRequestToken) (\s@CreateMediaConcatenationPipeline' {} a -> s {clientRequestToken = a} :: CreateMediaConcatenationPipeline) Prelude.. Lens.mapping Data._Sensitive

-- | The tags associated with the media concatenation pipeline.
createMediaConcatenationPipeline_tags :: Lens.Lens' CreateMediaConcatenationPipeline (Prelude.Maybe (Prelude.NonEmpty Tag))
createMediaConcatenationPipeline_tags = Lens.lens (\CreateMediaConcatenationPipeline' {tags} -> tags) (\s@CreateMediaConcatenationPipeline' {} a -> s {tags = a} :: CreateMediaConcatenationPipeline) Prelude.. Lens.mapping Lens.coerced

-- | An object that specifies the sources for the media concatenation
-- pipeline.
createMediaConcatenationPipeline_sources :: Lens.Lens' CreateMediaConcatenationPipeline (Prelude.NonEmpty ConcatenationSource)
createMediaConcatenationPipeline_sources = Lens.lens (\CreateMediaConcatenationPipeline' {sources} -> sources) (\s@CreateMediaConcatenationPipeline' {} a -> s {sources = a} :: CreateMediaConcatenationPipeline) Prelude.. Lens.coerced

-- | An object that specifies the data sinks for the media concatenation
-- pipeline.
createMediaConcatenationPipeline_sinks :: Lens.Lens' CreateMediaConcatenationPipeline (Prelude.NonEmpty ConcatenationSink)
createMediaConcatenationPipeline_sinks = Lens.lens (\CreateMediaConcatenationPipeline' {sinks} -> sinks) (\s@CreateMediaConcatenationPipeline' {} a -> s {sinks = a} :: CreateMediaConcatenationPipeline) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateMediaConcatenationPipeline
  where
  type
    AWSResponse CreateMediaConcatenationPipeline =
      CreateMediaConcatenationPipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMediaConcatenationPipelineResponse'
            Prelude.<$> (x Data..?> "MediaConcatenationPipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMediaConcatenationPipeline
  where
  hashWithSalt
    _salt
    CreateMediaConcatenationPipeline' {..} =
      _salt `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` sources
        `Prelude.hashWithSalt` sinks

instance
  Prelude.NFData
    CreateMediaConcatenationPipeline
  where
  rnf CreateMediaConcatenationPipeline' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf sinks

instance
  Data.ToHeaders
    CreateMediaConcatenationPipeline
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMediaConcatenationPipeline where
  toJSON CreateMediaConcatenationPipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Sources" Data..= sources),
            Prelude.Just ("Sinks" Data..= sinks)
          ]
      )

instance Data.ToPath CreateMediaConcatenationPipeline where
  toPath =
    Prelude.const "/sdk-media-concatenation-pipelines"

instance
  Data.ToQuery
    CreateMediaConcatenationPipeline
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMediaConcatenationPipelineResponse' smart constructor.
data CreateMediaConcatenationPipelineResponse = CreateMediaConcatenationPipelineResponse'
  { -- | A media concatenation pipeline object, the ID, source type,
    -- @MediaPipelineARN@, and sink of a media concatenation pipeline object.
    mediaConcatenationPipeline :: Prelude.Maybe MediaConcatenationPipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaConcatenationPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaConcatenationPipeline', 'createMediaConcatenationPipelineResponse_mediaConcatenationPipeline' - A media concatenation pipeline object, the ID, source type,
-- @MediaPipelineARN@, and sink of a media concatenation pipeline object.
--
-- 'httpStatus', 'createMediaConcatenationPipelineResponse_httpStatus' - The response's http status code.
newCreateMediaConcatenationPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMediaConcatenationPipelineResponse
newCreateMediaConcatenationPipelineResponse
  pHttpStatus_ =
    CreateMediaConcatenationPipelineResponse'
      { mediaConcatenationPipeline =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A media concatenation pipeline object, the ID, source type,
-- @MediaPipelineARN@, and sink of a media concatenation pipeline object.
createMediaConcatenationPipelineResponse_mediaConcatenationPipeline :: Lens.Lens' CreateMediaConcatenationPipelineResponse (Prelude.Maybe MediaConcatenationPipeline)
createMediaConcatenationPipelineResponse_mediaConcatenationPipeline = Lens.lens (\CreateMediaConcatenationPipelineResponse' {mediaConcatenationPipeline} -> mediaConcatenationPipeline) (\s@CreateMediaConcatenationPipelineResponse' {} a -> s {mediaConcatenationPipeline = a} :: CreateMediaConcatenationPipelineResponse)

-- | The response's http status code.
createMediaConcatenationPipelineResponse_httpStatus :: Lens.Lens' CreateMediaConcatenationPipelineResponse Prelude.Int
createMediaConcatenationPipelineResponse_httpStatus = Lens.lens (\CreateMediaConcatenationPipelineResponse' {httpStatus} -> httpStatus) (\s@CreateMediaConcatenationPipelineResponse' {} a -> s {httpStatus = a} :: CreateMediaConcatenationPipelineResponse)

instance
  Prelude.NFData
    CreateMediaConcatenationPipelineResponse
  where
  rnf CreateMediaConcatenationPipelineResponse' {..} =
    Prelude.rnf mediaConcatenationPipeline
      `Prelude.seq` Prelude.rnf httpStatus
