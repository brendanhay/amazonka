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
-- Module      : Amazonka.Chime.CreateMediaCapturePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a media capture pipeline.
module Amazonka.Chime.CreateMediaCapturePipeline
  ( -- * Creating a Request
    CreateMediaCapturePipeline (..),
    newCreateMediaCapturePipeline,

    -- * Request Lenses
    createMediaCapturePipeline_clientRequestToken,
    createMediaCapturePipeline_chimeSdkMeetingConfiguration,
    createMediaCapturePipeline_sourceType,
    createMediaCapturePipeline_sourceArn,
    createMediaCapturePipeline_sinkType,
    createMediaCapturePipeline_sinkArn,

    -- * Destructuring the Response
    CreateMediaCapturePipelineResponse (..),
    newCreateMediaCapturePipelineResponse,

    -- * Response Lenses
    createMediaCapturePipelineResponse_mediaCapturePipeline,
    createMediaCapturePipelineResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMediaCapturePipeline' smart constructor.
data CreateMediaCapturePipeline = CreateMediaCapturePipeline'
  { -- | The token assigned to the client making the pipeline request.
    clientRequestToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The configuration for a specified media capture pipeline. @SourceType@
    -- must be @ChimeSdkMeeting@.
    chimeSdkMeetingConfiguration :: Prelude.Maybe ChimeSdkMeetingConfiguration,
    -- | Source type from which the media artifacts will be captured. A Chime SDK
    -- Meeting is the only supported source.
    sourceType :: MediaPipelineSourceType,
    -- | ARN of the source from which the media artifacts are captured.
    sourceArn :: Data.Sensitive Prelude.Text,
    -- | Destination type to which the media artifacts are saved. You must use an
    -- S3 bucket.
    sinkType :: MediaPipelineSinkType,
    -- | The ARN of the sink type.
    sinkArn :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaCapturePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMediaCapturePipeline_clientRequestToken' - The token assigned to the client making the pipeline request.
--
-- 'chimeSdkMeetingConfiguration', 'createMediaCapturePipeline_chimeSdkMeetingConfiguration' - The configuration for a specified media capture pipeline. @SourceType@
-- must be @ChimeSdkMeeting@.
--
-- 'sourceType', 'createMediaCapturePipeline_sourceType' - Source type from which the media artifacts will be captured. A Chime SDK
-- Meeting is the only supported source.
--
-- 'sourceArn', 'createMediaCapturePipeline_sourceArn' - ARN of the source from which the media artifacts are captured.
--
-- 'sinkType', 'createMediaCapturePipeline_sinkType' - Destination type to which the media artifacts are saved. You must use an
-- S3 bucket.
--
-- 'sinkArn', 'createMediaCapturePipeline_sinkArn' - The ARN of the sink type.
newCreateMediaCapturePipeline ::
  -- | 'sourceType'
  MediaPipelineSourceType ->
  -- | 'sourceArn'
  Prelude.Text ->
  -- | 'sinkType'
  MediaPipelineSinkType ->
  -- | 'sinkArn'
  Prelude.Text ->
  CreateMediaCapturePipeline
newCreateMediaCapturePipeline
  pSourceType_
  pSourceArn_
  pSinkType_
  pSinkArn_ =
    CreateMediaCapturePipeline'
      { clientRequestToken =
          Prelude.Nothing,
        chimeSdkMeetingConfiguration = Prelude.Nothing,
        sourceType = pSourceType_,
        sourceArn = Data._Sensitive Lens.# pSourceArn_,
        sinkType = pSinkType_,
        sinkArn = Data._Sensitive Lens.# pSinkArn_
      }

-- | The token assigned to the client making the pipeline request.
createMediaCapturePipeline_clientRequestToken :: Lens.Lens' CreateMediaCapturePipeline (Prelude.Maybe Prelude.Text)
createMediaCapturePipeline_clientRequestToken = Lens.lens (\CreateMediaCapturePipeline' {clientRequestToken} -> clientRequestToken) (\s@CreateMediaCapturePipeline' {} a -> s {clientRequestToken = a} :: CreateMediaCapturePipeline) Prelude.. Lens.mapping Data._Sensitive

-- | The configuration for a specified media capture pipeline. @SourceType@
-- must be @ChimeSdkMeeting@.
createMediaCapturePipeline_chimeSdkMeetingConfiguration :: Lens.Lens' CreateMediaCapturePipeline (Prelude.Maybe ChimeSdkMeetingConfiguration)
createMediaCapturePipeline_chimeSdkMeetingConfiguration = Lens.lens (\CreateMediaCapturePipeline' {chimeSdkMeetingConfiguration} -> chimeSdkMeetingConfiguration) (\s@CreateMediaCapturePipeline' {} a -> s {chimeSdkMeetingConfiguration = a} :: CreateMediaCapturePipeline)

-- | Source type from which the media artifacts will be captured. A Chime SDK
-- Meeting is the only supported source.
createMediaCapturePipeline_sourceType :: Lens.Lens' CreateMediaCapturePipeline MediaPipelineSourceType
createMediaCapturePipeline_sourceType = Lens.lens (\CreateMediaCapturePipeline' {sourceType} -> sourceType) (\s@CreateMediaCapturePipeline' {} a -> s {sourceType = a} :: CreateMediaCapturePipeline)

-- | ARN of the source from which the media artifacts are captured.
createMediaCapturePipeline_sourceArn :: Lens.Lens' CreateMediaCapturePipeline Prelude.Text
createMediaCapturePipeline_sourceArn = Lens.lens (\CreateMediaCapturePipeline' {sourceArn} -> sourceArn) (\s@CreateMediaCapturePipeline' {} a -> s {sourceArn = a} :: CreateMediaCapturePipeline) Prelude.. Data._Sensitive

-- | Destination type to which the media artifacts are saved. You must use an
-- S3 bucket.
createMediaCapturePipeline_sinkType :: Lens.Lens' CreateMediaCapturePipeline MediaPipelineSinkType
createMediaCapturePipeline_sinkType = Lens.lens (\CreateMediaCapturePipeline' {sinkType} -> sinkType) (\s@CreateMediaCapturePipeline' {} a -> s {sinkType = a} :: CreateMediaCapturePipeline)

-- | The ARN of the sink type.
createMediaCapturePipeline_sinkArn :: Lens.Lens' CreateMediaCapturePipeline Prelude.Text
createMediaCapturePipeline_sinkArn = Lens.lens (\CreateMediaCapturePipeline' {sinkArn} -> sinkArn) (\s@CreateMediaCapturePipeline' {} a -> s {sinkArn = a} :: CreateMediaCapturePipeline) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateMediaCapturePipeline where
  type
    AWSResponse CreateMediaCapturePipeline =
      CreateMediaCapturePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMediaCapturePipelineResponse'
            Prelude.<$> (x Data..?> "MediaCapturePipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMediaCapturePipeline where
  hashWithSalt _salt CreateMediaCapturePipeline' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` chimeSdkMeetingConfiguration
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sinkType
      `Prelude.hashWithSalt` sinkArn

instance Prelude.NFData CreateMediaCapturePipeline where
  rnf CreateMediaCapturePipeline' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf chimeSdkMeetingConfiguration
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sinkType
      `Prelude.seq` Prelude.rnf sinkArn

instance Data.ToHeaders CreateMediaCapturePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMediaCapturePipeline where
  toJSON CreateMediaCapturePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("ChimeSdkMeetingConfiguration" Data..=)
              Prelude.<$> chimeSdkMeetingConfiguration,
            Prelude.Just ("SourceType" Data..= sourceType),
            Prelude.Just ("SourceArn" Data..= sourceArn),
            Prelude.Just ("SinkType" Data..= sinkType),
            Prelude.Just ("SinkArn" Data..= sinkArn)
          ]
      )

instance Data.ToPath CreateMediaCapturePipeline where
  toPath = Prelude.const "/media-capture-pipelines"

instance Data.ToQuery CreateMediaCapturePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMediaCapturePipelineResponse' smart constructor.
data CreateMediaCapturePipelineResponse = CreateMediaCapturePipelineResponse'
  { -- | A media capture pipeline object, the ID, source type, source ARN, sink
    -- type, and sink ARN of a media capture pipeline object.
    mediaCapturePipeline :: Prelude.Maybe MediaCapturePipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaCapturePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaCapturePipeline', 'createMediaCapturePipelineResponse_mediaCapturePipeline' - A media capture pipeline object, the ID, source type, source ARN, sink
-- type, and sink ARN of a media capture pipeline object.
--
-- 'httpStatus', 'createMediaCapturePipelineResponse_httpStatus' - The response's http status code.
newCreateMediaCapturePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMediaCapturePipelineResponse
newCreateMediaCapturePipelineResponse pHttpStatus_ =
  CreateMediaCapturePipelineResponse'
    { mediaCapturePipeline =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A media capture pipeline object, the ID, source type, source ARN, sink
-- type, and sink ARN of a media capture pipeline object.
createMediaCapturePipelineResponse_mediaCapturePipeline :: Lens.Lens' CreateMediaCapturePipelineResponse (Prelude.Maybe MediaCapturePipeline)
createMediaCapturePipelineResponse_mediaCapturePipeline = Lens.lens (\CreateMediaCapturePipelineResponse' {mediaCapturePipeline} -> mediaCapturePipeline) (\s@CreateMediaCapturePipelineResponse' {} a -> s {mediaCapturePipeline = a} :: CreateMediaCapturePipelineResponse)

-- | The response's http status code.
createMediaCapturePipelineResponse_httpStatus :: Lens.Lens' CreateMediaCapturePipelineResponse Prelude.Int
createMediaCapturePipelineResponse_httpStatus = Lens.lens (\CreateMediaCapturePipelineResponse' {httpStatus} -> httpStatus) (\s@CreateMediaCapturePipelineResponse' {} a -> s {httpStatus = a} :: CreateMediaCapturePipelineResponse)

instance
  Prelude.NFData
    CreateMediaCapturePipelineResponse
  where
  rnf CreateMediaCapturePipelineResponse' {..} =
    Prelude.rnf mediaCapturePipeline
      `Prelude.seq` Prelude.rnf httpStatus
