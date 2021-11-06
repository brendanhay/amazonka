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
-- Module      : Amazonka.MediaConnect.UpdateFlowMediaStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing media stream.
module Amazonka.MediaConnect.UpdateFlowMediaStream
  ( -- * Creating a Request
    UpdateFlowMediaStream (..),
    newUpdateFlowMediaStream,

    -- * Request Lenses
    updateFlowMediaStream_videoFormat,
    updateFlowMediaStream_mediaStreamType,
    updateFlowMediaStream_attributes,
    updateFlowMediaStream_clockRate,
    updateFlowMediaStream_description,
    updateFlowMediaStream_flowArn,
    updateFlowMediaStream_mediaStreamName,

    -- * Destructuring the Response
    UpdateFlowMediaStreamResponse (..),
    newUpdateFlowMediaStreamResponse,

    -- * Response Lenses
    updateFlowMediaStreamResponse_flowArn,
    updateFlowMediaStreamResponse_mediaStream,
    updateFlowMediaStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The fields that you want to update in the media stream.
--
-- /See:/ 'newUpdateFlowMediaStream' smart constructor.
data UpdateFlowMediaStream = UpdateFlowMediaStream'
  { -- | The resolution of the video.
    videoFormat :: Prelude.Maybe Prelude.Text,
    -- | The type of media stream.
    mediaStreamType :: Prelude.Maybe MediaStreamType,
    -- | The attributes that you want to assign to the media stream.
    attributes :: Prelude.Maybe MediaStreamAttributesRequest,
    -- | The sample rate (in Hz) for the stream. If the media stream type is
    -- video or ancillary data, set this value to 90000. If the media stream
    -- type is audio, set this value to either 48000 or 96000.
    clockRate :: Prelude.Maybe Prelude.Int,
    -- | Description
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow.
    flowArn :: Prelude.Text,
    -- | The name of the media stream that you want to update.
    mediaStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowMediaStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'videoFormat', 'updateFlowMediaStream_videoFormat' - The resolution of the video.
--
-- 'mediaStreamType', 'updateFlowMediaStream_mediaStreamType' - The type of media stream.
--
-- 'attributes', 'updateFlowMediaStream_attributes' - The attributes that you want to assign to the media stream.
--
-- 'clockRate', 'updateFlowMediaStream_clockRate' - The sample rate (in Hz) for the stream. If the media stream type is
-- video or ancillary data, set this value to 90000. If the media stream
-- type is audio, set this value to either 48000 or 96000.
--
-- 'description', 'updateFlowMediaStream_description' - Description
--
-- 'flowArn', 'updateFlowMediaStream_flowArn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'mediaStreamName', 'updateFlowMediaStream_mediaStreamName' - The name of the media stream that you want to update.
newUpdateFlowMediaStream ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'mediaStreamName'
  Prelude.Text ->
  UpdateFlowMediaStream
newUpdateFlowMediaStream pFlowArn_ pMediaStreamName_ =
  UpdateFlowMediaStream'
    { videoFormat =
        Prelude.Nothing,
      mediaStreamType = Prelude.Nothing,
      attributes = Prelude.Nothing,
      clockRate = Prelude.Nothing,
      description = Prelude.Nothing,
      flowArn = pFlowArn_,
      mediaStreamName = pMediaStreamName_
    }

-- | The resolution of the video.
updateFlowMediaStream_videoFormat :: Lens.Lens' UpdateFlowMediaStream (Prelude.Maybe Prelude.Text)
updateFlowMediaStream_videoFormat = Lens.lens (\UpdateFlowMediaStream' {videoFormat} -> videoFormat) (\s@UpdateFlowMediaStream' {} a -> s {videoFormat = a} :: UpdateFlowMediaStream)

-- | The type of media stream.
updateFlowMediaStream_mediaStreamType :: Lens.Lens' UpdateFlowMediaStream (Prelude.Maybe MediaStreamType)
updateFlowMediaStream_mediaStreamType = Lens.lens (\UpdateFlowMediaStream' {mediaStreamType} -> mediaStreamType) (\s@UpdateFlowMediaStream' {} a -> s {mediaStreamType = a} :: UpdateFlowMediaStream)

-- | The attributes that you want to assign to the media stream.
updateFlowMediaStream_attributes :: Lens.Lens' UpdateFlowMediaStream (Prelude.Maybe MediaStreamAttributesRequest)
updateFlowMediaStream_attributes = Lens.lens (\UpdateFlowMediaStream' {attributes} -> attributes) (\s@UpdateFlowMediaStream' {} a -> s {attributes = a} :: UpdateFlowMediaStream)

-- | The sample rate (in Hz) for the stream. If the media stream type is
-- video or ancillary data, set this value to 90000. If the media stream
-- type is audio, set this value to either 48000 or 96000.
updateFlowMediaStream_clockRate :: Lens.Lens' UpdateFlowMediaStream (Prelude.Maybe Prelude.Int)
updateFlowMediaStream_clockRate = Lens.lens (\UpdateFlowMediaStream' {clockRate} -> clockRate) (\s@UpdateFlowMediaStream' {} a -> s {clockRate = a} :: UpdateFlowMediaStream)

-- | Description
updateFlowMediaStream_description :: Lens.Lens' UpdateFlowMediaStream (Prelude.Maybe Prelude.Text)
updateFlowMediaStream_description = Lens.lens (\UpdateFlowMediaStream' {description} -> description) (\s@UpdateFlowMediaStream' {} a -> s {description = a} :: UpdateFlowMediaStream)

-- | The Amazon Resource Name (ARN) of the flow.
updateFlowMediaStream_flowArn :: Lens.Lens' UpdateFlowMediaStream Prelude.Text
updateFlowMediaStream_flowArn = Lens.lens (\UpdateFlowMediaStream' {flowArn} -> flowArn) (\s@UpdateFlowMediaStream' {} a -> s {flowArn = a} :: UpdateFlowMediaStream)

-- | The name of the media stream that you want to update.
updateFlowMediaStream_mediaStreamName :: Lens.Lens' UpdateFlowMediaStream Prelude.Text
updateFlowMediaStream_mediaStreamName = Lens.lens (\UpdateFlowMediaStream' {mediaStreamName} -> mediaStreamName) (\s@UpdateFlowMediaStream' {} a -> s {mediaStreamName = a} :: UpdateFlowMediaStream)

instance Core.AWSRequest UpdateFlowMediaStream where
  type
    AWSResponse UpdateFlowMediaStream =
      UpdateFlowMediaStreamResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlowMediaStreamResponse'
            Prelude.<$> (x Core..?> "flowArn")
            Prelude.<*> (x Core..?> "mediaStream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlowMediaStream

instance Prelude.NFData UpdateFlowMediaStream

instance Core.ToHeaders UpdateFlowMediaStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFlowMediaStream where
  toJSON UpdateFlowMediaStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("videoFormat" Core..=) Prelude.<$> videoFormat,
            ("mediaStreamType" Core..=)
              Prelude.<$> mediaStreamType,
            ("attributes" Core..=) Prelude.<$> attributes,
            ("clockRate" Core..=) Prelude.<$> clockRate,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateFlowMediaStream where
  toPath UpdateFlowMediaStream' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Core.toBS flowArn,
        "/mediaStreams/",
        Core.toBS mediaStreamName
      ]

instance Core.ToQuery UpdateFlowMediaStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlowMediaStreamResponse' smart constructor.
data UpdateFlowMediaStreamResponse = UpdateFlowMediaStreamResponse'
  { -- | The ARN of the flow that is associated with the media stream that you
    -- updated.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The media stream that you updated.
    mediaStream :: Prelude.Maybe MediaStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowMediaStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'updateFlowMediaStreamResponse_flowArn' - The ARN of the flow that is associated with the media stream that you
-- updated.
--
-- 'mediaStream', 'updateFlowMediaStreamResponse_mediaStream' - The media stream that you updated.
--
-- 'httpStatus', 'updateFlowMediaStreamResponse_httpStatus' - The response's http status code.
newUpdateFlowMediaStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlowMediaStreamResponse
newUpdateFlowMediaStreamResponse pHttpStatus_ =
  UpdateFlowMediaStreamResponse'
    { flowArn =
        Prelude.Nothing,
      mediaStream = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that is associated with the media stream that you
-- updated.
updateFlowMediaStreamResponse_flowArn :: Lens.Lens' UpdateFlowMediaStreamResponse (Prelude.Maybe Prelude.Text)
updateFlowMediaStreamResponse_flowArn = Lens.lens (\UpdateFlowMediaStreamResponse' {flowArn} -> flowArn) (\s@UpdateFlowMediaStreamResponse' {} a -> s {flowArn = a} :: UpdateFlowMediaStreamResponse)

-- | The media stream that you updated.
updateFlowMediaStreamResponse_mediaStream :: Lens.Lens' UpdateFlowMediaStreamResponse (Prelude.Maybe MediaStream)
updateFlowMediaStreamResponse_mediaStream = Lens.lens (\UpdateFlowMediaStreamResponse' {mediaStream} -> mediaStream) (\s@UpdateFlowMediaStreamResponse' {} a -> s {mediaStream = a} :: UpdateFlowMediaStreamResponse)

-- | The response's http status code.
updateFlowMediaStreamResponse_httpStatus :: Lens.Lens' UpdateFlowMediaStreamResponse Prelude.Int
updateFlowMediaStreamResponse_httpStatus = Lens.lens (\UpdateFlowMediaStreamResponse' {httpStatus} -> httpStatus) (\s@UpdateFlowMediaStreamResponse' {} a -> s {httpStatus = a} :: UpdateFlowMediaStreamResponse)

instance Prelude.NFData UpdateFlowMediaStreamResponse
