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
-- Module      : Amazonka.IoT.UpdateStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing stream. The stream version will be incremented by
-- one.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateStream>
-- action.
module Amazonka.IoT.UpdateStream
  ( -- * Creating a Request
    UpdateStream (..),
    newUpdateStream,

    -- * Request Lenses
    updateStream_description,
    updateStream_files,
    updateStream_roleArn,
    updateStream_streamId,

    -- * Destructuring the Response
    UpdateStreamResponse (..),
    newUpdateStreamResponse,

    -- * Response Lenses
    updateStreamResponse_description,
    updateStreamResponse_streamArn,
    updateStreamResponse_streamId,
    updateStreamResponse_streamVersion,
    updateStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { -- | The description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The files associated with the stream.
    files :: Prelude.Maybe (Prelude.NonEmpty StreamFile),
    -- | An IAM role that allows the IoT service principal assumes to access your
    -- S3 files.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateStream_description' - The description of the stream.
--
-- 'files', 'updateStream_files' - The files associated with the stream.
--
-- 'roleArn', 'updateStream_roleArn' - An IAM role that allows the IoT service principal assumes to access your
-- S3 files.
--
-- 'streamId', 'updateStream_streamId' - The stream ID.
newUpdateStream ::
  -- | 'streamId'
  Prelude.Text ->
  UpdateStream
newUpdateStream pStreamId_ =
  UpdateStream'
    { description = Prelude.Nothing,
      files = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      streamId = pStreamId_
    }

-- | The description of the stream.
updateStream_description :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_description = Lens.lens (\UpdateStream' {description} -> description) (\s@UpdateStream' {} a -> s {description = a} :: UpdateStream)

-- | The files associated with the stream.
updateStream_files :: Lens.Lens' UpdateStream (Prelude.Maybe (Prelude.NonEmpty StreamFile))
updateStream_files = Lens.lens (\UpdateStream' {files} -> files) (\s@UpdateStream' {} a -> s {files = a} :: UpdateStream) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role that allows the IoT service principal assumes to access your
-- S3 files.
updateStream_roleArn :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_roleArn = Lens.lens (\UpdateStream' {roleArn} -> roleArn) (\s@UpdateStream' {} a -> s {roleArn = a} :: UpdateStream)

-- | The stream ID.
updateStream_streamId :: Lens.Lens' UpdateStream Prelude.Text
updateStream_streamId = Lens.lens (\UpdateStream' {streamId} -> streamId) (\s@UpdateStream' {} a -> s {streamId = a} :: UpdateStream)

instance Core.AWSRequest UpdateStream where
  type AWSResponse UpdateStream = UpdateStreamResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStreamResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "streamArn")
            Prelude.<*> (x Data..?> "streamId")
            Prelude.<*> (x Data..?> "streamVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStream where
  hashWithSalt _salt UpdateStream' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` streamId

instance Prelude.NFData UpdateStream where
  rnf UpdateStream' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf files `Prelude.seq`
        Prelude.rnf roleArn `Prelude.seq`
          Prelude.rnf streamId

instance Data.ToHeaders UpdateStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateStream where
  toJSON UpdateStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("files" Data..=) Prelude.<$> files,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )

instance Data.ToPath UpdateStream where
  toPath UpdateStream' {..} =
    Prelude.mconcat ["/streams/", Data.toBS streamId]

instance Data.ToQuery UpdateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStreamResponse' smart constructor.
data UpdateStreamResponse = UpdateStreamResponse'
  { -- | A description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stream ARN.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The stream version.
    streamVersion :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateStreamResponse_description' - A description of the stream.
--
-- 'streamArn', 'updateStreamResponse_streamArn' - The stream ARN.
--
-- 'streamId', 'updateStreamResponse_streamId' - The stream ID.
--
-- 'streamVersion', 'updateStreamResponse_streamVersion' - The stream version.
--
-- 'httpStatus', 'updateStreamResponse_httpStatus' - The response's http status code.
newUpdateStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStreamResponse
newUpdateStreamResponse pHttpStatus_ =
  UpdateStreamResponse'
    { description =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      streamId = Prelude.Nothing,
      streamVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the stream.
updateStreamResponse_description :: Lens.Lens' UpdateStreamResponse (Prelude.Maybe Prelude.Text)
updateStreamResponse_description = Lens.lens (\UpdateStreamResponse' {description} -> description) (\s@UpdateStreamResponse' {} a -> s {description = a} :: UpdateStreamResponse)

-- | The stream ARN.
updateStreamResponse_streamArn :: Lens.Lens' UpdateStreamResponse (Prelude.Maybe Prelude.Text)
updateStreamResponse_streamArn = Lens.lens (\UpdateStreamResponse' {streamArn} -> streamArn) (\s@UpdateStreamResponse' {} a -> s {streamArn = a} :: UpdateStreamResponse)

-- | The stream ID.
updateStreamResponse_streamId :: Lens.Lens' UpdateStreamResponse (Prelude.Maybe Prelude.Text)
updateStreamResponse_streamId = Lens.lens (\UpdateStreamResponse' {streamId} -> streamId) (\s@UpdateStreamResponse' {} a -> s {streamId = a} :: UpdateStreamResponse)

-- | The stream version.
updateStreamResponse_streamVersion :: Lens.Lens' UpdateStreamResponse (Prelude.Maybe Prelude.Natural)
updateStreamResponse_streamVersion = Lens.lens (\UpdateStreamResponse' {streamVersion} -> streamVersion) (\s@UpdateStreamResponse' {} a -> s {streamVersion = a} :: UpdateStreamResponse)

-- | The response's http status code.
updateStreamResponse_httpStatus :: Lens.Lens' UpdateStreamResponse Prelude.Int
updateStreamResponse_httpStatus = Lens.lens (\UpdateStreamResponse' {httpStatus} -> httpStatus) (\s@UpdateStreamResponse' {} a -> s {httpStatus = a} :: UpdateStreamResponse)

instance Prelude.NFData UpdateStreamResponse where
  rnf UpdateStreamResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf streamArn `Prelude.seq`
        Prelude.rnf streamId `Prelude.seq`
          Prelude.rnf streamVersion `Prelude.seq`
            Prelude.rnf httpStatus
