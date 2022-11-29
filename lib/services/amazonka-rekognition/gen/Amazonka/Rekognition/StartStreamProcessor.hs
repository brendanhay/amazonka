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
-- Module      : Amazonka.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by
-- calling CreateStreamProcessor. To tell @StartStreamProcessor@ which
-- stream processor to start, use the value of the @Name@ field specified
-- in the call to @CreateStreamProcessor@.
--
-- If you are using a label detection stream processor to detect labels,
-- you need to provide a @Start selector@ and a @Stop selector@ to
-- determine the length of the stream processing time.
module Amazonka.Rekognition.StartStreamProcessor
  ( -- * Creating a Request
    StartStreamProcessor (..),
    newStartStreamProcessor,

    -- * Request Lenses
    startStreamProcessor_startSelector,
    startStreamProcessor_stopSelector,
    startStreamProcessor_name,

    -- * Destructuring the Response
    StartStreamProcessorResponse (..),
    newStartStreamProcessorResponse,

    -- * Response Lenses
    startStreamProcessorResponse_sessionId,
    startStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartStreamProcessor' smart constructor.
data StartStreamProcessor = StartStreamProcessor'
  { -- | Specifies the starting point in the Kinesis stream to start processing.
    -- You can use the producer timestamp or the fragment number. If you use
    -- the producer timestamp, you must put the time in milliseconds. For more
    -- information about fragment numbers, see
    -- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_reader_Fragment.html Fragment>.
    --
    -- This is a required parameter for label detection stream processors and
    -- should not be used to start a face search stream processor.
    startSelector :: Prelude.Maybe StreamProcessingStartSelector,
    -- | Specifies when to stop processing the stream. You can specify a maximum
    -- amount of time to process the video.
    --
    -- This is a required parameter for label detection stream processors and
    -- should not be used to start a face search stream processor.
    stopSelector :: Prelude.Maybe StreamProcessingStopSelector,
    -- | The name of the stream processor to start processing.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startSelector', 'startStreamProcessor_startSelector' - Specifies the starting point in the Kinesis stream to start processing.
-- You can use the producer timestamp or the fragment number. If you use
-- the producer timestamp, you must put the time in milliseconds. For more
-- information about fragment numbers, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_reader_Fragment.html Fragment>.
--
-- This is a required parameter for label detection stream processors and
-- should not be used to start a face search stream processor.
--
-- 'stopSelector', 'startStreamProcessor_stopSelector' - Specifies when to stop processing the stream. You can specify a maximum
-- amount of time to process the video.
--
-- This is a required parameter for label detection stream processors and
-- should not be used to start a face search stream processor.
--
-- 'name', 'startStreamProcessor_name' - The name of the stream processor to start processing.
newStartStreamProcessor ::
  -- | 'name'
  Prelude.Text ->
  StartStreamProcessor
newStartStreamProcessor pName_ =
  StartStreamProcessor'
    { startSelector =
        Prelude.Nothing,
      stopSelector = Prelude.Nothing,
      name = pName_
    }

-- | Specifies the starting point in the Kinesis stream to start processing.
-- You can use the producer timestamp or the fragment number. If you use
-- the producer timestamp, you must put the time in milliseconds. For more
-- information about fragment numbers, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_reader_Fragment.html Fragment>.
--
-- This is a required parameter for label detection stream processors and
-- should not be used to start a face search stream processor.
startStreamProcessor_startSelector :: Lens.Lens' StartStreamProcessor (Prelude.Maybe StreamProcessingStartSelector)
startStreamProcessor_startSelector = Lens.lens (\StartStreamProcessor' {startSelector} -> startSelector) (\s@StartStreamProcessor' {} a -> s {startSelector = a} :: StartStreamProcessor)

-- | Specifies when to stop processing the stream. You can specify a maximum
-- amount of time to process the video.
--
-- This is a required parameter for label detection stream processors and
-- should not be used to start a face search stream processor.
startStreamProcessor_stopSelector :: Lens.Lens' StartStreamProcessor (Prelude.Maybe StreamProcessingStopSelector)
startStreamProcessor_stopSelector = Lens.lens (\StartStreamProcessor' {stopSelector} -> stopSelector) (\s@StartStreamProcessor' {} a -> s {stopSelector = a} :: StartStreamProcessor)

-- | The name of the stream processor to start processing.
startStreamProcessor_name :: Lens.Lens' StartStreamProcessor Prelude.Text
startStreamProcessor_name = Lens.lens (\StartStreamProcessor' {name} -> name) (\s@StartStreamProcessor' {} a -> s {name = a} :: StartStreamProcessor)

instance Core.AWSRequest StartStreamProcessor where
  type
    AWSResponse StartStreamProcessor =
      StartStreamProcessorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartStreamProcessorResponse'
            Prelude.<$> (x Core..?> "SessionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartStreamProcessor where
  hashWithSalt _salt StartStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` startSelector
      `Prelude.hashWithSalt` stopSelector
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartStreamProcessor where
  rnf StartStreamProcessor' {..} =
    Prelude.rnf startSelector
      `Prelude.seq` Prelude.rnf stopSelector
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders StartStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StartStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartStreamProcessor where
  toJSON StartStreamProcessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartSelector" Core..=) Prelude.<$> startSelector,
            ("StopSelector" Core..=) Prelude.<$> stopSelector,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath StartStreamProcessor where
  toPath = Prelude.const "/"

instance Core.ToQuery StartStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStreamProcessorResponse' smart constructor.
data StartStreamProcessorResponse = StartStreamProcessorResponse'
  { -- | A unique identifier for the stream processing session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'startStreamProcessorResponse_sessionId' - A unique identifier for the stream processing session.
--
-- 'httpStatus', 'startStreamProcessorResponse_httpStatus' - The response's http status code.
newStartStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartStreamProcessorResponse
newStartStreamProcessorResponse pHttpStatus_ =
  StartStreamProcessorResponse'
    { sessionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the stream processing session.
startStreamProcessorResponse_sessionId :: Lens.Lens' StartStreamProcessorResponse (Prelude.Maybe Prelude.Text)
startStreamProcessorResponse_sessionId = Lens.lens (\StartStreamProcessorResponse' {sessionId} -> sessionId) (\s@StartStreamProcessorResponse' {} a -> s {sessionId = a} :: StartStreamProcessorResponse)

-- | The response's http status code.
startStreamProcessorResponse_httpStatus :: Lens.Lens' StartStreamProcessorResponse Prelude.Int
startStreamProcessorResponse_httpStatus = Lens.lens (\StartStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@StartStreamProcessorResponse' {} a -> s {httpStatus = a} :: StartStreamProcessorResponse)

instance Prelude.NFData StartStreamProcessorResponse where
  rnf StartStreamProcessorResponse' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf httpStatus
