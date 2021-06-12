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
-- Module      : Network.AWS.Rekognition.StopStreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running stream processor that was created by
-- CreateStreamProcessor.
module Network.AWS.Rekognition.StopStreamProcessor
  ( -- * Creating a Request
    StopStreamProcessor (..),
    newStopStreamProcessor,

    -- * Request Lenses
    stopStreamProcessor_name,

    -- * Destructuring the Response
    StopStreamProcessorResponse (..),
    newStopStreamProcessorResponse,

    -- * Response Lenses
    stopStreamProcessorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopStreamProcessor' smart constructor.
data StopStreamProcessor = StopStreamProcessor'
  { -- | The name of a stream processor created by CreateStreamProcessor.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopStreamProcessor_name' - The name of a stream processor created by CreateStreamProcessor.
newStopStreamProcessor ::
  -- | 'name'
  Core.Text ->
  StopStreamProcessor
newStopStreamProcessor pName_ =
  StopStreamProcessor' {name = pName_}

-- | The name of a stream processor created by CreateStreamProcessor.
stopStreamProcessor_name :: Lens.Lens' StopStreamProcessor Core.Text
stopStreamProcessor_name = Lens.lens (\StopStreamProcessor' {name} -> name) (\s@StopStreamProcessor' {} a -> s {name = a} :: StopStreamProcessor)

instance Core.AWSRequest StopStreamProcessor where
  type
    AWSResponse StopStreamProcessor =
      StopStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopStreamProcessorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopStreamProcessor

instance Core.NFData StopStreamProcessor

instance Core.ToHeaders StopStreamProcessor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StopStreamProcessor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopStreamProcessor where
  toJSON StopStreamProcessor' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StopStreamProcessor where
  toPath = Core.const "/"

instance Core.ToQuery StopStreamProcessor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopStreamProcessorResponse' smart constructor.
data StopStreamProcessorResponse = StopStreamProcessorResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopStreamProcessorResponse_httpStatus' - The response's http status code.
newStopStreamProcessorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopStreamProcessorResponse
newStopStreamProcessorResponse pHttpStatus_ =
  StopStreamProcessorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopStreamProcessorResponse_httpStatus :: Lens.Lens' StopStreamProcessorResponse Core.Int
stopStreamProcessorResponse_httpStatus = Lens.lens (\StopStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@StopStreamProcessorResponse' {} a -> s {httpStatus = a} :: StopStreamProcessorResponse)

instance Core.NFData StopStreamProcessorResponse
