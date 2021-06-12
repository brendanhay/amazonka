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
-- Module      : Network.AWS.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access
-- logging on a container, MediaStore stops sending access logs to Amazon
-- CloudWatch Logs. These access logs are not saved and are not
-- retrievable.
module Network.AWS.MediaStore.StopAccessLogging
  ( -- * Creating a Request
    StopAccessLogging (..),
    newStopAccessLogging,

    -- * Request Lenses
    stopAccessLogging_containerName,

    -- * Destructuring the Response
    StopAccessLoggingResponse (..),
    newStopAccessLoggingResponse,

    -- * Response Lenses
    stopAccessLoggingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopAccessLogging' smart constructor.
data StopAccessLogging = StopAccessLogging'
  { -- | The name of the container that you want to stop access logging on.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopAccessLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'stopAccessLogging_containerName' - The name of the container that you want to stop access logging on.
newStopAccessLogging ::
  -- | 'containerName'
  Core.Text ->
  StopAccessLogging
newStopAccessLogging pContainerName_ =
  StopAccessLogging' {containerName = pContainerName_}

-- | The name of the container that you want to stop access logging on.
stopAccessLogging_containerName :: Lens.Lens' StopAccessLogging Core.Text
stopAccessLogging_containerName = Lens.lens (\StopAccessLogging' {containerName} -> containerName) (\s@StopAccessLogging' {} a -> s {containerName = a} :: StopAccessLogging)

instance Core.AWSRequest StopAccessLogging where
  type
    AWSResponse StopAccessLogging =
      StopAccessLoggingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAccessLoggingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopAccessLogging

instance Core.NFData StopAccessLogging

instance Core.ToHeaders StopAccessLogging where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.StopAccessLogging" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopAccessLogging where
  toJSON StopAccessLogging' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.ToPath StopAccessLogging where
  toPath = Core.const "/"

instance Core.ToQuery StopAccessLogging where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopAccessLoggingResponse' smart constructor.
data StopAccessLoggingResponse = StopAccessLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopAccessLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAccessLoggingResponse_httpStatus' - The response's http status code.
newStopAccessLoggingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopAccessLoggingResponse
newStopAccessLoggingResponse pHttpStatus_ =
  StopAccessLoggingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopAccessLoggingResponse_httpStatus :: Lens.Lens' StopAccessLoggingResponse Core.Int
stopAccessLoggingResponse_httpStatus = Lens.lens (\StopAccessLoggingResponse' {httpStatus} -> httpStatus) (\s@StopAccessLoggingResponse' {} a -> s {httpStatus = a} :: StopAccessLoggingResponse)

instance Core.NFData StopAccessLoggingResponse
