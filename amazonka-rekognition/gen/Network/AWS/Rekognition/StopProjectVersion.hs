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
-- Module      : Network.AWS.Rekognition.StopProjectVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running model. The operation might take a while to complete. To
-- check the current status, call DescribeProjectVersions.
module Network.AWS.Rekognition.StopProjectVersion
  ( -- * Creating a Request
    StopProjectVersion (..),
    newStopProjectVersion,

    -- * Request Lenses
    stopProjectVersion_projectVersionArn,

    -- * Destructuring the Response
    StopProjectVersionResponse (..),
    newStopProjectVersionResponse,

    -- * Response Lenses
    stopProjectVersionResponse_status,
    stopProjectVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopProjectVersion' smart constructor.
data StopProjectVersion = StopProjectVersion'
  { -- | The Amazon Resource Name (ARN) of the model version that you want to
    -- delete.
    --
    -- This operation requires permissions to perform the
    -- @rekognition:StopProjectVersion@ action.
    projectVersionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVersionArn', 'stopProjectVersion_projectVersionArn' - The Amazon Resource Name (ARN) of the model version that you want to
-- delete.
--
-- This operation requires permissions to perform the
-- @rekognition:StopProjectVersion@ action.
newStopProjectVersion ::
  -- | 'projectVersionArn'
  Core.Text ->
  StopProjectVersion
newStopProjectVersion pProjectVersionArn_ =
  StopProjectVersion'
    { projectVersionArn =
        pProjectVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the model version that you want to
-- delete.
--
-- This operation requires permissions to perform the
-- @rekognition:StopProjectVersion@ action.
stopProjectVersion_projectVersionArn :: Lens.Lens' StopProjectVersion Core.Text
stopProjectVersion_projectVersionArn = Lens.lens (\StopProjectVersion' {projectVersionArn} -> projectVersionArn) (\s@StopProjectVersion' {} a -> s {projectVersionArn = a} :: StopProjectVersion)

instance Core.AWSRequest StopProjectVersion where
  type
    AWSResponse StopProjectVersion =
      StopProjectVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopProjectVersionResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopProjectVersion

instance Core.NFData StopProjectVersion

instance Core.ToHeaders StopProjectVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StopProjectVersion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopProjectVersion where
  toJSON StopProjectVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProjectVersionArn" Core..= projectVersionArn)
          ]
      )

instance Core.ToPath StopProjectVersion where
  toPath = Core.const "/"

instance Core.ToQuery StopProjectVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopProjectVersionResponse' smart constructor.
data StopProjectVersionResponse = StopProjectVersionResponse'
  { -- | The current status of the stop operation.
    status :: Core.Maybe ProjectVersionStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stopProjectVersionResponse_status' - The current status of the stop operation.
--
-- 'httpStatus', 'stopProjectVersionResponse_httpStatus' - The response's http status code.
newStopProjectVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopProjectVersionResponse
newStopProjectVersionResponse pHttpStatus_ =
  StopProjectVersionResponse'
    { status = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the stop operation.
stopProjectVersionResponse_status :: Lens.Lens' StopProjectVersionResponse (Core.Maybe ProjectVersionStatus)
stopProjectVersionResponse_status = Lens.lens (\StopProjectVersionResponse' {status} -> status) (\s@StopProjectVersionResponse' {} a -> s {status = a} :: StopProjectVersionResponse)

-- | The response's http status code.
stopProjectVersionResponse_httpStatus :: Lens.Lens' StopProjectVersionResponse Core.Int
stopProjectVersionResponse_httpStatus = Lens.lens (\StopProjectVersionResponse' {httpStatus} -> httpStatus) (\s@StopProjectVersionResponse' {} a -> s {httpStatus = a} :: StopProjectVersionResponse)

instance Core.NFData StopProjectVersionResponse
