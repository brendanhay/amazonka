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
-- Module      : Network.AWS.EC2.GetConsoleScreenshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JPG-format screenshot of a running instance to help with
-- troubleshooting.
--
-- The returned content is Base64-encoded.
module Network.AWS.EC2.GetConsoleScreenshot
  ( -- * Creating a Request
    GetConsoleScreenshot (..),
    newGetConsoleScreenshot,

    -- * Request Lenses
    getConsoleScreenshot_dryRun,
    getConsoleScreenshot_wakeUp,
    getConsoleScreenshot_instanceId,

    -- * Destructuring the Response
    GetConsoleScreenshotResponse (..),
    newGetConsoleScreenshotResponse,

    -- * Response Lenses
    getConsoleScreenshotResponse_instanceId,
    getConsoleScreenshotResponse_imageData,
    getConsoleScreenshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | When set to @true@, acts as keystroke input and wakes up an instance
    -- that\'s in standby or \"sleep\" mode.
    wakeUp :: Core.Maybe Core.Bool,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConsoleScreenshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getConsoleScreenshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'wakeUp', 'getConsoleScreenshot_wakeUp' - When set to @true@, acts as keystroke input and wakes up an instance
-- that\'s in standby or \"sleep\" mode.
--
-- 'instanceId', 'getConsoleScreenshot_instanceId' - The ID of the instance.
newGetConsoleScreenshot ::
  -- | 'instanceId'
  Core.Text ->
  GetConsoleScreenshot
newGetConsoleScreenshot pInstanceId_ =
  GetConsoleScreenshot'
    { dryRun = Core.Nothing,
      wakeUp = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getConsoleScreenshot_dryRun :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
getConsoleScreenshot_dryRun = Lens.lens (\GetConsoleScreenshot' {dryRun} -> dryRun) (\s@GetConsoleScreenshot' {} a -> s {dryRun = a} :: GetConsoleScreenshot)

-- | When set to @true@, acts as keystroke input and wakes up an instance
-- that\'s in standby or \"sleep\" mode.
getConsoleScreenshot_wakeUp :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
getConsoleScreenshot_wakeUp = Lens.lens (\GetConsoleScreenshot' {wakeUp} -> wakeUp) (\s@GetConsoleScreenshot' {} a -> s {wakeUp = a} :: GetConsoleScreenshot)

-- | The ID of the instance.
getConsoleScreenshot_instanceId :: Lens.Lens' GetConsoleScreenshot Core.Text
getConsoleScreenshot_instanceId = Lens.lens (\GetConsoleScreenshot' {instanceId} -> instanceId) (\s@GetConsoleScreenshot' {} a -> s {instanceId = a} :: GetConsoleScreenshot)

instance Core.AWSRequest GetConsoleScreenshot where
  type
    AWSResponse GetConsoleScreenshot =
      GetConsoleScreenshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetConsoleScreenshotResponse'
            Core.<$> (x Core..@? "instanceId")
            Core.<*> (x Core..@? "imageData")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConsoleScreenshot

instance Core.NFData GetConsoleScreenshot

instance Core.ToHeaders GetConsoleScreenshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetConsoleScreenshot where
  toPath = Core.const "/"

instance Core.ToQuery GetConsoleScreenshot where
  toQuery GetConsoleScreenshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetConsoleScreenshot" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "WakeUp" Core.=: wakeUp,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The data that comprises the image.
    imageData :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConsoleScreenshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getConsoleScreenshotResponse_instanceId' - The ID of the instance.
--
-- 'imageData', 'getConsoleScreenshotResponse_imageData' - The data that comprises the image.
--
-- 'httpStatus', 'getConsoleScreenshotResponse_httpStatus' - The response's http status code.
newGetConsoleScreenshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConsoleScreenshotResponse
newGetConsoleScreenshotResponse pHttpStatus_ =
  GetConsoleScreenshotResponse'
    { instanceId =
        Core.Nothing,
      imageData = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the instance.
getConsoleScreenshotResponse_instanceId :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Core.Text)
getConsoleScreenshotResponse_instanceId = Lens.lens (\GetConsoleScreenshotResponse' {instanceId} -> instanceId) (\s@GetConsoleScreenshotResponse' {} a -> s {instanceId = a} :: GetConsoleScreenshotResponse)

-- | The data that comprises the image.
getConsoleScreenshotResponse_imageData :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Core.Text)
getConsoleScreenshotResponse_imageData = Lens.lens (\GetConsoleScreenshotResponse' {imageData} -> imageData) (\s@GetConsoleScreenshotResponse' {} a -> s {imageData = a} :: GetConsoleScreenshotResponse)

-- | The response's http status code.
getConsoleScreenshotResponse_httpStatus :: Lens.Lens' GetConsoleScreenshotResponse Core.Int
getConsoleScreenshotResponse_httpStatus = Lens.lens (\GetConsoleScreenshotResponse' {httpStatus} -> httpStatus) (\s@GetConsoleScreenshotResponse' {} a -> s {httpStatus = a} :: GetConsoleScreenshotResponse)

instance Core.NFData GetConsoleScreenshotResponse
