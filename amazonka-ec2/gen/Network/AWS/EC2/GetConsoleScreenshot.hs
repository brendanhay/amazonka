{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, acts as keystroke input and wakes up an instance
    -- that\'s in standby or \"sleep\" mode.
    wakeUp :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetConsoleScreenshot
newGetConsoleScreenshot pInstanceId_ =
  GetConsoleScreenshot'
    { dryRun = Prelude.Nothing,
      wakeUp = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getConsoleScreenshot_dryRun :: Lens.Lens' GetConsoleScreenshot (Prelude.Maybe Prelude.Bool)
getConsoleScreenshot_dryRun = Lens.lens (\GetConsoleScreenshot' {dryRun} -> dryRun) (\s@GetConsoleScreenshot' {} a -> s {dryRun = a} :: GetConsoleScreenshot)

-- | When set to @true@, acts as keystroke input and wakes up an instance
-- that\'s in standby or \"sleep\" mode.
getConsoleScreenshot_wakeUp :: Lens.Lens' GetConsoleScreenshot (Prelude.Maybe Prelude.Bool)
getConsoleScreenshot_wakeUp = Lens.lens (\GetConsoleScreenshot' {wakeUp} -> wakeUp) (\s@GetConsoleScreenshot' {} a -> s {wakeUp = a} :: GetConsoleScreenshot)

-- | The ID of the instance.
getConsoleScreenshot_instanceId :: Lens.Lens' GetConsoleScreenshot Prelude.Text
getConsoleScreenshot_instanceId = Lens.lens (\GetConsoleScreenshot' {instanceId} -> instanceId) (\s@GetConsoleScreenshot' {} a -> s {instanceId = a} :: GetConsoleScreenshot)

instance Prelude.AWSRequest GetConsoleScreenshot where
  type
    Rs GetConsoleScreenshot =
      GetConsoleScreenshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetConsoleScreenshotResponse'
            Prelude.<$> (x Prelude..@? "instanceId")
            Prelude.<*> (x Prelude..@? "imageData")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConsoleScreenshot

instance Prelude.NFData GetConsoleScreenshot

instance Prelude.ToHeaders GetConsoleScreenshot where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetConsoleScreenshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetConsoleScreenshot where
  toQuery GetConsoleScreenshot' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetConsoleScreenshot" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "WakeUp" Prelude.=: wakeUp,
        "InstanceId" Prelude.=: instanceId
      ]

-- | /See:/ 'newGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The data that comprises the image.
    imageData :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetConsoleScreenshotResponse
newGetConsoleScreenshotResponse pHttpStatus_ =
  GetConsoleScreenshotResponse'
    { instanceId =
        Prelude.Nothing,
      imageData = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the instance.
getConsoleScreenshotResponse_instanceId :: Lens.Lens' GetConsoleScreenshotResponse (Prelude.Maybe Prelude.Text)
getConsoleScreenshotResponse_instanceId = Lens.lens (\GetConsoleScreenshotResponse' {instanceId} -> instanceId) (\s@GetConsoleScreenshotResponse' {} a -> s {instanceId = a} :: GetConsoleScreenshotResponse)

-- | The data that comprises the image.
getConsoleScreenshotResponse_imageData :: Lens.Lens' GetConsoleScreenshotResponse (Prelude.Maybe Prelude.Text)
getConsoleScreenshotResponse_imageData = Lens.lens (\GetConsoleScreenshotResponse' {imageData} -> imageData) (\s@GetConsoleScreenshotResponse' {} a -> s {imageData = a} :: GetConsoleScreenshotResponse)

-- | The response's http status code.
getConsoleScreenshotResponse_httpStatus :: Lens.Lens' GetConsoleScreenshotResponse Prelude.Int
getConsoleScreenshotResponse_httpStatus = Lens.lens (\GetConsoleScreenshotResponse' {httpStatus} -> httpStatus) (\s@GetConsoleScreenshotResponse' {} a -> s {httpStatus = a} :: GetConsoleScreenshotResponse)

instance Prelude.NFData GetConsoleScreenshotResponse
