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
-- Module      : Amazonka.EC2.GetConsoleScreenshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JPG-format screenshot of a running instance to help with
-- troubleshooting.
--
-- The returned content is Base64-encoded.
module Amazonka.EC2.GetConsoleScreenshot
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
    getConsoleScreenshotResponse_imageData,
    getConsoleScreenshotResponse_instanceId,
    getConsoleScreenshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetConsoleScreenshot where
  type
    AWSResponse GetConsoleScreenshot =
      GetConsoleScreenshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetConsoleScreenshotResponse'
            Prelude.<$> (x Data..@? "imageData")
            Prelude.<*> (x Data..@? "instanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConsoleScreenshot where
  hashWithSalt _salt GetConsoleScreenshot' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` wakeUp
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetConsoleScreenshot where
  rnf GetConsoleScreenshot' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf wakeUp
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders GetConsoleScreenshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetConsoleScreenshot where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConsoleScreenshot where
  toQuery GetConsoleScreenshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetConsoleScreenshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "WakeUp" Data.=: wakeUp,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { -- | The data that comprises the image.
    imageData :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConsoleScreenshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageData', 'getConsoleScreenshotResponse_imageData' - The data that comprises the image.
--
-- 'instanceId', 'getConsoleScreenshotResponse_instanceId' - The ID of the instance.
--
-- 'httpStatus', 'getConsoleScreenshotResponse_httpStatus' - The response's http status code.
newGetConsoleScreenshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConsoleScreenshotResponse
newGetConsoleScreenshotResponse pHttpStatus_ =
  GetConsoleScreenshotResponse'
    { imageData =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data that comprises the image.
getConsoleScreenshotResponse_imageData :: Lens.Lens' GetConsoleScreenshotResponse (Prelude.Maybe Prelude.Text)
getConsoleScreenshotResponse_imageData = Lens.lens (\GetConsoleScreenshotResponse' {imageData} -> imageData) (\s@GetConsoleScreenshotResponse' {} a -> s {imageData = a} :: GetConsoleScreenshotResponse)

-- | The ID of the instance.
getConsoleScreenshotResponse_instanceId :: Lens.Lens' GetConsoleScreenshotResponse (Prelude.Maybe Prelude.Text)
getConsoleScreenshotResponse_instanceId = Lens.lens (\GetConsoleScreenshotResponse' {instanceId} -> instanceId) (\s@GetConsoleScreenshotResponse' {} a -> s {instanceId = a} :: GetConsoleScreenshotResponse)

-- | The response's http status code.
getConsoleScreenshotResponse_httpStatus :: Lens.Lens' GetConsoleScreenshotResponse Prelude.Int
getConsoleScreenshotResponse_httpStatus = Lens.lens (\GetConsoleScreenshotResponse' {httpStatus} -> httpStatus) (\s@GetConsoleScreenshotResponse' {} a -> s {httpStatus = a} :: GetConsoleScreenshotResponse)

instance Prelude.NFData GetConsoleScreenshotResponse where
  rnf GetConsoleScreenshotResponse' {..} =
    Prelude.rnf imageData
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf httpStatus
