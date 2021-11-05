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
-- Module      : Network.AWS.GreengrassV2.GetCoreDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a Greengrass core device.
module Network.AWS.GreengrassV2.GetCoreDevice
  ( -- * Creating a Request
    GetCoreDevice (..),
    newGetCoreDevice,

    -- * Request Lenses
    getCoreDevice_coreDeviceThingName,

    -- * Destructuring the Response
    GetCoreDeviceResponse (..),
    newGetCoreDeviceResponse,

    -- * Response Lenses
    getCoreDeviceResponse_status,
    getCoreDeviceResponse_platform,
    getCoreDeviceResponse_architecture,
    getCoreDeviceResponse_coreDeviceThingName,
    getCoreDeviceResponse_tags,
    getCoreDeviceResponse_coreVersion,
    getCoreDeviceResponse_lastStatusUpdateTimestamp,
    getCoreDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCoreDevice' smart constructor.
data GetCoreDevice = GetCoreDevice'
  { -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDeviceThingName', 'getCoreDevice_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newGetCoreDevice ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  GetCoreDevice
newGetCoreDevice pCoreDeviceThingName_ =
  GetCoreDevice'
    { coreDeviceThingName =
        pCoreDeviceThingName_
    }

-- | The name of the core device. This is also the name of the IoT thing.
getCoreDevice_coreDeviceThingName :: Lens.Lens' GetCoreDevice Prelude.Text
getCoreDevice_coreDeviceThingName = Lens.lens (\GetCoreDevice' {coreDeviceThingName} -> coreDeviceThingName) (\s@GetCoreDevice' {} a -> s {coreDeviceThingName = a} :: GetCoreDevice)

instance Core.AWSRequest GetCoreDevice where
  type
    AWSResponse GetCoreDevice =
      GetCoreDeviceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDeviceResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "platform")
            Prelude.<*> (x Core..?> "architecture")
            Prelude.<*> (x Core..?> "coreDeviceThingName")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "coreVersion")
            Prelude.<*> (x Core..?> "lastStatusUpdateTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreDevice

instance Prelude.NFData GetCoreDevice

instance Core.ToHeaders GetCoreDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCoreDevice where
  toPath GetCoreDevice' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Core.toBS coreDeviceThingName
      ]

instance Core.ToQuery GetCoreDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreDeviceResponse' smart constructor.
data GetCoreDeviceResponse = GetCoreDeviceResponse'
  { -- | The status of the core device. The core device status can be:
    --
    -- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
    --     on the core device without issue.
    --
    -- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
    --     a failed state on the core device.
    status :: Prelude.Maybe CoreDeviceStatus,
    -- | The operating system platform that the core device runs.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The computer architecture of the core device.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the IoT Greengrass Core software that the core device
    -- runs. This version is equivalent to the version of the Greengrass
    -- nucleus component that runs on the core device. For more information,
    -- see the
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
    -- in the /IoT Greengrass V2 Developer Guide/.
    coreVersion :: Prelude.Maybe Prelude.Text,
    -- | The time at which the core device\'s status last updated, expressed in
    -- ISO 8601 format.
    lastStatusUpdateTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getCoreDeviceResponse_status' - The status of the core device. The core device status can be:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
--
-- 'platform', 'getCoreDeviceResponse_platform' - The operating system platform that the core device runs.
--
-- 'architecture', 'getCoreDeviceResponse_architecture' - The computer architecture of the core device.
--
-- 'coreDeviceThingName', 'getCoreDeviceResponse_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
--
-- 'tags', 'getCoreDeviceResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'coreVersion', 'getCoreDeviceResponse_coreVersion' - The version of the IoT Greengrass Core software that the core device
-- runs. This version is equivalent to the version of the Greengrass
-- nucleus component that runs on the core device. For more information,
-- see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'lastStatusUpdateTimestamp', 'getCoreDeviceResponse_lastStatusUpdateTimestamp' - The time at which the core device\'s status last updated, expressed in
-- ISO 8601 format.
--
-- 'httpStatus', 'getCoreDeviceResponse_httpStatus' - The response's http status code.
newGetCoreDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreDeviceResponse
newGetCoreDeviceResponse pHttpStatus_ =
  GetCoreDeviceResponse'
    { status = Prelude.Nothing,
      platform = Prelude.Nothing,
      architecture = Prelude.Nothing,
      coreDeviceThingName = Prelude.Nothing,
      tags = Prelude.Nothing,
      coreVersion = Prelude.Nothing,
      lastStatusUpdateTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the core device. The core device status can be:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
getCoreDeviceResponse_status :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe CoreDeviceStatus)
getCoreDeviceResponse_status = Lens.lens (\GetCoreDeviceResponse' {status} -> status) (\s@GetCoreDeviceResponse' {} a -> s {status = a} :: GetCoreDeviceResponse)

-- | The operating system platform that the core device runs.
getCoreDeviceResponse_platform :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_platform = Lens.lens (\GetCoreDeviceResponse' {platform} -> platform) (\s@GetCoreDeviceResponse' {} a -> s {platform = a} :: GetCoreDeviceResponse)

-- | The computer architecture of the core device.
getCoreDeviceResponse_architecture :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_architecture = Lens.lens (\GetCoreDeviceResponse' {architecture} -> architecture) (\s@GetCoreDeviceResponse' {} a -> s {architecture = a} :: GetCoreDeviceResponse)

-- | The name of the core device. This is also the name of the IoT thing.
getCoreDeviceResponse_coreDeviceThingName :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_coreDeviceThingName = Lens.lens (\GetCoreDeviceResponse' {coreDeviceThingName} -> coreDeviceThingName) (\s@GetCoreDeviceResponse' {} a -> s {coreDeviceThingName = a} :: GetCoreDeviceResponse)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
getCoreDeviceResponse_tags :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCoreDeviceResponse_tags = Lens.lens (\GetCoreDeviceResponse' {tags} -> tags) (\s@GetCoreDeviceResponse' {} a -> s {tags = a} :: GetCoreDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the IoT Greengrass Core software that the core device
-- runs. This version is equivalent to the version of the Greengrass
-- nucleus component that runs on the core device. For more information,
-- see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
-- in the /IoT Greengrass V2 Developer Guide/.
getCoreDeviceResponse_coreVersion :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_coreVersion = Lens.lens (\GetCoreDeviceResponse' {coreVersion} -> coreVersion) (\s@GetCoreDeviceResponse' {} a -> s {coreVersion = a} :: GetCoreDeviceResponse)

-- | The time at which the core device\'s status last updated, expressed in
-- ISO 8601 format.
getCoreDeviceResponse_lastStatusUpdateTimestamp :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.UTCTime)
getCoreDeviceResponse_lastStatusUpdateTimestamp = Lens.lens (\GetCoreDeviceResponse' {lastStatusUpdateTimestamp} -> lastStatusUpdateTimestamp) (\s@GetCoreDeviceResponse' {} a -> s {lastStatusUpdateTimestamp = a} :: GetCoreDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getCoreDeviceResponse_httpStatus :: Lens.Lens' GetCoreDeviceResponse Prelude.Int
getCoreDeviceResponse_httpStatus = Lens.lens (\GetCoreDeviceResponse' {httpStatus} -> httpStatus) (\s@GetCoreDeviceResponse' {} a -> s {httpStatus = a} :: GetCoreDeviceResponse)

instance Prelude.NFData GetCoreDeviceResponse
