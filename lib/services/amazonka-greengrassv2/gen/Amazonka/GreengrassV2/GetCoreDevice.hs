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
-- Module      : Amazonka.GreengrassV2.GetCoreDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a Greengrass core device.
--
-- IoT Greengrass relies on individual devices to send status updates to
-- the Amazon Web Services Cloud. If the IoT Greengrass Core software
-- isn\'t running on the device, or if device isn\'t connected to the
-- Amazon Web Services Cloud, then the reported status of that device might
-- not reflect its current status. The status timestamp indicates when the
-- device status was last updated.
--
-- Core devices send status updates at the following times:
--
-- -   When the IoT Greengrass Core software starts
--
-- -   When the core device receives a deployment from the Amazon Web
--     Services Cloud
--
-- -   When the status of any component on the core device becomes @BROKEN@
--
-- -   At a
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html#greengrass-nucleus-component-configuration-fss regular interval that you can configure>,
--     which defaults to 24 hours
--
-- -   For IoT Greengrass Core v2.7.0, the core device sends status updates
--     upon local deployment and cloud deployment
module Amazonka.GreengrassV2.GetCoreDevice
  ( -- * Creating a Request
    GetCoreDevice (..),
    newGetCoreDevice,

    -- * Request Lenses
    getCoreDevice_coreDeviceThingName,

    -- * Destructuring the Response
    GetCoreDeviceResponse (..),
    newGetCoreDeviceResponse,

    -- * Response Lenses
    getCoreDeviceResponse_architecture,
    getCoreDeviceResponse_coreDeviceThingName,
    getCoreDeviceResponse_coreVersion,
    getCoreDeviceResponse_lastStatusUpdateTimestamp,
    getCoreDeviceResponse_platform,
    getCoreDeviceResponse_status,
    getCoreDeviceResponse_tags,
    getCoreDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDeviceResponse'
            Prelude.<$> (x Data..?> "architecture")
            Prelude.<*> (x Data..?> "coreDeviceThingName")
            Prelude.<*> (x Data..?> "coreVersion")
            Prelude.<*> (x Data..?> "lastStatusUpdateTimestamp")
            Prelude.<*> (x Data..?> "platform")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreDevice where
  hashWithSalt _salt GetCoreDevice' {..} =
    _salt `Prelude.hashWithSalt` coreDeviceThingName

instance Prelude.NFData GetCoreDevice where
  rnf GetCoreDevice' {..} =
    Prelude.rnf coreDeviceThingName

instance Data.ToHeaders GetCoreDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCoreDevice where
  toPath GetCoreDevice' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName
      ]

instance Data.ToQuery GetCoreDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreDeviceResponse' smart constructor.
data GetCoreDeviceResponse = GetCoreDeviceResponse'
  { -- | The computer architecture of the core device.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Maybe Prelude.Text,
    -- | The version of the IoT Greengrass Core software that the core device
    -- runs. This version is equivalent to the version of the Greengrass
    -- nucleus component that runs on the core device. For more information,
    -- see the
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
    -- in the /IoT Greengrass V2 Developer Guide/.
    coreVersion :: Prelude.Maybe Prelude.Text,
    -- | The time at which the core device\'s status last updated, expressed in
    -- ISO 8601 format.
    lastStatusUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The operating system platform that the core device runs.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The status of the core device. The core device status can be:
    --
    -- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
    --     on the core device without issue.
    --
    -- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
    --     a failed state on the core device.
    status :: Prelude.Maybe CoreDeviceStatus,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'architecture', 'getCoreDeviceResponse_architecture' - The computer architecture of the core device.
--
-- 'coreDeviceThingName', 'getCoreDeviceResponse_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
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
-- 'platform', 'getCoreDeviceResponse_platform' - The operating system platform that the core device runs.
--
-- 'status', 'getCoreDeviceResponse_status' - The status of the core device. The core device status can be:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
--
-- 'tags', 'getCoreDeviceResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'httpStatus', 'getCoreDeviceResponse_httpStatus' - The response's http status code.
newGetCoreDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreDeviceResponse
newGetCoreDeviceResponse pHttpStatus_ =
  GetCoreDeviceResponse'
    { architecture =
        Prelude.Nothing,
      coreDeviceThingName = Prelude.Nothing,
      coreVersion = Prelude.Nothing,
      lastStatusUpdateTimestamp = Prelude.Nothing,
      platform = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The computer architecture of the core device.
getCoreDeviceResponse_architecture :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_architecture = Lens.lens (\GetCoreDeviceResponse' {architecture} -> architecture) (\s@GetCoreDeviceResponse' {} a -> s {architecture = a} :: GetCoreDeviceResponse)

-- | The name of the core device. This is also the name of the IoT thing.
getCoreDeviceResponse_coreDeviceThingName :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_coreDeviceThingName = Lens.lens (\GetCoreDeviceResponse' {coreDeviceThingName} -> coreDeviceThingName) (\s@GetCoreDeviceResponse' {} a -> s {coreDeviceThingName = a} :: GetCoreDeviceResponse)

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
getCoreDeviceResponse_lastStatusUpdateTimestamp = Lens.lens (\GetCoreDeviceResponse' {lastStatusUpdateTimestamp} -> lastStatusUpdateTimestamp) (\s@GetCoreDeviceResponse' {} a -> s {lastStatusUpdateTimestamp = a} :: GetCoreDeviceResponse) Prelude.. Lens.mapping Data._Time

-- | The operating system platform that the core device runs.
getCoreDeviceResponse_platform :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe Prelude.Text)
getCoreDeviceResponse_platform = Lens.lens (\GetCoreDeviceResponse' {platform} -> platform) (\s@GetCoreDeviceResponse' {} a -> s {platform = a} :: GetCoreDeviceResponse)

-- | The status of the core device. The core device status can be:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
getCoreDeviceResponse_status :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe CoreDeviceStatus)
getCoreDeviceResponse_status = Lens.lens (\GetCoreDeviceResponse' {status} -> status) (\s@GetCoreDeviceResponse' {} a -> s {status = a} :: GetCoreDeviceResponse)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
getCoreDeviceResponse_tags :: Lens.Lens' GetCoreDeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCoreDeviceResponse_tags = Lens.lens (\GetCoreDeviceResponse' {tags} -> tags) (\s@GetCoreDeviceResponse' {} a -> s {tags = a} :: GetCoreDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCoreDeviceResponse_httpStatus :: Lens.Lens' GetCoreDeviceResponse Prelude.Int
getCoreDeviceResponse_httpStatus = Lens.lens (\GetCoreDeviceResponse' {httpStatus} -> httpStatus) (\s@GetCoreDeviceResponse' {} a -> s {httpStatus = a} :: GetCoreDeviceResponse)

instance Prelude.NFData GetCoreDeviceResponse where
  rnf GetCoreDeviceResponse' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf coreDeviceThingName
      `Prelude.seq` Prelude.rnf coreVersion
      `Prelude.seq` Prelude.rnf lastStatusUpdateTimestamp
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
