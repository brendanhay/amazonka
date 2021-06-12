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
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a device definition that has already been defined.
module Network.AWS.Greengrass.CreateDeviceDefinitionVersion
  ( -- * Creating a Request
    CreateDeviceDefinitionVersion (..),
    newCreateDeviceDefinitionVersion,

    -- * Request Lenses
    createDeviceDefinitionVersion_devices,
    createDeviceDefinitionVersion_amznClientToken,
    createDeviceDefinitionVersion_deviceDefinitionId,

    -- * Destructuring the Response
    CreateDeviceDefinitionVersionResponse (..),
    newCreateDeviceDefinitionVersionResponse,

    -- * Response Lenses
    createDeviceDefinitionVersionResponse_creationTimestamp,
    createDeviceDefinitionVersionResponse_arn,
    createDeviceDefinitionVersionResponse_id,
    createDeviceDefinitionVersionResponse_version,
    createDeviceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeviceDefinitionVersion' smart constructor.
data CreateDeviceDefinitionVersion = CreateDeviceDefinitionVersion'
  { -- | A list of devices in the definition version.
    devices :: Core.Maybe [Device],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'createDeviceDefinitionVersion_devices' - A list of devices in the definition version.
--
-- 'amznClientToken', 'createDeviceDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'deviceDefinitionId', 'createDeviceDefinitionVersion_deviceDefinitionId' - The ID of the device definition.
newCreateDeviceDefinitionVersion ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  CreateDeviceDefinitionVersion
newCreateDeviceDefinitionVersion pDeviceDefinitionId_ =
  CreateDeviceDefinitionVersion'
    { devices =
        Core.Nothing,
      amznClientToken = Core.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | A list of devices in the definition version.
createDeviceDefinitionVersion_devices :: Lens.Lens' CreateDeviceDefinitionVersion (Core.Maybe [Device])
createDeviceDefinitionVersion_devices = Lens.lens (\CreateDeviceDefinitionVersion' {devices} -> devices) (\s@CreateDeviceDefinitionVersion' {} a -> s {devices = a} :: CreateDeviceDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createDeviceDefinitionVersion_amznClientToken :: Lens.Lens' CreateDeviceDefinitionVersion (Core.Maybe Core.Text)
createDeviceDefinitionVersion_amznClientToken = Lens.lens (\CreateDeviceDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateDeviceDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateDeviceDefinitionVersion)

-- | The ID of the device definition.
createDeviceDefinitionVersion_deviceDefinitionId :: Lens.Lens' CreateDeviceDefinitionVersion Core.Text
createDeviceDefinitionVersion_deviceDefinitionId = Lens.lens (\CreateDeviceDefinitionVersion' {deviceDefinitionId} -> deviceDefinitionId) (\s@CreateDeviceDefinitionVersion' {} a -> s {deviceDefinitionId = a} :: CreateDeviceDefinitionVersion)

instance
  Core.AWSRequest
    CreateDeviceDefinitionVersion
  where
  type
    AWSResponse CreateDeviceDefinitionVersion =
      CreateDeviceDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDeviceDefinitionVersion

instance Core.NFData CreateDeviceDefinitionVersion

instance Core.ToHeaders CreateDeviceDefinitionVersion where
  toHeaders CreateDeviceDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateDeviceDefinitionVersion where
  toJSON CreateDeviceDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Devices" Core..=) Core.<$> devices]
      )

instance Core.ToPath CreateDeviceDefinitionVersion where
  toPath CreateDeviceDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId,
        "/versions"
      ]

instance Core.ToQuery CreateDeviceDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDeviceDefinitionVersionResponse' smart constructor.
data CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createDeviceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createDeviceDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createDeviceDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createDeviceDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createDeviceDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateDeviceDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDeviceDefinitionVersionResponse
newCreateDeviceDefinitionVersionResponse pHttpStatus_ =
  CreateDeviceDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createDeviceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
createDeviceDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateDeviceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ARN of the version.
createDeviceDefinitionVersionResponse_arn :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
createDeviceDefinitionVersionResponse_arn = Lens.lens (\CreateDeviceDefinitionVersionResponse' {arn} -> arn) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {arn = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createDeviceDefinitionVersionResponse_id :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
createDeviceDefinitionVersionResponse_id = Lens.lens (\CreateDeviceDefinitionVersionResponse' {id} -> id) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {id = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ID of the version.
createDeviceDefinitionVersionResponse_version :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
createDeviceDefinitionVersionResponse_version = Lens.lens (\CreateDeviceDefinitionVersionResponse' {version} -> version) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {version = a} :: CreateDeviceDefinitionVersionResponse)

-- | The response's http status code.
createDeviceDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateDeviceDefinitionVersionResponse Core.Int
createDeviceDefinitionVersionResponse_httpStatus = Lens.lens (\CreateDeviceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateDeviceDefinitionVersionResponse)

instance
  Core.NFData
    CreateDeviceDefinitionVersionResponse
