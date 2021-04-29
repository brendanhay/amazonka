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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeviceDefinitionVersion' smart constructor.
data CreateDeviceDefinitionVersion = CreateDeviceDefinitionVersion'
  { -- | A list of devices in the definition version.
    devices :: Prelude.Maybe [Device],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateDeviceDefinitionVersion
newCreateDeviceDefinitionVersion pDeviceDefinitionId_ =
  CreateDeviceDefinitionVersion'
    { devices =
        Prelude.Nothing,
      amznClientToken = Prelude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | A list of devices in the definition version.
createDeviceDefinitionVersion_devices :: Lens.Lens' CreateDeviceDefinitionVersion (Prelude.Maybe [Device])
createDeviceDefinitionVersion_devices = Lens.lens (\CreateDeviceDefinitionVersion' {devices} -> devices) (\s@CreateDeviceDefinitionVersion' {} a -> s {devices = a} :: CreateDeviceDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createDeviceDefinitionVersion_amznClientToken :: Lens.Lens' CreateDeviceDefinitionVersion (Prelude.Maybe Prelude.Text)
createDeviceDefinitionVersion_amznClientToken = Lens.lens (\CreateDeviceDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateDeviceDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateDeviceDefinitionVersion)

-- | The ID of the device definition.
createDeviceDefinitionVersion_deviceDefinitionId :: Lens.Lens' CreateDeviceDefinitionVersion Prelude.Text
createDeviceDefinitionVersion_deviceDefinitionId = Lens.lens (\CreateDeviceDefinitionVersion' {deviceDefinitionId} -> deviceDefinitionId) (\s@CreateDeviceDefinitionVersion' {} a -> s {deviceDefinitionId = a} :: CreateDeviceDefinitionVersion)

instance
  Prelude.AWSRequest
    CreateDeviceDefinitionVersion
  where
  type
    Rs CreateDeviceDefinitionVersion =
      CreateDeviceDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDeviceDefinitionVersion

instance Prelude.NFData CreateDeviceDefinitionVersion

instance
  Prelude.ToHeaders
    CreateDeviceDefinitionVersion
  where
  toHeaders CreateDeviceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateDeviceDefinitionVersion where
  toJSON CreateDeviceDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Devices" Prelude..=) Prelude.<$> devices]
      )

instance Prelude.ToPath CreateDeviceDefinitionVersion where
  toPath CreateDeviceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Prelude.toBS deviceDefinitionId,
        "/versions"
      ]

instance
  Prelude.ToQuery
    CreateDeviceDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceDefinitionVersionResponse' smart constructor.
data CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateDeviceDefinitionVersionResponse
newCreateDeviceDefinitionVersionResponse pHttpStatus_ =
  CreateDeviceDefinitionVersionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createDeviceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateDeviceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ARN of the version.
createDeviceDefinitionVersionResponse_arn :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionVersionResponse_arn = Lens.lens (\CreateDeviceDefinitionVersionResponse' {arn} -> arn) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {arn = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createDeviceDefinitionVersionResponse_id :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionVersionResponse_id = Lens.lens (\CreateDeviceDefinitionVersionResponse' {id} -> id) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {id = a} :: CreateDeviceDefinitionVersionResponse)

-- | The ID of the version.
createDeviceDefinitionVersionResponse_version :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionVersionResponse_version = Lens.lens (\CreateDeviceDefinitionVersionResponse' {version} -> version) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {version = a} :: CreateDeviceDefinitionVersionResponse)

-- | The response's http status code.
createDeviceDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateDeviceDefinitionVersionResponse Prelude.Int
createDeviceDefinitionVersionResponse_httpStatus = Lens.lens (\CreateDeviceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateDeviceDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateDeviceDefinitionVersionResponse
