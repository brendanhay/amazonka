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
-- Module      : Network.AWS.Greengrass.GetDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition.
module Network.AWS.Greengrass.GetDeviceDefinition
  ( -- * Creating a Request
    GetDeviceDefinition (..),
    newGetDeviceDefinition,

    -- * Request Lenses
    getDeviceDefinition_deviceDefinitionId,

    -- * Destructuring the Response
    GetDeviceDefinitionResponse (..),
    newGetDeviceDefinitionResponse,

    -- * Response Lenses
    getDeviceDefinitionResponse_creationTimestamp,
    getDeviceDefinitionResponse_latestVersionArn,
    getDeviceDefinitionResponse_latestVersion,
    getDeviceDefinitionResponse_arn,
    getDeviceDefinitionResponse_id,
    getDeviceDefinitionResponse_name,
    getDeviceDefinitionResponse_lastUpdatedTimestamp,
    getDeviceDefinitionResponse_tags,
    getDeviceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeviceDefinition' smart constructor.
data GetDeviceDefinition = GetDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceDefinitionId', 'getDeviceDefinition_deviceDefinitionId' - The ID of the device definition.
newGetDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  GetDeviceDefinition
newGetDeviceDefinition pDeviceDefinitionId_ =
  GetDeviceDefinition'
    { deviceDefinitionId =
        pDeviceDefinitionId_
    }

-- | The ID of the device definition.
getDeviceDefinition_deviceDefinitionId :: Lens.Lens' GetDeviceDefinition Core.Text
getDeviceDefinition_deviceDefinitionId = Lens.lens (\GetDeviceDefinition' {deviceDefinitionId} -> deviceDefinitionId) (\s@GetDeviceDefinition' {} a -> s {deviceDefinitionId = a} :: GetDeviceDefinition)

instance Core.AWSRequest GetDeviceDefinition where
  type
    AWSResponse GetDeviceDefinition =
      GetDeviceDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeviceDefinition

instance Core.NFData GetDeviceDefinition

instance Core.ToHeaders GetDeviceDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDeviceDefinition where
  toPath GetDeviceDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId
      ]

instance Core.ToQuery GetDeviceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getDeviceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getDeviceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getDeviceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getDeviceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getDeviceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getDeviceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getDeviceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getDeviceDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getDeviceDefinitionResponse_httpStatus' - The response's http status code.
newGetDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeviceDefinitionResponse
newGetDeviceDefinitionResponse pHttpStatus_ =
  GetDeviceDefinitionResponse'
    { creationTimestamp =
        Core.Nothing,
      latestVersionArn = Core.Nothing,
      latestVersion = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getDeviceDefinitionResponse_creationTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_creationTimestamp = Lens.lens (\GetDeviceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDeviceDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetDeviceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getDeviceDefinitionResponse_latestVersionArn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_latestVersionArn = Lens.lens (\GetDeviceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetDeviceDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetDeviceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getDeviceDefinitionResponse_latestVersion :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_latestVersion = Lens.lens (\GetDeviceDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetDeviceDefinitionResponse' {} a -> s {latestVersion = a} :: GetDeviceDefinitionResponse)

-- | The ARN of the definition.
getDeviceDefinitionResponse_arn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_arn = Lens.lens (\GetDeviceDefinitionResponse' {arn} -> arn) (\s@GetDeviceDefinitionResponse' {} a -> s {arn = a} :: GetDeviceDefinitionResponse)

-- | The ID of the definition.
getDeviceDefinitionResponse_id :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_id = Lens.lens (\GetDeviceDefinitionResponse' {id} -> id) (\s@GetDeviceDefinitionResponse' {} a -> s {id = a} :: GetDeviceDefinitionResponse)

-- | The name of the definition.
getDeviceDefinitionResponse_name :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_name = Lens.lens (\GetDeviceDefinitionResponse' {name} -> name) (\s@GetDeviceDefinitionResponse' {} a -> s {name = a} :: GetDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getDeviceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
getDeviceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetDeviceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetDeviceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetDeviceDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getDeviceDefinitionResponse_tags :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getDeviceDefinitionResponse_tags = Lens.lens (\GetDeviceDefinitionResponse' {tags} -> tags) (\s@GetDeviceDefinitionResponse' {} a -> s {tags = a} :: GetDeviceDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDeviceDefinitionResponse_httpStatus :: Lens.Lens' GetDeviceDefinitionResponse Core.Int
getDeviceDefinitionResponse_httpStatus = Lens.lens (\GetDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: GetDeviceDefinitionResponse)

instance Core.NFData GetDeviceDefinitionResponse
