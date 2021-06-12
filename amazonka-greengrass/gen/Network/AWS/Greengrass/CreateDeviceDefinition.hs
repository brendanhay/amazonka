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
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the
-- device definition now or use \'\'CreateDeviceDefinitionVersion\'\' at a
-- later time.
module Network.AWS.Greengrass.CreateDeviceDefinition
  ( -- * Creating a Request
    CreateDeviceDefinition (..),
    newCreateDeviceDefinition,

    -- * Request Lenses
    createDeviceDefinition_name,
    createDeviceDefinition_initialVersion,
    createDeviceDefinition_tags,
    createDeviceDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateDeviceDefinitionResponse (..),
    newCreateDeviceDefinitionResponse,

    -- * Response Lenses
    createDeviceDefinitionResponse_creationTimestamp,
    createDeviceDefinitionResponse_latestVersionArn,
    createDeviceDefinitionResponse_latestVersion,
    createDeviceDefinitionResponse_arn,
    createDeviceDefinitionResponse_id,
    createDeviceDefinitionResponse_name,
    createDeviceDefinitionResponse_lastUpdatedTimestamp,
    createDeviceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { -- | The name of the device definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the device definition.
    initialVersion :: Core.Maybe DeviceDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDeviceDefinition_name' - The name of the device definition.
--
-- 'initialVersion', 'createDeviceDefinition_initialVersion' - Information about the initial version of the device definition.
--
-- 'tags', 'createDeviceDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createDeviceDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateDeviceDefinition ::
  CreateDeviceDefinition
newCreateDeviceDefinition =
  CreateDeviceDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the device definition.
createDeviceDefinition_name :: Lens.Lens' CreateDeviceDefinition (Core.Maybe Core.Text)
createDeviceDefinition_name = Lens.lens (\CreateDeviceDefinition' {name} -> name) (\s@CreateDeviceDefinition' {} a -> s {name = a} :: CreateDeviceDefinition)

-- | Information about the initial version of the device definition.
createDeviceDefinition_initialVersion :: Lens.Lens' CreateDeviceDefinition (Core.Maybe DeviceDefinitionVersion)
createDeviceDefinition_initialVersion = Lens.lens (\CreateDeviceDefinition' {initialVersion} -> initialVersion) (\s@CreateDeviceDefinition' {} a -> s {initialVersion = a} :: CreateDeviceDefinition)

-- | Tag(s) to add to the new resource.
createDeviceDefinition_tags :: Lens.Lens' CreateDeviceDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createDeviceDefinition_tags = Lens.lens (\CreateDeviceDefinition' {tags} -> tags) (\s@CreateDeviceDefinition' {} a -> s {tags = a} :: CreateDeviceDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createDeviceDefinition_amznClientToken :: Lens.Lens' CreateDeviceDefinition (Core.Maybe Core.Text)
createDeviceDefinition_amznClientToken = Lens.lens (\CreateDeviceDefinition' {amznClientToken} -> amznClientToken) (\s@CreateDeviceDefinition' {} a -> s {amznClientToken = a} :: CreateDeviceDefinition)

instance Core.AWSRequest CreateDeviceDefinition where
  type
    AWSResponse CreateDeviceDefinition =
      CreateDeviceDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDeviceDefinition

instance Core.NFData CreateDeviceDefinition

instance Core.ToHeaders CreateDeviceDefinition where
  toHeaders CreateDeviceDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateDeviceDefinition where
  toJSON CreateDeviceDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateDeviceDefinition where
  toPath = Core.const "/greengrass/definition/devices"

instance Core.ToQuery CreateDeviceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
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
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createDeviceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createDeviceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createDeviceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createDeviceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createDeviceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createDeviceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createDeviceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createDeviceDefinitionResponse_httpStatus' - The response's http status code.
newCreateDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDeviceDefinitionResponse
newCreateDeviceDefinitionResponse pHttpStatus_ =
  CreateDeviceDefinitionResponse'
    { creationTimestamp =
        Core.Nothing,
      latestVersionArn = Core.Nothing,
      latestVersion = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createDeviceDefinitionResponse_creationTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_creationTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersionArn :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_latestVersionArn = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersion :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_latestVersion = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersion = a} :: CreateDeviceDefinitionResponse)

-- | The ARN of the definition.
createDeviceDefinitionResponse_arn :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_arn = Lens.lens (\CreateDeviceDefinitionResponse' {arn} -> arn) (\s@CreateDeviceDefinitionResponse' {} a -> s {arn = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the definition.
createDeviceDefinitionResponse_id :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_id = Lens.lens (\CreateDeviceDefinitionResponse' {id} -> id) (\s@CreateDeviceDefinitionResponse' {} a -> s {id = a} :: CreateDeviceDefinitionResponse)

-- | The name of the definition.
createDeviceDefinitionResponse_name :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_name = Lens.lens (\CreateDeviceDefinitionResponse' {name} -> name) (\s@CreateDeviceDefinitionResponse' {} a -> s {name = a} :: CreateDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createDeviceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
createDeviceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The response's http status code.
createDeviceDefinitionResponse_httpStatus :: Lens.Lens' CreateDeviceDefinitionResponse Core.Int
createDeviceDefinitionResponse_httpStatus = Lens.lens (\CreateDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: CreateDeviceDefinitionResponse)

instance Core.NFData CreateDeviceDefinitionResponse
