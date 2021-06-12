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
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logger definition. You may provide the initial version of the
-- logger definition now or use \'\'CreateLoggerDefinitionVersion\'\' at a
-- later time.
module Network.AWS.Greengrass.CreateLoggerDefinition
  ( -- * Creating a Request
    CreateLoggerDefinition (..),
    newCreateLoggerDefinition,

    -- * Request Lenses
    createLoggerDefinition_name,
    createLoggerDefinition_initialVersion,
    createLoggerDefinition_tags,
    createLoggerDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateLoggerDefinitionResponse (..),
    newCreateLoggerDefinitionResponse,

    -- * Response Lenses
    createLoggerDefinitionResponse_creationTimestamp,
    createLoggerDefinitionResponse_latestVersionArn,
    createLoggerDefinitionResponse_latestVersion,
    createLoggerDefinitionResponse_arn,
    createLoggerDefinitionResponse_id,
    createLoggerDefinitionResponse_name,
    createLoggerDefinitionResponse_lastUpdatedTimestamp,
    createLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoggerDefinition' smart constructor.
data CreateLoggerDefinition = CreateLoggerDefinition'
  { -- | The name of the logger definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the logger definition.
    initialVersion :: Core.Maybe LoggerDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createLoggerDefinition_name' - The name of the logger definition.
--
-- 'initialVersion', 'createLoggerDefinition_initialVersion' - Information about the initial version of the logger definition.
--
-- 'tags', 'createLoggerDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createLoggerDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateLoggerDefinition ::
  CreateLoggerDefinition
newCreateLoggerDefinition =
  CreateLoggerDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the logger definition.
createLoggerDefinition_name :: Lens.Lens' CreateLoggerDefinition (Core.Maybe Core.Text)
createLoggerDefinition_name = Lens.lens (\CreateLoggerDefinition' {name} -> name) (\s@CreateLoggerDefinition' {} a -> s {name = a} :: CreateLoggerDefinition)

-- | Information about the initial version of the logger definition.
createLoggerDefinition_initialVersion :: Lens.Lens' CreateLoggerDefinition (Core.Maybe LoggerDefinitionVersion)
createLoggerDefinition_initialVersion = Lens.lens (\CreateLoggerDefinition' {initialVersion} -> initialVersion) (\s@CreateLoggerDefinition' {} a -> s {initialVersion = a} :: CreateLoggerDefinition)

-- | Tag(s) to add to the new resource.
createLoggerDefinition_tags :: Lens.Lens' CreateLoggerDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createLoggerDefinition_tags = Lens.lens (\CreateLoggerDefinition' {tags} -> tags) (\s@CreateLoggerDefinition' {} a -> s {tags = a} :: CreateLoggerDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createLoggerDefinition_amznClientToken :: Lens.Lens' CreateLoggerDefinition (Core.Maybe Core.Text)
createLoggerDefinition_amznClientToken = Lens.lens (\CreateLoggerDefinition' {amznClientToken} -> amznClientToken) (\s@CreateLoggerDefinition' {} a -> s {amznClientToken = a} :: CreateLoggerDefinition)

instance Core.AWSRequest CreateLoggerDefinition where
  type
    AWSResponse CreateLoggerDefinition =
      CreateLoggerDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLoggerDefinition

instance Core.NFData CreateLoggerDefinition

instance Core.ToHeaders CreateLoggerDefinition where
  toHeaders CreateLoggerDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateLoggerDefinition where
  toJSON CreateLoggerDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateLoggerDefinition where
  toPath = Core.const "/greengrass/definition/loggers"

instance Core.ToQuery CreateLoggerDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateLoggerDefinitionResponse' smart constructor.
data CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse'
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
-- Create a value of 'CreateLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createLoggerDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createLoggerDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createLoggerDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createLoggerDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createLoggerDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createLoggerDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createLoggerDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createLoggerDefinitionResponse_httpStatus' - The response's http status code.
newCreateLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLoggerDefinitionResponse
newCreateLoggerDefinitionResponse pHttpStatus_ =
  CreateLoggerDefinitionResponse'
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
createLoggerDefinitionResponse_creationTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_creationTimestamp = Lens.lens (\CreateLoggerDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateLoggerDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateLoggerDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createLoggerDefinitionResponse_latestVersionArn :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_latestVersionArn = Lens.lens (\CreateLoggerDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateLoggerDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateLoggerDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createLoggerDefinitionResponse_latestVersion :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_latestVersion = Lens.lens (\CreateLoggerDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateLoggerDefinitionResponse' {} a -> s {latestVersion = a} :: CreateLoggerDefinitionResponse)

-- | The ARN of the definition.
createLoggerDefinitionResponse_arn :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_arn = Lens.lens (\CreateLoggerDefinitionResponse' {arn} -> arn) (\s@CreateLoggerDefinitionResponse' {} a -> s {arn = a} :: CreateLoggerDefinitionResponse)

-- | The ID of the definition.
createLoggerDefinitionResponse_id :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_id = Lens.lens (\CreateLoggerDefinitionResponse' {id} -> id) (\s@CreateLoggerDefinitionResponse' {} a -> s {id = a} :: CreateLoggerDefinitionResponse)

-- | The name of the definition.
createLoggerDefinitionResponse_name :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_name = Lens.lens (\CreateLoggerDefinitionResponse' {name} -> name) (\s@CreateLoggerDefinitionResponse' {} a -> s {name = a} :: CreateLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createLoggerDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
createLoggerDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateLoggerDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateLoggerDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateLoggerDefinitionResponse)

-- | The response's http status code.
createLoggerDefinitionResponse_httpStatus :: Lens.Lens' CreateLoggerDefinitionResponse Core.Int
createLoggerDefinitionResponse_httpStatus = Lens.lens (\CreateLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: CreateLoggerDefinitionResponse)

instance Core.NFData CreateLoggerDefinitionResponse
