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
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of
-- the connector definition now or use
-- \'\'CreateConnectorDefinitionVersion\'\' at a later time.
module Network.AWS.Greengrass.CreateConnectorDefinition
  ( -- * Creating a Request
    CreateConnectorDefinition (..),
    newCreateConnectorDefinition,

    -- * Request Lenses
    createConnectorDefinition_name,
    createConnectorDefinition_initialVersion,
    createConnectorDefinition_tags,
    createConnectorDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateConnectorDefinitionResponse (..),
    newCreateConnectorDefinitionResponse,

    -- * Response Lenses
    createConnectorDefinitionResponse_creationTimestamp,
    createConnectorDefinitionResponse_latestVersionArn,
    createConnectorDefinitionResponse_latestVersion,
    createConnectorDefinitionResponse_arn,
    createConnectorDefinitionResponse_id,
    createConnectorDefinitionResponse_name,
    createConnectorDefinitionResponse_lastUpdatedTimestamp,
    createConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { -- | The name of the connector definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the connector definition.
    initialVersion :: Core.Maybe ConnectorDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createConnectorDefinition_name' - The name of the connector definition.
--
-- 'initialVersion', 'createConnectorDefinition_initialVersion' - Information about the initial version of the connector definition.
--
-- 'tags', 'createConnectorDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createConnectorDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateConnectorDefinition ::
  CreateConnectorDefinition
newCreateConnectorDefinition =
  CreateConnectorDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the connector definition.
createConnectorDefinition_name :: Lens.Lens' CreateConnectorDefinition (Core.Maybe Core.Text)
createConnectorDefinition_name = Lens.lens (\CreateConnectorDefinition' {name} -> name) (\s@CreateConnectorDefinition' {} a -> s {name = a} :: CreateConnectorDefinition)

-- | Information about the initial version of the connector definition.
createConnectorDefinition_initialVersion :: Lens.Lens' CreateConnectorDefinition (Core.Maybe ConnectorDefinitionVersion)
createConnectorDefinition_initialVersion = Lens.lens (\CreateConnectorDefinition' {initialVersion} -> initialVersion) (\s@CreateConnectorDefinition' {} a -> s {initialVersion = a} :: CreateConnectorDefinition)

-- | Tag(s) to add to the new resource.
createConnectorDefinition_tags :: Lens.Lens' CreateConnectorDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createConnectorDefinition_tags = Lens.lens (\CreateConnectorDefinition' {tags} -> tags) (\s@CreateConnectorDefinition' {} a -> s {tags = a} :: CreateConnectorDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createConnectorDefinition_amznClientToken :: Lens.Lens' CreateConnectorDefinition (Core.Maybe Core.Text)
createConnectorDefinition_amznClientToken = Lens.lens (\CreateConnectorDefinition' {amznClientToken} -> amznClientToken) (\s@CreateConnectorDefinition' {} a -> s {amznClientToken = a} :: CreateConnectorDefinition)

instance Core.AWSRequest CreateConnectorDefinition where
  type
    AWSResponse CreateConnectorDefinition =
      CreateConnectorDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConnectorDefinition

instance Core.NFData CreateConnectorDefinition

instance Core.ToHeaders CreateConnectorDefinition where
  toHeaders CreateConnectorDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateConnectorDefinition where
  toJSON CreateConnectorDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateConnectorDefinition where
  toPath =
    Core.const "/greengrass/definition/connectors"

instance Core.ToQuery CreateConnectorDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
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
-- Create a value of 'CreateConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createConnectorDefinitionResponse_httpStatus' - The response's http status code.
newCreateConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConnectorDefinitionResponse
newCreateConnectorDefinitionResponse pHttpStatus_ =
  CreateConnectorDefinitionResponse'
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
createConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_latestVersionArn = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_latestVersion = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the definition.
createConnectorDefinitionResponse_arn :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_arn = Lens.lens (\CreateConnectorDefinitionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the definition.
createConnectorDefinitionResponse_id :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_id = Lens.lens (\CreateConnectorDefinitionResponse' {id} -> id) (\s@CreateConnectorDefinitionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionResponse)

-- | The name of the definition.
createConnectorDefinitionResponse_name :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_name = Lens.lens (\CreateConnectorDefinitionResponse' {name} -> name) (\s@CreateConnectorDefinitionResponse' {} a -> s {name = a} :: CreateConnectorDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
createConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The response's http status code.
createConnectorDefinitionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionResponse Core.Int
createConnectorDefinitionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionResponse)

instance
  Core.NFData
    CreateConnectorDefinitionResponse
