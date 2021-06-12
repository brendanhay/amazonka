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
-- Module      : Network.AWS.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the
-- core definition now or use \'\'CreateCoreDefinitionVersion\'\' at a
-- later time. Greengrass groups must each contain exactly one Greengrass
-- core.
module Network.AWS.Greengrass.CreateCoreDefinition
  ( -- * Creating a Request
    CreateCoreDefinition (..),
    newCreateCoreDefinition,

    -- * Request Lenses
    createCoreDefinition_name,
    createCoreDefinition_initialVersion,
    createCoreDefinition_tags,
    createCoreDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateCoreDefinitionResponse (..),
    newCreateCoreDefinitionResponse,

    -- * Response Lenses
    createCoreDefinitionResponse_creationTimestamp,
    createCoreDefinitionResponse_latestVersionArn,
    createCoreDefinitionResponse_latestVersion,
    createCoreDefinitionResponse_arn,
    createCoreDefinitionResponse_id,
    createCoreDefinitionResponse_name,
    createCoreDefinitionResponse_lastUpdatedTimestamp,
    createCoreDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to create a core definition.
--
-- /See:/ 'newCreateCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { -- | The name of the core definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the core definition.
    initialVersion :: Core.Maybe CoreDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCoreDefinition_name' - The name of the core definition.
--
-- 'initialVersion', 'createCoreDefinition_initialVersion' - Information about the initial version of the core definition.
--
-- 'tags', 'createCoreDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createCoreDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateCoreDefinition ::
  CreateCoreDefinition
newCreateCoreDefinition =
  CreateCoreDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the core definition.
createCoreDefinition_name :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
createCoreDefinition_name = Lens.lens (\CreateCoreDefinition' {name} -> name) (\s@CreateCoreDefinition' {} a -> s {name = a} :: CreateCoreDefinition)

-- | Information about the initial version of the core definition.
createCoreDefinition_initialVersion :: Lens.Lens' CreateCoreDefinition (Core.Maybe CoreDefinitionVersion)
createCoreDefinition_initialVersion = Lens.lens (\CreateCoreDefinition' {initialVersion} -> initialVersion) (\s@CreateCoreDefinition' {} a -> s {initialVersion = a} :: CreateCoreDefinition)

-- | Tag(s) to add to the new resource.
createCoreDefinition_tags :: Lens.Lens' CreateCoreDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createCoreDefinition_tags = Lens.lens (\CreateCoreDefinition' {tags} -> tags) (\s@CreateCoreDefinition' {} a -> s {tags = a} :: CreateCoreDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createCoreDefinition_amznClientToken :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
createCoreDefinition_amznClientToken = Lens.lens (\CreateCoreDefinition' {amznClientToken} -> amznClientToken) (\s@CreateCoreDefinition' {} a -> s {amznClientToken = a} :: CreateCoreDefinition)

instance Core.AWSRequest CreateCoreDefinition where
  type
    AWSResponse CreateCoreDefinition =
      CreateCoreDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCoreDefinition

instance Core.NFData CreateCoreDefinition

instance Core.ToHeaders CreateCoreDefinition where
  toHeaders CreateCoreDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateCoreDefinition where
  toJSON CreateCoreDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateCoreDefinition where
  toPath = Core.const "/greengrass/definition/cores"

instance Core.ToQuery CreateCoreDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
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
-- Create a value of 'CreateCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createCoreDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createCoreDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createCoreDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createCoreDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createCoreDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createCoreDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createCoreDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createCoreDefinitionResponse_httpStatus' - The response's http status code.
newCreateCoreDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCoreDefinitionResponse
newCreateCoreDefinitionResponse pHttpStatus_ =
  CreateCoreDefinitionResponse'
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
createCoreDefinitionResponse_creationTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_creationTimestamp = Lens.lens (\CreateCoreDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateCoreDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateCoreDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createCoreDefinitionResponse_latestVersionArn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_latestVersionArn = Lens.lens (\CreateCoreDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateCoreDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateCoreDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createCoreDefinitionResponse_latestVersion :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_latestVersion = Lens.lens (\CreateCoreDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateCoreDefinitionResponse' {} a -> s {latestVersion = a} :: CreateCoreDefinitionResponse)

-- | The ARN of the definition.
createCoreDefinitionResponse_arn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_arn = Lens.lens (\CreateCoreDefinitionResponse' {arn} -> arn) (\s@CreateCoreDefinitionResponse' {} a -> s {arn = a} :: CreateCoreDefinitionResponse)

-- | The ID of the definition.
createCoreDefinitionResponse_id :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_id = Lens.lens (\CreateCoreDefinitionResponse' {id} -> id) (\s@CreateCoreDefinitionResponse' {} a -> s {id = a} :: CreateCoreDefinitionResponse)

-- | The name of the definition.
createCoreDefinitionResponse_name :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_name = Lens.lens (\CreateCoreDefinitionResponse' {name} -> name) (\s@CreateCoreDefinitionResponse' {} a -> s {name = a} :: CreateCoreDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createCoreDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
createCoreDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateCoreDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateCoreDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateCoreDefinitionResponse)

-- | The response's http status code.
createCoreDefinitionResponse_httpStatus :: Lens.Lens' CreateCoreDefinitionResponse Core.Int
createCoreDefinitionResponse_httpStatus = Lens.lens (\CreateCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCoreDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCoreDefinitionResponse)

instance Core.NFData CreateCoreDefinitionResponse
