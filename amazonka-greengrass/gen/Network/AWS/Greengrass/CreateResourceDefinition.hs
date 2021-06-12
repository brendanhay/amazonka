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
-- Module      : Network.AWS.Greengrass.CreateResourceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource definition which contains a list of resources to be
-- used in a group. You can create an initial version of the definition by
-- providing a list of resources now, or use
-- \'\'CreateResourceDefinitionVersion\'\' later.
module Network.AWS.Greengrass.CreateResourceDefinition
  ( -- * Creating a Request
    CreateResourceDefinition (..),
    newCreateResourceDefinition,

    -- * Request Lenses
    createResourceDefinition_name,
    createResourceDefinition_initialVersion,
    createResourceDefinition_tags,
    createResourceDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateResourceDefinitionResponse (..),
    newCreateResourceDefinitionResponse,

    -- * Response Lenses
    createResourceDefinitionResponse_creationTimestamp,
    createResourceDefinitionResponse_latestVersionArn,
    createResourceDefinitionResponse_latestVersion,
    createResourceDefinitionResponse_arn,
    createResourceDefinitionResponse_id,
    createResourceDefinitionResponse_name,
    createResourceDefinitionResponse_lastUpdatedTimestamp,
    createResourceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResourceDefinition' smart constructor.
data CreateResourceDefinition = CreateResourceDefinition'
  { -- | The name of the resource definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the resource definition.
    initialVersion :: Core.Maybe ResourceDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createResourceDefinition_name' - The name of the resource definition.
--
-- 'initialVersion', 'createResourceDefinition_initialVersion' - Information about the initial version of the resource definition.
--
-- 'tags', 'createResourceDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createResourceDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateResourceDefinition ::
  CreateResourceDefinition
newCreateResourceDefinition =
  CreateResourceDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the resource definition.
createResourceDefinition_name :: Lens.Lens' CreateResourceDefinition (Core.Maybe Core.Text)
createResourceDefinition_name = Lens.lens (\CreateResourceDefinition' {name} -> name) (\s@CreateResourceDefinition' {} a -> s {name = a} :: CreateResourceDefinition)

-- | Information about the initial version of the resource definition.
createResourceDefinition_initialVersion :: Lens.Lens' CreateResourceDefinition (Core.Maybe ResourceDefinitionVersion)
createResourceDefinition_initialVersion = Lens.lens (\CreateResourceDefinition' {initialVersion} -> initialVersion) (\s@CreateResourceDefinition' {} a -> s {initialVersion = a} :: CreateResourceDefinition)

-- | Tag(s) to add to the new resource.
createResourceDefinition_tags :: Lens.Lens' CreateResourceDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createResourceDefinition_tags = Lens.lens (\CreateResourceDefinition' {tags} -> tags) (\s@CreateResourceDefinition' {} a -> s {tags = a} :: CreateResourceDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createResourceDefinition_amznClientToken :: Lens.Lens' CreateResourceDefinition (Core.Maybe Core.Text)
createResourceDefinition_amznClientToken = Lens.lens (\CreateResourceDefinition' {amznClientToken} -> amznClientToken) (\s@CreateResourceDefinition' {} a -> s {amznClientToken = a} :: CreateResourceDefinition)

instance Core.AWSRequest CreateResourceDefinition where
  type
    AWSResponse CreateResourceDefinition =
      CreateResourceDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateResourceDefinition

instance Core.NFData CreateResourceDefinition

instance Core.ToHeaders CreateResourceDefinition where
  toHeaders CreateResourceDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateResourceDefinition where
  toJSON CreateResourceDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateResourceDefinition where
  toPath =
    Core.const "/greengrass/definition/resources"

instance Core.ToQuery CreateResourceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateResourceDefinitionResponse' smart constructor.
data CreateResourceDefinitionResponse = CreateResourceDefinitionResponse'
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
-- Create a value of 'CreateResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createResourceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createResourceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createResourceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createResourceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createResourceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createResourceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createResourceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createResourceDefinitionResponse_httpStatus' - The response's http status code.
newCreateResourceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateResourceDefinitionResponse
newCreateResourceDefinitionResponse pHttpStatus_ =
  CreateResourceDefinitionResponse'
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
createResourceDefinitionResponse_creationTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_creationTimestamp = Lens.lens (\CreateResourceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateResourceDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateResourceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createResourceDefinitionResponse_latestVersionArn :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_latestVersionArn = Lens.lens (\CreateResourceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateResourceDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateResourceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createResourceDefinitionResponse_latestVersion :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_latestVersion = Lens.lens (\CreateResourceDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateResourceDefinitionResponse' {} a -> s {latestVersion = a} :: CreateResourceDefinitionResponse)

-- | The ARN of the definition.
createResourceDefinitionResponse_arn :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_arn = Lens.lens (\CreateResourceDefinitionResponse' {arn} -> arn) (\s@CreateResourceDefinitionResponse' {} a -> s {arn = a} :: CreateResourceDefinitionResponse)

-- | The ID of the definition.
createResourceDefinitionResponse_id :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_id = Lens.lens (\CreateResourceDefinitionResponse' {id} -> id) (\s@CreateResourceDefinitionResponse' {} a -> s {id = a} :: CreateResourceDefinitionResponse)

-- | The name of the definition.
createResourceDefinitionResponse_name :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_name = Lens.lens (\CreateResourceDefinitionResponse' {name} -> name) (\s@CreateResourceDefinitionResponse' {} a -> s {name = a} :: CreateResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createResourceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
createResourceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateResourceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateResourceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateResourceDefinitionResponse)

-- | The response's http status code.
createResourceDefinitionResponse_httpStatus :: Lens.Lens' CreateResourceDefinitionResponse Core.Int
createResourceDefinitionResponse_httpStatus = Lens.lens (\CreateResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateResourceDefinitionResponse' {} a -> s {httpStatus = a} :: CreateResourceDefinitionResponse)

instance Core.NFData CreateResourceDefinitionResponse
