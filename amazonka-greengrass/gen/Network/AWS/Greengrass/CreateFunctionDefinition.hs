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
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda
-- functions and their configurations to be used in a group. You can create
-- an initial version of the definition by providing a list of Lambda
-- functions and their configurations now, or use
-- \'\'CreateFunctionDefinitionVersion\'\' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
  ( -- * Creating a Request
    CreateFunctionDefinition (..),
    newCreateFunctionDefinition,

    -- * Request Lenses
    createFunctionDefinition_name,
    createFunctionDefinition_initialVersion,
    createFunctionDefinition_tags,
    createFunctionDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateFunctionDefinitionResponse (..),
    newCreateFunctionDefinitionResponse,

    -- * Response Lenses
    createFunctionDefinitionResponse_creationTimestamp,
    createFunctionDefinitionResponse_latestVersionArn,
    createFunctionDefinitionResponse_latestVersion,
    createFunctionDefinitionResponse_arn,
    createFunctionDefinitionResponse_id,
    createFunctionDefinitionResponse_name,
    createFunctionDefinitionResponse_lastUpdatedTimestamp,
    createFunctionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { -- | The name of the function definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the function definition.
    initialVersion :: Core.Maybe FunctionDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createFunctionDefinition_name' - The name of the function definition.
--
-- 'initialVersion', 'createFunctionDefinition_initialVersion' - Information about the initial version of the function definition.
--
-- 'tags', 'createFunctionDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createFunctionDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateFunctionDefinition ::
  CreateFunctionDefinition
newCreateFunctionDefinition =
  CreateFunctionDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the function definition.
createFunctionDefinition_name :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
createFunctionDefinition_name = Lens.lens (\CreateFunctionDefinition' {name} -> name) (\s@CreateFunctionDefinition' {} a -> s {name = a} :: CreateFunctionDefinition)

-- | Information about the initial version of the function definition.
createFunctionDefinition_initialVersion :: Lens.Lens' CreateFunctionDefinition (Core.Maybe FunctionDefinitionVersion)
createFunctionDefinition_initialVersion = Lens.lens (\CreateFunctionDefinition' {initialVersion} -> initialVersion) (\s@CreateFunctionDefinition' {} a -> s {initialVersion = a} :: CreateFunctionDefinition)

-- | Tag(s) to add to the new resource.
createFunctionDefinition_tags :: Lens.Lens' CreateFunctionDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createFunctionDefinition_tags = Lens.lens (\CreateFunctionDefinition' {tags} -> tags) (\s@CreateFunctionDefinition' {} a -> s {tags = a} :: CreateFunctionDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createFunctionDefinition_amznClientToken :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
createFunctionDefinition_amznClientToken = Lens.lens (\CreateFunctionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateFunctionDefinition' {} a -> s {amznClientToken = a} :: CreateFunctionDefinition)

instance Core.AWSRequest CreateFunctionDefinition where
  type
    AWSResponse CreateFunctionDefinition =
      CreateFunctionDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateFunctionDefinition

instance Core.NFData CreateFunctionDefinition

instance Core.ToHeaders CreateFunctionDefinition where
  toHeaders CreateFunctionDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateFunctionDefinition where
  toJSON CreateFunctionDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateFunctionDefinition where
  toPath =
    Core.const "/greengrass/definition/functions"

instance Core.ToQuery CreateFunctionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
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
-- Create a value of 'CreateFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createFunctionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createFunctionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createFunctionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createFunctionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createFunctionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createFunctionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createFunctionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createFunctionDefinitionResponse_httpStatus' - The response's http status code.
newCreateFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateFunctionDefinitionResponse
newCreateFunctionDefinitionResponse pHttpStatus_ =
  CreateFunctionDefinitionResponse'
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
createFunctionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_creationTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_latestVersionArn = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_latestVersion = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateFunctionDefinitionResponse)

-- | The ARN of the definition.
createFunctionDefinitionResponse_arn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_arn = Lens.lens (\CreateFunctionDefinitionResponse' {arn} -> arn) (\s@CreateFunctionDefinitionResponse' {} a -> s {arn = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the definition.
createFunctionDefinitionResponse_id :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_id = Lens.lens (\CreateFunctionDefinitionResponse' {id} -> id) (\s@CreateFunctionDefinitionResponse' {} a -> s {id = a} :: CreateFunctionDefinitionResponse)

-- | The name of the definition.
createFunctionDefinitionResponse_name :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_name = Lens.lens (\CreateFunctionDefinitionResponse' {name} -> name) (\s@CreateFunctionDefinitionResponse' {} a -> s {name = a} :: CreateFunctionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createFunctionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
createFunctionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The response's http status code.
createFunctionDefinitionResponse_httpStatus :: Lens.Lens' CreateFunctionDefinitionResponse Core.Int
createFunctionDefinitionResponse_httpStatus = Lens.lens (\CreateFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateFunctionDefinitionResponse)

instance Core.NFData CreateFunctionDefinitionResponse
