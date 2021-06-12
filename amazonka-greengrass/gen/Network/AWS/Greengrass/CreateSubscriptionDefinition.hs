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
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version
-- of the subscription definition now or use
-- \'\'CreateSubscriptionDefinitionVersion\'\' at a later time.
module Network.AWS.Greengrass.CreateSubscriptionDefinition
  ( -- * Creating a Request
    CreateSubscriptionDefinition (..),
    newCreateSubscriptionDefinition,

    -- * Request Lenses
    createSubscriptionDefinition_name,
    createSubscriptionDefinition_initialVersion,
    createSubscriptionDefinition_tags,
    createSubscriptionDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateSubscriptionDefinitionResponse (..),
    newCreateSubscriptionDefinitionResponse,

    -- * Response Lenses
    createSubscriptionDefinitionResponse_creationTimestamp,
    createSubscriptionDefinitionResponse_latestVersionArn,
    createSubscriptionDefinitionResponse_latestVersion,
    createSubscriptionDefinitionResponse_arn,
    createSubscriptionDefinitionResponse_id,
    createSubscriptionDefinitionResponse_name,
    createSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    createSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { -- | The name of the subscription definition.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the subscription definition.
    initialVersion :: Core.Maybe SubscriptionDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSubscriptionDefinition_name' - The name of the subscription definition.
--
-- 'initialVersion', 'createSubscriptionDefinition_initialVersion' - Information about the initial version of the subscription definition.
--
-- 'tags', 'createSubscriptionDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createSubscriptionDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateSubscriptionDefinition ::
  CreateSubscriptionDefinition
newCreateSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the subscription definition.
createSubscriptionDefinition_name :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe Core.Text)
createSubscriptionDefinition_name = Lens.lens (\CreateSubscriptionDefinition' {name} -> name) (\s@CreateSubscriptionDefinition' {} a -> s {name = a} :: CreateSubscriptionDefinition)

-- | Information about the initial version of the subscription definition.
createSubscriptionDefinition_initialVersion :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe SubscriptionDefinitionVersion)
createSubscriptionDefinition_initialVersion = Lens.lens (\CreateSubscriptionDefinition' {initialVersion} -> initialVersion) (\s@CreateSubscriptionDefinition' {} a -> s {initialVersion = a} :: CreateSubscriptionDefinition)

-- | Tag(s) to add to the new resource.
createSubscriptionDefinition_tags :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
createSubscriptionDefinition_tags = Lens.lens (\CreateSubscriptionDefinition' {tags} -> tags) (\s@CreateSubscriptionDefinition' {} a -> s {tags = a} :: CreateSubscriptionDefinition) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createSubscriptionDefinition_amznClientToken :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe Core.Text)
createSubscriptionDefinition_amznClientToken = Lens.lens (\CreateSubscriptionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinition' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinition)

instance Core.AWSRequest CreateSubscriptionDefinition where
  type
    AWSResponse CreateSubscriptionDefinition =
      CreateSubscriptionDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSubscriptionDefinition

instance Core.NFData CreateSubscriptionDefinition

instance Core.ToHeaders CreateSubscriptionDefinition where
  toHeaders CreateSubscriptionDefinition' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateSubscriptionDefinition where
  toJSON CreateSubscriptionDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateSubscriptionDefinition where
  toPath =
    Core.const "/greengrass/definition/subscriptions"

instance Core.ToQuery CreateSubscriptionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
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
-- Create a value of 'CreateSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createSubscriptionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createSubscriptionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createSubscriptionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createSubscriptionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createSubscriptionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createSubscriptionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createSubscriptionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newCreateSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSubscriptionDefinitionResponse
newCreateSubscriptionDefinitionResponse pHttpStatus_ =
  CreateSubscriptionDefinitionResponse'
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
createSubscriptionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_latestVersionArn = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersion :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_latestVersion = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the definition.
createSubscriptionDefinitionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the definition.
createSubscriptionDefinitionResponse_id :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_id = Lens.lens (\CreateSubscriptionDefinitionResponse' {id} -> id) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {id = a} :: CreateSubscriptionDefinitionResponse)

-- | The name of the definition.
createSubscriptionDefinitionResponse_name :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_name = Lens.lens (\CreateSubscriptionDefinitionResponse' {name} -> name) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {name = a} :: CreateSubscriptionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createSubscriptionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The response's http status code.
createSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' CreateSubscriptionDefinitionResponse Core.Int
createSubscriptionDefinitionResponse_httpStatus = Lens.lens (\CreateSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionDefinitionResponse)

instance
  Core.NFData
    CreateSubscriptionDefinitionResponse
