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
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition.
module Network.AWS.Greengrass.GetSubscriptionDefinition
  ( -- * Creating a Request
    GetSubscriptionDefinition (..),
    newGetSubscriptionDefinition,

    -- * Request Lenses
    getSubscriptionDefinition_subscriptionDefinitionId,

    -- * Destructuring the Response
    GetSubscriptionDefinitionResponse (..),
    newGetSubscriptionDefinitionResponse,

    -- * Response Lenses
    getSubscriptionDefinitionResponse_creationTimestamp,
    getSubscriptionDefinitionResponse_latestVersionArn,
    getSubscriptionDefinitionResponse_latestVersion,
    getSubscriptionDefinitionResponse_arn,
    getSubscriptionDefinitionResponse_id,
    getSubscriptionDefinitionResponse_name,
    getSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    getSubscriptionDefinitionResponse_tags,
    getSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSubscriptionDefinition' smart constructor.
data GetSubscriptionDefinition = GetSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSubscriptionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionDefinitionId', 'getSubscriptionDefinition_subscriptionDefinitionId' - The ID of the subscription definition.
newGetSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  GetSubscriptionDefinition
newGetSubscriptionDefinition
  pSubscriptionDefinitionId_ =
    GetSubscriptionDefinition'
      { subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The ID of the subscription definition.
getSubscriptionDefinition_subscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinition Core.Text
getSubscriptionDefinition_subscriptionDefinitionId = Lens.lens (\GetSubscriptionDefinition' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@GetSubscriptionDefinition' {} a -> s {subscriptionDefinitionId = a} :: GetSubscriptionDefinition)

instance Core.AWSRequest GetSubscriptionDefinition where
  type
    AWSResponse GetSubscriptionDefinition =
      GetSubscriptionDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionResponse'
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

instance Core.Hashable GetSubscriptionDefinition

instance Core.NFData GetSubscriptionDefinition

instance Core.ToHeaders GetSubscriptionDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSubscriptionDefinition where
  toPath GetSubscriptionDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId
      ]

instance Core.ToQuery GetSubscriptionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSubscriptionDefinitionResponse' smart constructor.
data GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse'
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
-- Create a value of 'GetSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getSubscriptionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getSubscriptionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getSubscriptionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getSubscriptionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getSubscriptionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getSubscriptionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getSubscriptionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getSubscriptionDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newGetSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSubscriptionDefinitionResponse
newGetSubscriptionDefinitionResponse pHttpStatus_ =
  GetSubscriptionDefinitionResponse'
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
getSubscriptionDefinitionResponse_creationTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_creationTimestamp = Lens.lens (\GetSubscriptionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetSubscriptionDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetSubscriptionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getSubscriptionDefinitionResponse_latestVersionArn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_latestVersionArn = Lens.lens (\GetSubscriptionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetSubscriptionDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetSubscriptionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getSubscriptionDefinitionResponse_latestVersion :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_latestVersion = Lens.lens (\GetSubscriptionDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetSubscriptionDefinitionResponse' {} a -> s {latestVersion = a} :: GetSubscriptionDefinitionResponse)

-- | The ARN of the definition.
getSubscriptionDefinitionResponse_arn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_arn = Lens.lens (\GetSubscriptionDefinitionResponse' {arn} -> arn) (\s@GetSubscriptionDefinitionResponse' {} a -> s {arn = a} :: GetSubscriptionDefinitionResponse)

-- | The ID of the definition.
getSubscriptionDefinitionResponse_id :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_id = Lens.lens (\GetSubscriptionDefinitionResponse' {id} -> id) (\s@GetSubscriptionDefinitionResponse' {} a -> s {id = a} :: GetSubscriptionDefinitionResponse)

-- | The name of the definition.
getSubscriptionDefinitionResponse_name :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_name = Lens.lens (\GetSubscriptionDefinitionResponse' {name} -> name) (\s@GetSubscriptionDefinitionResponse' {} a -> s {name = a} :: GetSubscriptionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getSubscriptionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
getSubscriptionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetSubscriptionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetSubscriptionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetSubscriptionDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getSubscriptionDefinitionResponse_tags :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getSubscriptionDefinitionResponse_tags = Lens.lens (\GetSubscriptionDefinitionResponse' {tags} -> tags) (\s@GetSubscriptionDefinitionResponse' {} a -> s {tags = a} :: GetSubscriptionDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' GetSubscriptionDefinitionResponse Core.Int
getSubscriptionDefinitionResponse_httpStatus = Lens.lens (\GetSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: GetSubscriptionDefinitionResponse)

instance
  Core.NFData
    GetSubscriptionDefinitionResponse
