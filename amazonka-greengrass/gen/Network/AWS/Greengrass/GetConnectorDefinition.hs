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
-- Module      : Network.AWS.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Network.AWS.Greengrass.GetConnectorDefinition
  ( -- * Creating a Request
    GetConnectorDefinition (..),
    newGetConnectorDefinition,

    -- * Request Lenses
    getConnectorDefinition_connectorDefinitionId,

    -- * Destructuring the Response
    GetConnectorDefinitionResponse (..),
    newGetConnectorDefinitionResponse,

    -- * Response Lenses
    getConnectorDefinitionResponse_creationTimestamp,
    getConnectorDefinitionResponse_latestVersionArn,
    getConnectorDefinitionResponse_latestVersion,
    getConnectorDefinitionResponse_arn,
    getConnectorDefinitionResponse_id,
    getConnectorDefinitionResponse_name,
    getConnectorDefinitionResponse_lastUpdatedTimestamp,
    getConnectorDefinitionResponse_tags,
    getConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnectorDefinition' smart constructor.
data GetConnectorDefinition = GetConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorDefinitionId', 'getConnectorDefinition_connectorDefinitionId' - The ID of the connector definition.
newGetConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  GetConnectorDefinition
newGetConnectorDefinition pConnectorDefinitionId_ =
  GetConnectorDefinition'
    { connectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
getConnectorDefinition_connectorDefinitionId :: Lens.Lens' GetConnectorDefinition Core.Text
getConnectorDefinition_connectorDefinitionId = Lens.lens (\GetConnectorDefinition' {connectorDefinitionId} -> connectorDefinitionId) (\s@GetConnectorDefinition' {} a -> s {connectorDefinitionId = a} :: GetConnectorDefinition)

instance Core.AWSRequest GetConnectorDefinition where
  type
    AWSResponse GetConnectorDefinition =
      GetConnectorDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
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

instance Core.Hashable GetConnectorDefinition

instance Core.NFData GetConnectorDefinition

instance Core.ToHeaders GetConnectorDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetConnectorDefinition where
  toPath GetConnectorDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/connectors/",
        Core.toBS connectorDefinitionId
      ]

instance Core.ToQuery GetConnectorDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
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
-- Create a value of 'GetConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getConnectorDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getConnectorDefinitionResponse_httpStatus' - The response's http status code.
newGetConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectorDefinitionResponse
newGetConnectorDefinitionResponse pHttpStatus_ =
  GetConnectorDefinitionResponse'
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
getConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_creationTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_latestVersionArn = Lens.lens (\GetConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_latestVersion = Lens.lens (\GetConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the definition.
getConnectorDefinitionResponse_arn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_arn = Lens.lens (\GetConnectorDefinitionResponse' {arn} -> arn) (\s@GetConnectorDefinitionResponse' {} a -> s {arn = a} :: GetConnectorDefinitionResponse)

-- | The ID of the definition.
getConnectorDefinitionResponse_id :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_id = Lens.lens (\GetConnectorDefinitionResponse' {id} -> id) (\s@GetConnectorDefinitionResponse' {} a -> s {id = a} :: GetConnectorDefinitionResponse)

-- | The name of the definition.
getConnectorDefinitionResponse_name :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_name = Lens.lens (\GetConnectorDefinitionResponse' {name} -> name) (\s@GetConnectorDefinitionResponse' {} a -> s {name = a} :: GetConnectorDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
getConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetConnectorDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getConnectorDefinitionResponse_tags :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getConnectorDefinitionResponse_tags = Lens.lens (\GetConnectorDefinitionResponse' {tags} -> tags) (\s@GetConnectorDefinitionResponse' {} a -> s {tags = a} :: GetConnectorDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getConnectorDefinitionResponse_httpStatus :: Lens.Lens' GetConnectorDefinitionResponse Core.Int
getConnectorDefinitionResponse_httpStatus = Lens.lens (\GetConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: GetConnectorDefinitionResponse)

instance Core.NFData GetConnectorDefinitionResponse
