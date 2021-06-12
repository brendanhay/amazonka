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
-- Module      : Network.AWS.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its
-- creation time and latest version.
module Network.AWS.Greengrass.GetResourceDefinition
  ( -- * Creating a Request
    GetResourceDefinition (..),
    newGetResourceDefinition,

    -- * Request Lenses
    getResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    GetResourceDefinitionResponse (..),
    newGetResourceDefinitionResponse,

    -- * Response Lenses
    getResourceDefinitionResponse_creationTimestamp,
    getResourceDefinitionResponse_latestVersionArn,
    getResourceDefinitionResponse_latestVersion,
    getResourceDefinitionResponse_arn,
    getResourceDefinitionResponse_id,
    getResourceDefinitionResponse_name,
    getResourceDefinitionResponse_lastUpdatedTimestamp,
    getResourceDefinitionResponse_tags,
    getResourceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourceDefinition' smart constructor.
data GetResourceDefinition = GetResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionId', 'getResourceDefinition_resourceDefinitionId' - The ID of the resource definition.
newGetResourceDefinition ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  GetResourceDefinition
newGetResourceDefinition pResourceDefinitionId_ =
  GetResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
getResourceDefinition_resourceDefinitionId :: Lens.Lens' GetResourceDefinition Core.Text
getResourceDefinition_resourceDefinitionId = Lens.lens (\GetResourceDefinition' {resourceDefinitionId} -> resourceDefinitionId) (\s@GetResourceDefinition' {} a -> s {resourceDefinitionId = a} :: GetResourceDefinition)

instance Core.AWSRequest GetResourceDefinition where
  type
    AWSResponse GetResourceDefinition =
      GetResourceDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionResponse'
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

instance Core.Hashable GetResourceDefinition

instance Core.NFData GetResourceDefinition

instance Core.ToHeaders GetResourceDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetResourceDefinition where
  toPath GetResourceDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId
      ]

instance Core.ToQuery GetResourceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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
-- Create a value of 'GetResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getResourceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getResourceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getResourceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getResourceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getResourceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getResourceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getResourceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getResourceDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getResourceDefinitionResponse_httpStatus' - The response's http status code.
newGetResourceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetResourceDefinitionResponse
newGetResourceDefinitionResponse pHttpStatus_ =
  GetResourceDefinitionResponse'
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
getResourceDefinitionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersionArn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_latestVersionArn = Lens.lens (\GetResourceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetResourceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersion :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_latestVersion = Lens.lens (\GetResourceDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersion = a} :: GetResourceDefinitionResponse)

-- | The ARN of the definition.
getResourceDefinitionResponse_arn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_arn = Lens.lens (\GetResourceDefinitionResponse' {arn} -> arn) (\s@GetResourceDefinitionResponse' {} a -> s {arn = a} :: GetResourceDefinitionResponse)

-- | The ID of the definition.
getResourceDefinitionResponse_id :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_id = Lens.lens (\GetResourceDefinitionResponse' {id} -> id) (\s@GetResourceDefinitionResponse' {} a -> s {id = a} :: GetResourceDefinitionResponse)

-- | The name of the definition.
getResourceDefinitionResponse_name :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_name = Lens.lens (\GetResourceDefinitionResponse' {name} -> name) (\s@GetResourceDefinitionResponse' {} a -> s {name = a} :: GetResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getResourceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
getResourceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetResourceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetResourceDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getResourceDefinitionResponse_tags :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getResourceDefinitionResponse_tags = Lens.lens (\GetResourceDefinitionResponse' {tags} -> tags) (\s@GetResourceDefinitionResponse' {} a -> s {tags = a} :: GetResourceDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getResourceDefinitionResponse_httpStatus :: Lens.Lens' GetResourceDefinitionResponse Core.Int
getResourceDefinitionResponse_httpStatus = Lens.lens (\GetResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetResourceDefinitionResponse' {} a -> s {httpStatus = a} :: GetResourceDefinitionResponse)

instance Core.NFData GetResourceDefinitionResponse
