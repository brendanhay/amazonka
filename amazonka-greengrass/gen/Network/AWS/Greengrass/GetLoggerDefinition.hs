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
-- Module      : Network.AWS.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Network.AWS.Greengrass.GetLoggerDefinition
  ( -- * Creating a Request
    GetLoggerDefinition (..),
    newGetLoggerDefinition,

    -- * Request Lenses
    getLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    GetLoggerDefinitionResponse (..),
    newGetLoggerDefinitionResponse,

    -- * Response Lenses
    getLoggerDefinitionResponse_creationTimestamp,
    getLoggerDefinitionResponse_latestVersionArn,
    getLoggerDefinitionResponse_latestVersion,
    getLoggerDefinitionResponse_arn,
    getLoggerDefinitionResponse_id,
    getLoggerDefinitionResponse_name,
    getLoggerDefinitionResponse_lastUpdatedTimestamp,
    getLoggerDefinitionResponse_tags,
    getLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoggerDefinition' smart constructor.
data GetLoggerDefinition = GetLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggerDefinitionId', 'getLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newGetLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  GetLoggerDefinition
newGetLoggerDefinition pLoggerDefinitionId_ =
  GetLoggerDefinition'
    { loggerDefinitionId =
        pLoggerDefinitionId_
    }

-- | The ID of the logger definition.
getLoggerDefinition_loggerDefinitionId :: Lens.Lens' GetLoggerDefinition Core.Text
getLoggerDefinition_loggerDefinitionId = Lens.lens (\GetLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@GetLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: GetLoggerDefinition)

instance Core.AWSRequest GetLoggerDefinition where
  type
    AWSResponse GetLoggerDefinition =
      GetLoggerDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
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

instance Core.Hashable GetLoggerDefinition

instance Core.NFData GetLoggerDefinition

instance Core.ToHeaders GetLoggerDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetLoggerDefinition where
  toPath GetLoggerDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId
      ]

instance Core.ToQuery GetLoggerDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
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
-- Create a value of 'GetLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getLoggerDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getLoggerDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getLoggerDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getLoggerDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getLoggerDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getLoggerDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getLoggerDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getLoggerDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getLoggerDefinitionResponse_httpStatus' - The response's http status code.
newGetLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLoggerDefinitionResponse
newGetLoggerDefinitionResponse pHttpStatus_ =
  GetLoggerDefinitionResponse'
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
getLoggerDefinitionResponse_creationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_creationTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetLoggerDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersionArn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_latestVersionArn = Lens.lens (\GetLoggerDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetLoggerDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_latestVersion = Lens.lens (\GetLoggerDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersion = a} :: GetLoggerDefinitionResponse)

-- | The ARN of the definition.
getLoggerDefinitionResponse_arn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_arn = Lens.lens (\GetLoggerDefinitionResponse' {arn} -> arn) (\s@GetLoggerDefinitionResponse' {} a -> s {arn = a} :: GetLoggerDefinitionResponse)

-- | The ID of the definition.
getLoggerDefinitionResponse_id :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_id = Lens.lens (\GetLoggerDefinitionResponse' {id} -> id) (\s@GetLoggerDefinitionResponse' {} a -> s {id = a} :: GetLoggerDefinitionResponse)

-- | The name of the definition.
getLoggerDefinitionResponse_name :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_name = Lens.lens (\GetLoggerDefinitionResponse' {name} -> name) (\s@GetLoggerDefinitionResponse' {} a -> s {name = a} :: GetLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getLoggerDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
getLoggerDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetLoggerDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getLoggerDefinitionResponse_tags :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getLoggerDefinitionResponse_tags = Lens.lens (\GetLoggerDefinitionResponse' {tags} -> tags) (\s@GetLoggerDefinitionResponse' {} a -> s {tags = a} :: GetLoggerDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLoggerDefinitionResponse_httpStatus :: Lens.Lens' GetLoggerDefinitionResponse Core.Int
getLoggerDefinitionResponse_httpStatus = Lens.lens (\GetLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: GetLoggerDefinitionResponse)

instance Core.NFData GetLoggerDefinitionResponse
