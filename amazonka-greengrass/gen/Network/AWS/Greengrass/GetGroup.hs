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
-- Module      : Network.AWS.Greengrass.GetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group.
module Network.AWS.Greengrass.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_groupId,

    -- * Destructuring the Response
    GetGroupResponse (..),
    newGetGroupResponse,

    -- * Response Lenses
    getGroupResponse_creationTimestamp,
    getGroupResponse_latestVersionArn,
    getGroupResponse_latestVersion,
    getGroupResponse_arn,
    getGroupResponse_id,
    getGroupResponse_name,
    getGroupResponse_lastUpdatedTimestamp,
    getGroupResponse_tags,
    getGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getGroup_groupId' - The ID of the Greengrass group.
newGetGroup ::
  -- | 'groupId'
  Core.Text ->
  GetGroup
newGetGroup pGroupId_ =
  GetGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
getGroup_groupId :: Lens.Lens' GetGroup Core.Text
getGroup_groupId = Lens.lens (\GetGroup' {groupId} -> groupId) (\s@GetGroup' {} a -> s {groupId = a} :: GetGroup)

instance Core.AWSRequest GetGroup where
  type AWSResponse GetGroup = GetGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
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

instance Core.Hashable GetGroup

instance Core.NFData GetGroup

instance Core.ToHeaders GetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetGroup where
  toPath GetGroup' {..} =
    Core.mconcat
      ["/greengrass/groups/", Core.toBS groupId]

instance Core.ToQuery GetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
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
-- Create a value of 'GetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getGroupResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getGroupResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getGroupResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getGroupResponse_arn' - The ARN of the definition.
--
-- 'id', 'getGroupResponse_id' - The ID of the definition.
--
-- 'name', 'getGroupResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getGroupResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getGroupResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getGroupResponse_httpStatus' - The response's http status code.
newGetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ =
  GetGroupResponse'
    { creationTimestamp = Core.Nothing,
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
getGroupResponse_creationTimestamp :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_creationTimestamp = Lens.lens (\GetGroupResponse' {creationTimestamp} -> creationTimestamp) (\s@GetGroupResponse' {} a -> s {creationTimestamp = a} :: GetGroupResponse)

-- | The ARN of the latest version associated with the definition.
getGroupResponse_latestVersionArn :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_latestVersionArn = Lens.lens (\GetGroupResponse' {latestVersionArn} -> latestVersionArn) (\s@GetGroupResponse' {} a -> s {latestVersionArn = a} :: GetGroupResponse)

-- | The ID of the latest version associated with the definition.
getGroupResponse_latestVersion :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_latestVersion = Lens.lens (\GetGroupResponse' {latestVersion} -> latestVersion) (\s@GetGroupResponse' {} a -> s {latestVersion = a} :: GetGroupResponse)

-- | The ARN of the definition.
getGroupResponse_arn :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_arn = Lens.lens (\GetGroupResponse' {arn} -> arn) (\s@GetGroupResponse' {} a -> s {arn = a} :: GetGroupResponse)

-- | The ID of the definition.
getGroupResponse_id :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_id = Lens.lens (\GetGroupResponse' {id} -> id) (\s@GetGroupResponse' {} a -> s {id = a} :: GetGroupResponse)

-- | The name of the definition.
getGroupResponse_name :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_name = Lens.lens (\GetGroupResponse' {name} -> name) (\s@GetGroupResponse' {} a -> s {name = a} :: GetGroupResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getGroupResponse_lastUpdatedTimestamp :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Text)
getGroupResponse_lastUpdatedTimestamp = Lens.lens (\GetGroupResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetGroupResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetGroupResponse)

-- | Tag(s) attached to the resource arn.
getGroupResponse_tags :: Lens.Lens' GetGroupResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getGroupResponse_tags = Lens.lens (\GetGroupResponse' {tags} -> tags) (\s@GetGroupResponse' {} a -> s {tags = a} :: GetGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Core.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Core.NFData GetGroupResponse
