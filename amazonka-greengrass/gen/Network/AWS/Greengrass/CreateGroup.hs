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
-- Module      : Network.AWS.Greengrass.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group. You may provide the initial version of the group or use
-- \'\'CreateGroupVersion\'\' at a later time. Tip: You can use the
-- \'\'gg_group_setup\'\' package
-- (https:\/\/github.com\/awslabs\/aws-greengrass-group-setup) as a library
-- or command-line application to create and deploy Greengrass groups.
module Network.AWS.Greengrass.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_name,
    createGroup_initialVersion,
    createGroup_tags,
    createGroup_amznClientToken,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_creationTimestamp,
    createGroupResponse_latestVersionArn,
    createGroupResponse_latestVersion,
    createGroupResponse_arn,
    createGroupResponse_id,
    createGroupResponse_name,
    createGroupResponse_lastUpdatedTimestamp,
    createGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The name of the group.
    name :: Core.Maybe Core.Text,
    -- | Information about the initial version of the group.
    initialVersion :: Core.Maybe GroupVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createGroup_name' - The name of the group.
--
-- 'initialVersion', 'createGroup_initialVersion' - Information about the initial version of the group.
--
-- 'tags', 'createGroup_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createGroup_amznClientToken' - A client token used to correlate requests and responses.
newCreateGroup ::
  CreateGroup
newCreateGroup =
  CreateGroup'
    { name = Core.Nothing,
      initialVersion = Core.Nothing,
      tags = Core.Nothing,
      amznClientToken = Core.Nothing
    }

-- | The name of the group.
createGroup_name :: Lens.Lens' CreateGroup (Core.Maybe Core.Text)
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

-- | Information about the initial version of the group.
createGroup_initialVersion :: Lens.Lens' CreateGroup (Core.Maybe GroupVersion)
createGroup_initialVersion = Lens.lens (\CreateGroup' {initialVersion} -> initialVersion) (\s@CreateGroup' {} a -> s {initialVersion = a} :: CreateGroup)

-- | Tag(s) to add to the new resource.
createGroup_tags :: Lens.Lens' CreateGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
createGroup_tags = Lens.lens (\CreateGroup' {tags} -> tags) (\s@CreateGroup' {} a -> s {tags = a} :: CreateGroup) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createGroup_amznClientToken :: Lens.Lens' CreateGroup (Core.Maybe Core.Text)
createGroup_amznClientToken = Lens.lens (\CreateGroup' {amznClientToken} -> amznClientToken) (\s@CreateGroup' {} a -> s {amznClientToken = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "LatestVersionArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastUpdatedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGroup

instance Core.NFData CreateGroup

instance Core.ToHeaders CreateGroup where
  toHeaders CreateGroup' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Core.const "/greengrass/groups"

instance Core.ToQuery CreateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
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
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createGroupResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createGroupResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createGroupResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createGroupResponse_arn' - The ARN of the definition.
--
-- 'id', 'createGroupResponse_id' - The ID of the definition.
--
-- 'name', 'createGroupResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createGroupResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
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
createGroupResponse_creationTimestamp :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_creationTimestamp = Lens.lens (\CreateGroupResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateGroupResponse' {} a -> s {creationTimestamp = a} :: CreateGroupResponse)

-- | The ARN of the latest version associated with the definition.
createGroupResponse_latestVersionArn :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_latestVersionArn = Lens.lens (\CreateGroupResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateGroupResponse' {} a -> s {latestVersionArn = a} :: CreateGroupResponse)

-- | The ID of the latest version associated with the definition.
createGroupResponse_latestVersion :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_latestVersion = Lens.lens (\CreateGroupResponse' {latestVersion} -> latestVersion) (\s@CreateGroupResponse' {} a -> s {latestVersion = a} :: CreateGroupResponse)

-- | The ARN of the definition.
createGroupResponse_arn :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_arn = Lens.lens (\CreateGroupResponse' {arn} -> arn) (\s@CreateGroupResponse' {} a -> s {arn = a} :: CreateGroupResponse)

-- | The ID of the definition.
createGroupResponse_id :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_id = Lens.lens (\CreateGroupResponse' {id} -> id) (\s@CreateGroupResponse' {} a -> s {id = a} :: CreateGroupResponse)

-- | The name of the definition.
createGroupResponse_name :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_name = Lens.lens (\CreateGroupResponse' {name} -> name) (\s@CreateGroupResponse' {} a -> s {name = a} :: CreateGroupResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createGroupResponse_lastUpdatedTimestamp :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_lastUpdatedTimestamp = Lens.lens (\CreateGroupResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateGroupResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Core.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Core.NFData CreateGroupResponse
