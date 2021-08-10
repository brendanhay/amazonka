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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the group.
    initialVersion :: Prelude.Maybe GroupVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the group.
createGroup_name :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

-- | Information about the initial version of the group.
createGroup_initialVersion :: Lens.Lens' CreateGroup (Prelude.Maybe GroupVersion)
createGroup_initialVersion = Lens.lens (\CreateGroup' {initialVersion} -> initialVersion) (\s@CreateGroup' {} a -> s {initialVersion = a} :: CreateGroup)

-- | Tag(s) to add to the new resource.
createGroup_tags :: Lens.Lens' CreateGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGroup_tags = Lens.lens (\CreateGroup' {tags} -> tags) (\s@CreateGroup' {} a -> s {tags = a} :: CreateGroup) Prelude.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createGroup_amznClientToken :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_amznClientToken = Lens.lens (\CreateGroup' {amznClientToken} -> amznClientToken) (\s@CreateGroup' {} a -> s {amznClientToken = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup

instance Prelude.NFData CreateGroup

instance Core.ToHeaders CreateGroup where
  toHeaders CreateGroup' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("InitialVersion" Core..=)
              Prelude.<$> initialVersion,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Prelude.const "/greengrass/groups"

instance Core.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createGroupResponse_creationTimestamp :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_creationTimestamp = Lens.lens (\CreateGroupResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateGroupResponse' {} a -> s {creationTimestamp = a} :: CreateGroupResponse)

-- | The ARN of the latest version associated with the definition.
createGroupResponse_latestVersionArn :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_latestVersionArn = Lens.lens (\CreateGroupResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateGroupResponse' {} a -> s {latestVersionArn = a} :: CreateGroupResponse)

-- | The ID of the latest version associated with the definition.
createGroupResponse_latestVersion :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_latestVersion = Lens.lens (\CreateGroupResponse' {latestVersion} -> latestVersion) (\s@CreateGroupResponse' {} a -> s {latestVersion = a} :: CreateGroupResponse)

-- | The ARN of the definition.
createGroupResponse_arn :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_arn = Lens.lens (\CreateGroupResponse' {arn} -> arn) (\s@CreateGroupResponse' {} a -> s {arn = a} :: CreateGroupResponse)

-- | The ID of the definition.
createGroupResponse_id :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_id = Lens.lens (\CreateGroupResponse' {id} -> id) (\s@CreateGroupResponse' {} a -> s {id = a} :: CreateGroupResponse)

-- | The name of the definition.
createGroupResponse_name :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_name = Lens.lens (\CreateGroupResponse' {name} -> name) (\s@CreateGroupResponse' {} a -> s {name = a} :: CreateGroupResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createGroupResponse_lastUpdatedTimestamp :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_lastUpdatedTimestamp = Lens.lens (\CreateGroupResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateGroupResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse
