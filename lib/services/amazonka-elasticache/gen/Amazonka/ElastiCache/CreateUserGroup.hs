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
-- Module      : Amazonka.ElastiCache.CreateUserGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user group. For
-- more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>
module Amazonka.ElastiCache.CreateUserGroup
  ( -- * Creating a Request
    CreateUserGroup (..),
    newCreateUserGroup,

    -- * Request Lenses
    createUserGroup_userIds,
    createUserGroup_tags,
    createUserGroup_userGroupId,
    createUserGroup_engine,

    -- * Destructuring the Response
    UserGroup (..),
    newUserGroup,

    -- * Response Lenses
    userGroup_status,
    userGroup_userIds,
    userGroup_arn,
    userGroup_userGroupId,
    userGroup_engine,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUserGroup' smart constructor.
data CreateUserGroup = CreateUserGroup'
  { -- | The list of user IDs that belong to the user group.
    userIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the user group.
    userGroupId :: Prelude.Text,
    -- | The current supported value is Redis.
    engine :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIds', 'createUserGroup_userIds' - The list of user IDs that belong to the user group.
--
-- 'tags', 'createUserGroup_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'userGroupId', 'createUserGroup_userGroupId' - The ID of the user group.
--
-- 'engine', 'createUserGroup_engine' - The current supported value is Redis.
newCreateUserGroup ::
  -- | 'userGroupId'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  CreateUserGroup
newCreateUserGroup pUserGroupId_ pEngine_ =
  CreateUserGroup'
    { userIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      userGroupId = pUserGroupId_,
      engine = pEngine_
    }

-- | The list of user IDs that belong to the user group.
createUserGroup_userIds :: Lens.Lens' CreateUserGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createUserGroup_userIds = Lens.lens (\CreateUserGroup' {userIds} -> userIds) (\s@CreateUserGroup' {} a -> s {userIds = a} :: CreateUserGroup) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createUserGroup_tags :: Lens.Lens' CreateUserGroup (Prelude.Maybe [Tag])
createUserGroup_tags = Lens.lens (\CreateUserGroup' {tags} -> tags) (\s@CreateUserGroup' {} a -> s {tags = a} :: CreateUserGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user group.
createUserGroup_userGroupId :: Lens.Lens' CreateUserGroup Prelude.Text
createUserGroup_userGroupId = Lens.lens (\CreateUserGroup' {userGroupId} -> userGroupId) (\s@CreateUserGroup' {} a -> s {userGroupId = a} :: CreateUserGroup)

-- | The current supported value is Redis.
createUserGroup_engine :: Lens.Lens' CreateUserGroup Prelude.Text
createUserGroup_engine = Lens.lens (\CreateUserGroup' {engine} -> engine) (\s@CreateUserGroup' {} a -> s {engine = a} :: CreateUserGroup)

instance Core.AWSRequest CreateUserGroup where
  type AWSResponse CreateUserGroup = UserGroup
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateUserGroupResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateUserGroup where
  hashWithSalt _salt CreateUserGroup' {..} =
    _salt `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userGroupId
      `Prelude.hashWithSalt` engine

instance Prelude.NFData CreateUserGroup where
  rnf CreateUserGroup' {..} =
    Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userGroupId
      `Prelude.seq` Prelude.rnf engine

instance Core.ToHeaders CreateUserGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateUserGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserGroup where
  toQuery CreateUserGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateUserGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "UserIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> userIds),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "UserGroupId" Core.=: userGroupId,
        "Engine" Core.=: engine
      ]
