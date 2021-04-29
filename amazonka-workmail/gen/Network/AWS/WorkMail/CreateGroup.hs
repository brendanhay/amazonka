{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkMail.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group that can be used in Amazon WorkMail by calling the
-- RegisterToWorkMail operation.
module Network.AWS.WorkMail.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_organizationId,
    createGroup_name,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_groupId,
    createGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The organization under which the group is to be created.
    organizationId :: Prelude.Text,
    -- | The name of the group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createGroup_organizationId' - The organization under which the group is to be created.
--
-- 'name', 'createGroup_name' - The name of the group.
newCreateGroup ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateGroup
newCreateGroup pOrganizationId_ pName_ =
  CreateGroup'
    { organizationId = pOrganizationId_,
      name = pName_
    }

-- | The organization under which the group is to be created.
createGroup_organizationId :: Lens.Lens' CreateGroup Prelude.Text
createGroup_organizationId = Lens.lens (\CreateGroup' {organizationId} -> organizationId) (\s@CreateGroup' {} a -> s {organizationId = a} :: CreateGroup)

-- | The name of the group.
createGroup_name :: Lens.Lens' CreateGroup Prelude.Text
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

instance Prelude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Prelude..?> "GroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup

instance Prelude.NFData CreateGroup

instance Prelude.ToHeaders CreateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.CreateGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The identifier of the group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'createGroupResponse_groupId' - The identifier of the group.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { groupId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the group.
createGroupResponse_groupId :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_groupId = Lens.lens (\CreateGroupResponse' {groupId} -> groupId) (\s@CreateGroupResponse' {} a -> s {groupId = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse
