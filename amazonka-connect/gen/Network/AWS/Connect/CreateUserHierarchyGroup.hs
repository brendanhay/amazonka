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
-- Module      : Network.AWS.Connect.CreateUserHierarchyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user hierarchy group.
module Network.AWS.Connect.CreateUserHierarchyGroup
  ( -- * Creating a Request
    CreateUserHierarchyGroup (..),
    newCreateUserHierarchyGroup,

    -- * Request Lenses
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_name,
    createUserHierarchyGroup_instanceId,

    -- * Destructuring the Response
    CreateUserHierarchyGroupResponse (..),
    newCreateUserHierarchyGroupResponse,

    -- * Response Lenses
    createUserHierarchyGroupResponse_hierarchyGroupArn,
    createUserHierarchyGroupResponse_hierarchyGroupId,
    createUserHierarchyGroupResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserHierarchyGroup' smart constructor.
data CreateUserHierarchyGroup = CreateUserHierarchyGroup'
  { -- | The identifier for the parent hierarchy group. The user hierarchy is
    -- created at level one if the parent group ID is null.
    parentGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user hierarchy group. Must not be more than 100
    -- characters.
    name :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUserHierarchyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentGroupId', 'createUserHierarchyGroup_parentGroupId' - The identifier for the parent hierarchy group. The user hierarchy is
-- created at level one if the parent group ID is null.
--
-- 'name', 'createUserHierarchyGroup_name' - The name of the user hierarchy group. Must not be more than 100
-- characters.
--
-- 'instanceId', 'createUserHierarchyGroup_instanceId' - The identifier of the Amazon Connect instance.
newCreateUserHierarchyGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  CreateUserHierarchyGroup
newCreateUserHierarchyGroup pName_ pInstanceId_ =
  CreateUserHierarchyGroup'
    { parentGroupId =
        Prelude.Nothing,
      name = pName_,
      instanceId = pInstanceId_
    }

-- | The identifier for the parent hierarchy group. The user hierarchy is
-- created at level one if the parent group ID is null.
createUserHierarchyGroup_parentGroupId :: Lens.Lens' CreateUserHierarchyGroup (Prelude.Maybe Prelude.Text)
createUserHierarchyGroup_parentGroupId = Lens.lens (\CreateUserHierarchyGroup' {parentGroupId} -> parentGroupId) (\s@CreateUserHierarchyGroup' {} a -> s {parentGroupId = a} :: CreateUserHierarchyGroup)

-- | The name of the user hierarchy group. Must not be more than 100
-- characters.
createUserHierarchyGroup_name :: Lens.Lens' CreateUserHierarchyGroup Prelude.Text
createUserHierarchyGroup_name = Lens.lens (\CreateUserHierarchyGroup' {name} -> name) (\s@CreateUserHierarchyGroup' {} a -> s {name = a} :: CreateUserHierarchyGroup)

-- | The identifier of the Amazon Connect instance.
createUserHierarchyGroup_instanceId :: Lens.Lens' CreateUserHierarchyGroup Prelude.Text
createUserHierarchyGroup_instanceId = Lens.lens (\CreateUserHierarchyGroup' {instanceId} -> instanceId) (\s@CreateUserHierarchyGroup' {} a -> s {instanceId = a} :: CreateUserHierarchyGroup)

instance Prelude.AWSRequest CreateUserHierarchyGroup where
  type
    Rs CreateUserHierarchyGroup =
      CreateUserHierarchyGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserHierarchyGroupResponse'
            Prelude.<$> (x Prelude..?> "HierarchyGroupArn")
            Prelude.<*> (x Prelude..?> "HierarchyGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserHierarchyGroup

instance Prelude.NFData CreateUserHierarchyGroup

instance Prelude.ToHeaders CreateUserHierarchyGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUserHierarchyGroup where
  toJSON CreateUserHierarchyGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ParentGroupId" Prelude..=)
              Prelude.<$> parentGroupId,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateUserHierarchyGroup where
  toPath CreateUserHierarchyGroup' {..} =
    Prelude.mconcat
      ["/user-hierarchy-groups/", Prelude.toBS instanceId]

instance Prelude.ToQuery CreateUserHierarchyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserHierarchyGroupResponse' smart constructor.
data CreateUserHierarchyGroupResponse = CreateUserHierarchyGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the hierarchy group.
    hierarchyGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUserHierarchyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupArn', 'createUserHierarchyGroupResponse_hierarchyGroupArn' - The Amazon Resource Name (ARN) of the hierarchy group.
--
-- 'hierarchyGroupId', 'createUserHierarchyGroupResponse_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'httpStatus', 'createUserHierarchyGroupResponse_httpStatus' - The response's http status code.
newCreateUserHierarchyGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserHierarchyGroupResponse
newCreateUserHierarchyGroupResponse pHttpStatus_ =
  CreateUserHierarchyGroupResponse'
    { hierarchyGroupArn =
        Prelude.Nothing,
      hierarchyGroupId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
createUserHierarchyGroupResponse_hierarchyGroupArn :: Lens.Lens' CreateUserHierarchyGroupResponse (Prelude.Maybe Prelude.Text)
createUserHierarchyGroupResponse_hierarchyGroupArn = Lens.lens (\CreateUserHierarchyGroupResponse' {hierarchyGroupArn} -> hierarchyGroupArn) (\s@CreateUserHierarchyGroupResponse' {} a -> s {hierarchyGroupArn = a} :: CreateUserHierarchyGroupResponse)

-- | The identifier of the hierarchy group.
createUserHierarchyGroupResponse_hierarchyGroupId :: Lens.Lens' CreateUserHierarchyGroupResponse (Prelude.Maybe Prelude.Text)
createUserHierarchyGroupResponse_hierarchyGroupId = Lens.lens (\CreateUserHierarchyGroupResponse' {hierarchyGroupId} -> hierarchyGroupId) (\s@CreateUserHierarchyGroupResponse' {} a -> s {hierarchyGroupId = a} :: CreateUserHierarchyGroupResponse)

-- | The response's http status code.
createUserHierarchyGroupResponse_httpStatus :: Lens.Lens' CreateUserHierarchyGroupResponse Prelude.Int
createUserHierarchyGroupResponse_httpStatus = Lens.lens (\CreateUserHierarchyGroupResponse' {httpStatus} -> httpStatus) (\s@CreateUserHierarchyGroupResponse' {} a -> s {httpStatus = a} :: CreateUserHierarchyGroupResponse)

instance
  Prelude.NFData
    CreateUserHierarchyGroupResponse
