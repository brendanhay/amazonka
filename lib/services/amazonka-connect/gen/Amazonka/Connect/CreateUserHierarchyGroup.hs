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
-- Module      : Amazonka.Connect.CreateUserHierarchyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user hierarchy group.
module Amazonka.Connect.CreateUserHierarchyGroup
  ( -- * Creating a Request
    CreateUserHierarchyGroup (..),
    newCreateUserHierarchyGroup,

    -- * Request Lenses
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_tags,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUserHierarchyGroup' smart constructor.
data CreateUserHierarchyGroup = CreateUserHierarchyGroup'
  { -- | The identifier for the parent hierarchy group. The user hierarchy is
    -- created at level one if the parent group ID is null.
    parentGroupId :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the user hierarchy group. Must not be more than 100
    -- characters.
    name :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'tags', 'createUserHierarchyGroup_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'createUserHierarchyGroup_name' - The name of the user hierarchy group. Must not be more than 100
-- characters.
--
-- 'instanceId', 'createUserHierarchyGroup_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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
      tags = Prelude.Nothing,
      name = pName_,
      instanceId = pInstanceId_
    }

-- | The identifier for the parent hierarchy group. The user hierarchy is
-- created at level one if the parent group ID is null.
createUserHierarchyGroup_parentGroupId :: Lens.Lens' CreateUserHierarchyGroup (Prelude.Maybe Prelude.Text)
createUserHierarchyGroup_parentGroupId = Lens.lens (\CreateUserHierarchyGroup' {parentGroupId} -> parentGroupId) (\s@CreateUserHierarchyGroup' {} a -> s {parentGroupId = a} :: CreateUserHierarchyGroup)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createUserHierarchyGroup_tags :: Lens.Lens' CreateUserHierarchyGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUserHierarchyGroup_tags = Lens.lens (\CreateUserHierarchyGroup' {tags} -> tags) (\s@CreateUserHierarchyGroup' {} a -> s {tags = a} :: CreateUserHierarchyGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the user hierarchy group. Must not be more than 100
-- characters.
createUserHierarchyGroup_name :: Lens.Lens' CreateUserHierarchyGroup Prelude.Text
createUserHierarchyGroup_name = Lens.lens (\CreateUserHierarchyGroup' {name} -> name) (\s@CreateUserHierarchyGroup' {} a -> s {name = a} :: CreateUserHierarchyGroup)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createUserHierarchyGroup_instanceId :: Lens.Lens' CreateUserHierarchyGroup Prelude.Text
createUserHierarchyGroup_instanceId = Lens.lens (\CreateUserHierarchyGroup' {instanceId} -> instanceId) (\s@CreateUserHierarchyGroup' {} a -> s {instanceId = a} :: CreateUserHierarchyGroup)

instance Core.AWSRequest CreateUserHierarchyGroup where
  type
    AWSResponse CreateUserHierarchyGroup =
      CreateUserHierarchyGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserHierarchyGroupResponse'
            Prelude.<$> (x Data..?> "HierarchyGroupArn")
            Prelude.<*> (x Data..?> "HierarchyGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserHierarchyGroup where
  hashWithSalt _salt CreateUserHierarchyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` parentGroupId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData CreateUserHierarchyGroup where
  rnf CreateUserHierarchyGroup' {..} =
    Prelude.rnf parentGroupId `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf instanceId

instance Data.ToHeaders CreateUserHierarchyGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserHierarchyGroup where
  toJSON CreateUserHierarchyGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParentGroupId" Data..=) Prelude.<$> parentGroupId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateUserHierarchyGroup where
  toPath CreateUserHierarchyGroup' {..} =
    Prelude.mconcat
      ["/user-hierarchy-groups/", Data.toBS instanceId]

instance Data.ToQuery CreateUserHierarchyGroup where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateUserHierarchyGroupResponse' {..} =
    Prelude.rnf hierarchyGroupArn `Prelude.seq`
      Prelude.rnf hierarchyGroupId `Prelude.seq`
        Prelude.rnf httpStatus
