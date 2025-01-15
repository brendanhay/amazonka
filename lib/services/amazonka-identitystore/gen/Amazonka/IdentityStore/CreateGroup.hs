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
-- Module      : Amazonka.IdentityStore.CreateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group within the specified identity store.
module Amazonka.IdentityStore.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_description,
    createGroup_displayName,
    createGroup_identityStoreId,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_httpStatus,
    createGroupResponse_groupId,
    createGroupResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | A string containing the description of the group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the name of the group. This value is commonly
    -- displayed when the group is referenced.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createGroup_description' - A string containing the description of the group.
--
-- 'displayName', 'createGroup_displayName' - A string containing the name of the group. This value is commonly
-- displayed when the group is referenced.
--
-- 'identityStoreId', 'createGroup_identityStoreId' - The globally unique identifier for the identity store.
newCreateGroup ::
  -- | 'identityStoreId'
  Prelude.Text ->
  CreateGroup
newCreateGroup pIdentityStoreId_ =
  CreateGroup'
    { description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      identityStoreId = pIdentityStoreId_
    }

-- | A string containing the description of the group.
createGroup_description :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the name of the group. This value is commonly
-- displayed when the group is referenced.
createGroup_displayName :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_displayName = Lens.lens (\CreateGroup' {displayName} -> displayName) (\s@CreateGroup' {} a -> s {displayName = a} :: CreateGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The globally unique identifier for the identity store.
createGroup_identityStoreId :: Lens.Lens' CreateGroup Prelude.Text
createGroup_identityStoreId = Lens.lens (\CreateGroup' {identityStoreId} -> identityStoreId) (\s@CreateGroup' {} a -> s {identityStoreId = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "GroupId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable CreateGroup where
  hashWithSalt _salt CreateGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData CreateGroup where
  rnf CreateGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf displayName `Prelude.seq`
        Prelude.rnf identityStoreId

instance Data.ToHeaders CreateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.CreateGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId)
          ]
      )

instance Data.ToPath CreateGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the newly created group in the identity store.
    groupId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
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
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
--
-- 'groupId', 'createGroupResponse_groupId' - The identifier of the newly created group in the identity store.
--
-- 'identityStoreId', 'createGroupResponse_identityStoreId' - The globally unique identifier for the identity store.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  CreateGroupResponse
newCreateGroupResponse
  pHttpStatus_
  pGroupId_
  pIdentityStoreId_ =
    CreateGroupResponse'
      { httpStatus = pHttpStatus_,
        groupId = pGroupId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

-- | The identifier of the newly created group in the identity store.
createGroupResponse_groupId :: Lens.Lens' CreateGroupResponse Prelude.Text
createGroupResponse_groupId = Lens.lens (\CreateGroupResponse' {groupId} -> groupId) (\s@CreateGroupResponse' {} a -> s {groupId = a} :: CreateGroupResponse)

-- | The globally unique identifier for the identity store.
createGroupResponse_identityStoreId :: Lens.Lens' CreateGroupResponse Prelude.Text
createGroupResponse_identityStoreId = Lens.lens (\CreateGroupResponse' {identityStoreId} -> identityStoreId) (\s@CreateGroupResponse' {} a -> s {identityStoreId = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse where
  rnf CreateGroupResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf groupId `Prelude.seq`
        Prelude.rnf identityStoreId
