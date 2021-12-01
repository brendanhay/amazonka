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
-- Module      : Amazonka.IdentityStore.DescribeGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the group metadata and attributes from @GroupId@ in an
-- identity store.
module Amazonka.IdentityStore.DescribeGroup
  ( -- * Creating a Request
    DescribeGroup (..),
    newDescribeGroup,

    -- * Request Lenses
    describeGroup_identityStoreId,
    describeGroup_groupId,

    -- * Destructuring the Response
    DescribeGroupResponse (..),
    newDescribeGroupResponse,

    -- * Response Lenses
    describeGroupResponse_httpStatus,
    describeGroupResponse_groupId,
    describeGroupResponse_displayName,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IdentityStore.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { -- | The globally unique identifier for the identity store, such as
    -- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
    -- @1234567890@ is a randomly generated string that contains number and
    -- lower case letters. This value is generated at the time that a new
    -- identity store is created.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'describeGroup_identityStoreId' - The globally unique identifier for the identity store, such as
-- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
-- @1234567890@ is a randomly generated string that contains number and
-- lower case letters. This value is generated at the time that a new
-- identity store is created.
--
-- 'groupId', 'describeGroup_groupId' - The identifier for a group in the identity store.
newDescribeGroup ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  DescribeGroup
newDescribeGroup pIdentityStoreId_ pGroupId_ =
  DescribeGroup'
    { identityStoreId = pIdentityStoreId_,
      groupId = pGroupId_
    }

-- | The globally unique identifier for the identity store, such as
-- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
-- @1234567890@ is a randomly generated string that contains number and
-- lower case letters. This value is generated at the time that a new
-- identity store is created.
describeGroup_identityStoreId :: Lens.Lens' DescribeGroup Prelude.Text
describeGroup_identityStoreId = Lens.lens (\DescribeGroup' {identityStoreId} -> identityStoreId) (\s@DescribeGroup' {} a -> s {identityStoreId = a} :: DescribeGroup)

-- | The identifier for a group in the identity store.
describeGroup_groupId :: Lens.Lens' DescribeGroup Prelude.Text
describeGroup_groupId = Lens.lens (\DescribeGroup' {groupId} -> groupId) (\s@DescribeGroup' {} a -> s {groupId = a} :: DescribeGroup)

instance Core.AWSRequest DescribeGroup where
  type
    AWSResponse DescribeGroup =
      DescribeGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "GroupId")
            Prelude.<*> (x Core..:> "DisplayName")
      )

instance Prelude.Hashable DescribeGroup where
  hashWithSalt salt' DescribeGroup' {..} =
    salt' `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData DescribeGroup where
  rnf DescribeGroup' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf groupId

instance Core.ToHeaders DescribeGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIdentityStore.DescribeGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGroup where
  toJSON DescribeGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Core..= identityStoreId),
            Prelude.Just ("GroupId" Core..= groupId)
          ]
      )

instance Core.ToPath DescribeGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    -- | Contains the group’s display name value. The length limit is 1,024
    -- characters. This value can consist of letters, accented characters,
    -- symbols, numbers, punctuation, tab, new line, carriage return, space,
    -- and nonbreaking space in this attribute. The characters @\<>;:%@ are
    -- excluded. This value is specified at the time that the group is created
    -- and stored as an attribute of the group object in the identity store.
    displayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeGroupResponse_httpStatus' - The response's http status code.
--
-- 'groupId', 'describeGroupResponse_groupId' - The identifier for a group in the identity store.
--
-- 'displayName', 'describeGroupResponse_displayName' - Contains the group’s display name value. The length limit is 1,024
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, punctuation, tab, new line, carriage return, space,
-- and nonbreaking space in this attribute. The characters @\<>;:%@ are
-- excluded. This value is specified at the time that the group is created
-- and stored as an attribute of the group object in the identity store.
newDescribeGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  DescribeGroupResponse
newDescribeGroupResponse
  pHttpStatus_
  pGroupId_
  pDisplayName_ =
    DescribeGroupResponse'
      { httpStatus = pHttpStatus_,
        groupId = pGroupId_,
        displayName = pDisplayName_
      }

-- | The response's http status code.
describeGroupResponse_httpStatus :: Lens.Lens' DescribeGroupResponse Prelude.Int
describeGroupResponse_httpStatus = Lens.lens (\DescribeGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeGroupResponse' {} a -> s {httpStatus = a} :: DescribeGroupResponse)

-- | The identifier for a group in the identity store.
describeGroupResponse_groupId :: Lens.Lens' DescribeGroupResponse Prelude.Text
describeGroupResponse_groupId = Lens.lens (\DescribeGroupResponse' {groupId} -> groupId) (\s@DescribeGroupResponse' {} a -> s {groupId = a} :: DescribeGroupResponse)

-- | Contains the group’s display name value. The length limit is 1,024
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, punctuation, tab, new line, carriage return, space,
-- and nonbreaking space in this attribute. The characters @\<>;:%@ are
-- excluded. This value is specified at the time that the group is created
-- and stored as an attribute of the group object in the identity store.
describeGroupResponse_displayName :: Lens.Lens' DescribeGroupResponse Prelude.Text
describeGroupResponse_displayName = Lens.lens (\DescribeGroupResponse' {displayName} -> displayName) (\s@DescribeGroupResponse' {} a -> s {displayName = a} :: DescribeGroupResponse)

instance Prelude.NFData DescribeGroupResponse where
  rnf DescribeGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf groupId
