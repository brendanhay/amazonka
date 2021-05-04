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
-- Module      : Network.AWS.WorkMail.AssociateMemberToGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the group\'s set.
module Network.AWS.WorkMail.AssociateMemberToGroup
  ( -- * Creating a Request
    AssociateMemberToGroup (..),
    newAssociateMemberToGroup,

    -- * Request Lenses
    associateMemberToGroup_organizationId,
    associateMemberToGroup_groupId,
    associateMemberToGroup_memberId,

    -- * Destructuring the Response
    AssociateMemberToGroupResponse (..),
    newAssociateMemberToGroupResponse,

    -- * Response Lenses
    associateMemberToGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newAssociateMemberToGroup' smart constructor.
data AssociateMemberToGroup = AssociateMemberToGroup'
  { -- | The organization under which the group exists.
    organizationId :: Prelude.Text,
    -- | The group to which the member (user or group) is associated.
    groupId :: Prelude.Text,
    -- | The member (user or group) to associate to the group.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateMemberToGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'associateMemberToGroup_organizationId' - The organization under which the group exists.
--
-- 'groupId', 'associateMemberToGroup_groupId' - The group to which the member (user or group) is associated.
--
-- 'memberId', 'associateMemberToGroup_memberId' - The member (user or group) to associate to the group.
newAssociateMemberToGroup ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  AssociateMemberToGroup
newAssociateMemberToGroup
  pOrganizationId_
  pGroupId_
  pMemberId_ =
    AssociateMemberToGroup'
      { organizationId =
          pOrganizationId_,
        groupId = pGroupId_,
        memberId = pMemberId_
      }

-- | The organization under which the group exists.
associateMemberToGroup_organizationId :: Lens.Lens' AssociateMemberToGroup Prelude.Text
associateMemberToGroup_organizationId = Lens.lens (\AssociateMemberToGroup' {organizationId} -> organizationId) (\s@AssociateMemberToGroup' {} a -> s {organizationId = a} :: AssociateMemberToGroup)

-- | The group to which the member (user or group) is associated.
associateMemberToGroup_groupId :: Lens.Lens' AssociateMemberToGroup Prelude.Text
associateMemberToGroup_groupId = Lens.lens (\AssociateMemberToGroup' {groupId} -> groupId) (\s@AssociateMemberToGroup' {} a -> s {groupId = a} :: AssociateMemberToGroup)

-- | The member (user or group) to associate to the group.
associateMemberToGroup_memberId :: Lens.Lens' AssociateMemberToGroup Prelude.Text
associateMemberToGroup_memberId = Lens.lens (\AssociateMemberToGroup' {memberId} -> memberId) (\s@AssociateMemberToGroup' {} a -> s {memberId = a} :: AssociateMemberToGroup)

instance Prelude.AWSRequest AssociateMemberToGroup where
  type
    Rs AssociateMemberToGroup =
      AssociateMemberToGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateMemberToGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateMemberToGroup

instance Prelude.NFData AssociateMemberToGroup

instance Prelude.ToHeaders AssociateMemberToGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.AssociateMemberToGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateMemberToGroup where
  toJSON AssociateMemberToGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("GroupId" Prelude..= groupId),
            Prelude.Just ("MemberId" Prelude..= memberId)
          ]
      )

instance Prelude.ToPath AssociateMemberToGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateMemberToGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMemberToGroupResponse' smart constructor.
data AssociateMemberToGroupResponse = AssociateMemberToGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateMemberToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateMemberToGroupResponse_httpStatus' - The response's http status code.
newAssociateMemberToGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateMemberToGroupResponse
newAssociateMemberToGroupResponse pHttpStatus_ =
  AssociateMemberToGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateMemberToGroupResponse_httpStatus :: Lens.Lens' AssociateMemberToGroupResponse Prelude.Int
associateMemberToGroupResponse_httpStatus = Lens.lens (\AssociateMemberToGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateMemberToGroupResponse' {} a -> s {httpStatus = a} :: AssociateMemberToGroupResponse)

instance
  Prelude.NFData
    AssociateMemberToGroupResponse
