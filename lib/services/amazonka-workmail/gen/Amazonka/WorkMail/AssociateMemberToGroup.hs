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
-- Module      : Amazonka.WorkMail.AssociateMemberToGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the group\'s set.
module Amazonka.WorkMail.AssociateMemberToGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newAssociateMemberToGroup' smart constructor.
data AssociateMemberToGroup = AssociateMemberToGroup'
  { -- | The organization under which the group exists.
    organizationId :: Prelude.Text,
    -- | The group to which the member (user or group) is associated.
    groupId :: Prelude.Text,
    -- | The member (user or group) to associate to the group.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest AssociateMemberToGroup where
  type
    AWSResponse AssociateMemberToGroup =
      AssociateMemberToGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateMemberToGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateMemberToGroup where
  hashWithSalt _salt AssociateMemberToGroup' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData AssociateMemberToGroup where
  rnf AssociateMemberToGroup' {..} =
    Prelude.rnf organizationId `Prelude.seq`
      Prelude.rnf groupId `Prelude.seq`
        Prelude.rnf memberId

instance Data.ToHeaders AssociateMemberToGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.AssociateMemberToGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateMemberToGroup where
  toJSON AssociateMemberToGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("MemberId" Data..= memberId)
          ]
      )

instance Data.ToPath AssociateMemberToGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateMemberToGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMemberToGroupResponse' smart constructor.
data AssociateMemberToGroupResponse = AssociateMemberToGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AssociateMemberToGroupResponse' {..} =
    Prelude.rnf httpStatus
