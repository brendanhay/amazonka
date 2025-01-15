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
-- Module      : Amazonka.WorkMail.DisassociateMemberFromGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from a group.
module Amazonka.WorkMail.DisassociateMemberFromGroup
  ( -- * Creating a Request
    DisassociateMemberFromGroup (..),
    newDisassociateMemberFromGroup,

    -- * Request Lenses
    disassociateMemberFromGroup_organizationId,
    disassociateMemberFromGroup_groupId,
    disassociateMemberFromGroup_memberId,

    -- * Destructuring the Response
    DisassociateMemberFromGroupResponse (..),
    newDisassociateMemberFromGroupResponse,

    -- * Response Lenses
    disassociateMemberFromGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDisassociateMemberFromGroup' smart constructor.
data DisassociateMemberFromGroup = DisassociateMemberFromGroup'
  { -- | The identifier for the organization under which the group exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the group from which members are removed.
    groupId :: Prelude.Text,
    -- | The identifier for the member to be removed to the group.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberFromGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'disassociateMemberFromGroup_organizationId' - The identifier for the organization under which the group exists.
--
-- 'groupId', 'disassociateMemberFromGroup_groupId' - The identifier for the group from which members are removed.
--
-- 'memberId', 'disassociateMemberFromGroup_memberId' - The identifier for the member to be removed to the group.
newDisassociateMemberFromGroup ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  DisassociateMemberFromGroup
newDisassociateMemberFromGroup
  pOrganizationId_
  pGroupId_
  pMemberId_ =
    DisassociateMemberFromGroup'
      { organizationId =
          pOrganizationId_,
        groupId = pGroupId_,
        memberId = pMemberId_
      }

-- | The identifier for the organization under which the group exists.
disassociateMemberFromGroup_organizationId :: Lens.Lens' DisassociateMemberFromGroup Prelude.Text
disassociateMemberFromGroup_organizationId = Lens.lens (\DisassociateMemberFromGroup' {organizationId} -> organizationId) (\s@DisassociateMemberFromGroup' {} a -> s {organizationId = a} :: DisassociateMemberFromGroup)

-- | The identifier for the group from which members are removed.
disassociateMemberFromGroup_groupId :: Lens.Lens' DisassociateMemberFromGroup Prelude.Text
disassociateMemberFromGroup_groupId = Lens.lens (\DisassociateMemberFromGroup' {groupId} -> groupId) (\s@DisassociateMemberFromGroup' {} a -> s {groupId = a} :: DisassociateMemberFromGroup)

-- | The identifier for the member to be removed to the group.
disassociateMemberFromGroup_memberId :: Lens.Lens' DisassociateMemberFromGroup Prelude.Text
disassociateMemberFromGroup_memberId = Lens.lens (\DisassociateMemberFromGroup' {memberId} -> memberId) (\s@DisassociateMemberFromGroup' {} a -> s {memberId = a} :: DisassociateMemberFromGroup)

instance Core.AWSRequest DisassociateMemberFromGroup where
  type
    AWSResponse DisassociateMemberFromGroup =
      DisassociateMemberFromGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMemberFromGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateMemberFromGroup where
  hashWithSalt _salt DisassociateMemberFromGroup' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData DisassociateMemberFromGroup where
  rnf DisassociateMemberFromGroup' {..} =
    Prelude.rnf organizationId `Prelude.seq`
      Prelude.rnf groupId `Prelude.seq`
        Prelude.rnf memberId

instance Data.ToHeaders DisassociateMemberFromGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DisassociateMemberFromGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateMemberFromGroup where
  toJSON DisassociateMemberFromGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("MemberId" Data..= memberId)
          ]
      )

instance Data.ToPath DisassociateMemberFromGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateMemberFromGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMemberFromGroupResponse' smart constructor.
data DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberFromGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMemberFromGroupResponse_httpStatus' - The response's http status code.
newDisassociateMemberFromGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateMemberFromGroupResponse
newDisassociateMemberFromGroupResponse pHttpStatus_ =
  DisassociateMemberFromGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateMemberFromGroupResponse_httpStatus :: Lens.Lens' DisassociateMemberFromGroupResponse Prelude.Int
disassociateMemberFromGroupResponse_httpStatus = Lens.lens (\DisassociateMemberFromGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateMemberFromGroupResponse' {} a -> s {httpStatus = a} :: DisassociateMemberFromGroupResponse)

instance
  Prelude.NFData
    DisassociateMemberFromGroupResponse
  where
  rnf DisassociateMemberFromGroupResponse' {..} =
    Prelude.rnf httpStatus
