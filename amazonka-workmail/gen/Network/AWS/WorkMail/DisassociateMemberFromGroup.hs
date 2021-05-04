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
-- Module      : Network.AWS.WorkMail.DisassociateMemberFromGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from a group.
module Network.AWS.WorkMail.DisassociateMemberFromGroup
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDisassociateMemberFromGroup' smart constructor.
data DisassociateMemberFromGroup = DisassociateMemberFromGroup'
  { -- | The identifier for the organization under which the group exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the group from which members are removed.
    groupId :: Prelude.Text,
    -- | The identifier for the member to be removed to the group.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DisassociateMemberFromGroup
  where
  type
    Rs DisassociateMemberFromGroup =
      DisassociateMemberFromGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMemberFromGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateMemberFromGroup

instance Prelude.NFData DisassociateMemberFromGroup

instance
  Prelude.ToHeaders
    DisassociateMemberFromGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DisassociateMemberFromGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateMemberFromGroup where
  toJSON DisassociateMemberFromGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("GroupId" Prelude..= groupId),
            Prelude.Just ("MemberId" Prelude..= memberId)
          ]
      )

instance Prelude.ToPath DisassociateMemberFromGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateMemberFromGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMemberFromGroupResponse' smart constructor.
data DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
