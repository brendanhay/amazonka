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
-- Module      : Amazonka.Greengrass.AssociateRoleToGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with a group. Your Greengrass core will use the role
-- to access AWS cloud services. The role\'s permissions should allow
-- Greengrass core Lambda functions to perform actions against the cloud.
module Amazonka.Greengrass.AssociateRoleToGroup
  ( -- * Creating a Request
    AssociateRoleToGroup (..),
    newAssociateRoleToGroup,

    -- * Request Lenses
    associateRoleToGroup_groupId,
    associateRoleToGroup_roleArn,

    -- * Destructuring the Response
    AssociateRoleToGroupResponse (..),
    newAssociateRoleToGroupResponse,

    -- * Response Lenses
    associateRoleToGroupResponse_associatedAt,
    associateRoleToGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateRoleToGroup' smart constructor.
data AssociateRoleToGroup = AssociateRoleToGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text,
    -- | The ARN of the role you wish to associate with this group. The existence
    -- of the role is not validated.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRoleToGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'associateRoleToGroup_groupId' - The ID of the Greengrass group.
--
-- 'roleArn', 'associateRoleToGroup_roleArn' - The ARN of the role you wish to associate with this group. The existence
-- of the role is not validated.
newAssociateRoleToGroup ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  AssociateRoleToGroup
newAssociateRoleToGroup pGroupId_ pRoleArn_ =
  AssociateRoleToGroup'
    { groupId = pGroupId_,
      roleArn = pRoleArn_
    }

-- | The ID of the Greengrass group.
associateRoleToGroup_groupId :: Lens.Lens' AssociateRoleToGroup Prelude.Text
associateRoleToGroup_groupId = Lens.lens (\AssociateRoleToGroup' {groupId} -> groupId) (\s@AssociateRoleToGroup' {} a -> s {groupId = a} :: AssociateRoleToGroup)

-- | The ARN of the role you wish to associate with this group. The existence
-- of the role is not validated.
associateRoleToGroup_roleArn :: Lens.Lens' AssociateRoleToGroup Prelude.Text
associateRoleToGroup_roleArn = Lens.lens (\AssociateRoleToGroup' {roleArn} -> roleArn) (\s@AssociateRoleToGroup' {} a -> s {roleArn = a} :: AssociateRoleToGroup)

instance Core.AWSRequest AssociateRoleToGroup where
  type
    AWSResponse AssociateRoleToGroup =
      AssociateRoleToGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateRoleToGroupResponse'
            Prelude.<$> (x Data..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateRoleToGroup where
  hashWithSalt _salt AssociateRoleToGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AssociateRoleToGroup where
  rnf AssociateRoleToGroup' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders AssociateRoleToGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateRoleToGroup where
  toJSON AssociateRoleToGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Data..= roleArn)]
      )

instance Data.ToPath AssociateRoleToGroup where
  toPath AssociateRoleToGroup' {..} =
    Prelude.mconcat
      ["/greengrass/groups/", Data.toBS groupId, "/role"]

instance Data.ToQuery AssociateRoleToGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateRoleToGroupResponse' smart constructor.
data AssociateRoleToGroupResponse = AssociateRoleToGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role ARN was
    -- associated with the group.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRoleToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAt', 'associateRoleToGroupResponse_associatedAt' - The time, in milliseconds since the epoch, when the role ARN was
-- associated with the group.
--
-- 'httpStatus', 'associateRoleToGroupResponse_httpStatus' - The response's http status code.
newAssociateRoleToGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateRoleToGroupResponse
newAssociateRoleToGroupResponse pHttpStatus_ =
  AssociateRoleToGroupResponse'
    { associatedAt =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the role ARN was
-- associated with the group.
associateRoleToGroupResponse_associatedAt :: Lens.Lens' AssociateRoleToGroupResponse (Prelude.Maybe Prelude.Text)
associateRoleToGroupResponse_associatedAt = Lens.lens (\AssociateRoleToGroupResponse' {associatedAt} -> associatedAt) (\s@AssociateRoleToGroupResponse' {} a -> s {associatedAt = a} :: AssociateRoleToGroupResponse)

-- | The response's http status code.
associateRoleToGroupResponse_httpStatus :: Lens.Lens' AssociateRoleToGroupResponse Prelude.Int
associateRoleToGroupResponse_httpStatus = Lens.lens (\AssociateRoleToGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateRoleToGroupResponse' {} a -> s {httpStatus = a} :: AssociateRoleToGroupResponse)

instance Prelude.NFData AssociateRoleToGroupResponse where
  rnf AssociateRoleToGroupResponse' {..} =
    Prelude.rnf associatedAt
      `Prelude.seq` Prelude.rnf httpStatus
