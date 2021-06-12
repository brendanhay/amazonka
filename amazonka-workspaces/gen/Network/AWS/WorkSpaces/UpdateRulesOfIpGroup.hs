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
-- Module      : Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current rules of the specified IP access control group with
-- the specified rules.
module Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
  ( -- * Creating a Request
    UpdateRulesOfIpGroup (..),
    newUpdateRulesOfIpGroup,

    -- * Request Lenses
    updateRulesOfIpGroup_groupId,
    updateRulesOfIpGroup_userRules,

    -- * Destructuring the Response
    UpdateRulesOfIpGroupResponse (..),
    newUpdateRulesOfIpGroupResponse,

    -- * Response Lenses
    updateRulesOfIpGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newUpdateRulesOfIpGroup' smart constructor.
data UpdateRulesOfIpGroup = UpdateRulesOfIpGroup'
  { -- | The identifier of the group.
    groupId :: Core.Text,
    -- | One or more rules.
    userRules :: [IpRuleItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRulesOfIpGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'updateRulesOfIpGroup_groupId' - The identifier of the group.
--
-- 'userRules', 'updateRulesOfIpGroup_userRules' - One or more rules.
newUpdateRulesOfIpGroup ::
  -- | 'groupId'
  Core.Text ->
  UpdateRulesOfIpGroup
newUpdateRulesOfIpGroup pGroupId_ =
  UpdateRulesOfIpGroup'
    { groupId = pGroupId_,
      userRules = Core.mempty
    }

-- | The identifier of the group.
updateRulesOfIpGroup_groupId :: Lens.Lens' UpdateRulesOfIpGroup Core.Text
updateRulesOfIpGroup_groupId = Lens.lens (\UpdateRulesOfIpGroup' {groupId} -> groupId) (\s@UpdateRulesOfIpGroup' {} a -> s {groupId = a} :: UpdateRulesOfIpGroup)

-- | One or more rules.
updateRulesOfIpGroup_userRules :: Lens.Lens' UpdateRulesOfIpGroup [IpRuleItem]
updateRulesOfIpGroup_userRules = Lens.lens (\UpdateRulesOfIpGroup' {userRules} -> userRules) (\s@UpdateRulesOfIpGroup' {} a -> s {userRules = a} :: UpdateRulesOfIpGroup) Core.. Lens._Coerce

instance Core.AWSRequest UpdateRulesOfIpGroup where
  type
    AWSResponse UpdateRulesOfIpGroup =
      UpdateRulesOfIpGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRulesOfIpGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRulesOfIpGroup

instance Core.NFData UpdateRulesOfIpGroup

instance Core.ToHeaders UpdateRulesOfIpGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.UpdateRulesOfIpGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRulesOfIpGroup where
  toJSON UpdateRulesOfIpGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupId" Core..= groupId),
            Core.Just ("UserRules" Core..= userRules)
          ]
      )

instance Core.ToPath UpdateRulesOfIpGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRulesOfIpGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRulesOfIpGroupResponse' smart constructor.
data UpdateRulesOfIpGroupResponse = UpdateRulesOfIpGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRulesOfIpGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRulesOfIpGroupResponse_httpStatus' - The response's http status code.
newUpdateRulesOfIpGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRulesOfIpGroupResponse
newUpdateRulesOfIpGroupResponse pHttpStatus_ =
  UpdateRulesOfIpGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRulesOfIpGroupResponse_httpStatus :: Lens.Lens' UpdateRulesOfIpGroupResponse Core.Int
updateRulesOfIpGroupResponse_httpStatus = Lens.lens (\UpdateRulesOfIpGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRulesOfIpGroupResponse' {} a -> s {httpStatus = a} :: UpdateRulesOfIpGroupResponse)

instance Core.NFData UpdateRulesOfIpGroupResponse
