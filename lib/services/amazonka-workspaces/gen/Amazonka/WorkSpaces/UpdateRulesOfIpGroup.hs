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
-- Module      : Amazonka.WorkSpaces.UpdateRulesOfIpGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current rules of the specified IP access control group with
-- the specified rules.
module Amazonka.WorkSpaces.UpdateRulesOfIpGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newUpdateRulesOfIpGroup' smart constructor.
data UpdateRulesOfIpGroup = UpdateRulesOfIpGroup'
  { -- | The identifier of the group.
    groupId :: Prelude.Text,
    -- | One or more rules.
    userRules :: [IpRuleItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateRulesOfIpGroup
newUpdateRulesOfIpGroup pGroupId_ =
  UpdateRulesOfIpGroup'
    { groupId = pGroupId_,
      userRules = Prelude.mempty
    }

-- | The identifier of the group.
updateRulesOfIpGroup_groupId :: Lens.Lens' UpdateRulesOfIpGroup Prelude.Text
updateRulesOfIpGroup_groupId = Lens.lens (\UpdateRulesOfIpGroup' {groupId} -> groupId) (\s@UpdateRulesOfIpGroup' {} a -> s {groupId = a} :: UpdateRulesOfIpGroup)

-- | One or more rules.
updateRulesOfIpGroup_userRules :: Lens.Lens' UpdateRulesOfIpGroup [IpRuleItem]
updateRulesOfIpGroup_userRules = Lens.lens (\UpdateRulesOfIpGroup' {userRules} -> userRules) (\s@UpdateRulesOfIpGroup' {} a -> s {userRules = a} :: UpdateRulesOfIpGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRulesOfIpGroup where
  type
    AWSResponse UpdateRulesOfIpGroup =
      UpdateRulesOfIpGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRulesOfIpGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRulesOfIpGroup where
  hashWithSalt _salt UpdateRulesOfIpGroup' {..} =
    _salt `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` userRules

instance Prelude.NFData UpdateRulesOfIpGroup where
  rnf UpdateRulesOfIpGroup' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf userRules

instance Data.ToHeaders UpdateRulesOfIpGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.UpdateRulesOfIpGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRulesOfIpGroup where
  toJSON UpdateRulesOfIpGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("UserRules" Data..= userRules)
          ]
      )

instance Data.ToPath UpdateRulesOfIpGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRulesOfIpGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRulesOfIpGroupResponse' smart constructor.
data UpdateRulesOfIpGroupResponse = UpdateRulesOfIpGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateRulesOfIpGroupResponse
newUpdateRulesOfIpGroupResponse pHttpStatus_ =
  UpdateRulesOfIpGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRulesOfIpGroupResponse_httpStatus :: Lens.Lens' UpdateRulesOfIpGroupResponse Prelude.Int
updateRulesOfIpGroupResponse_httpStatus = Lens.lens (\UpdateRulesOfIpGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRulesOfIpGroupResponse' {} a -> s {httpStatus = a} :: UpdateRulesOfIpGroupResponse)

instance Prelude.NFData UpdateRulesOfIpGroupResponse where
  rnf UpdateRulesOfIpGroupResponse' {..} =
    Prelude.rnf httpStatus
