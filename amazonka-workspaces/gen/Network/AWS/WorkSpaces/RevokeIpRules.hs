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
-- Module      : Network.AWS.WorkSpaces.RevokeIpRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more rules from the specified IP access control group.
module Network.AWS.WorkSpaces.RevokeIpRules
  ( -- * Creating a Request
    RevokeIpRules (..),
    newRevokeIpRules,

    -- * Request Lenses
    revokeIpRules_groupId,
    revokeIpRules_userRules,

    -- * Destructuring the Response
    RevokeIpRulesResponse (..),
    newRevokeIpRulesResponse,

    -- * Response Lenses
    revokeIpRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRevokeIpRules' smart constructor.
data RevokeIpRules = RevokeIpRules'
  { -- | The identifier of the group.
    groupId :: Core.Text,
    -- | The rules to remove from the group.
    userRules :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeIpRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'revokeIpRules_groupId' - The identifier of the group.
--
-- 'userRules', 'revokeIpRules_userRules' - The rules to remove from the group.
newRevokeIpRules ::
  -- | 'groupId'
  Core.Text ->
  RevokeIpRules
newRevokeIpRules pGroupId_ =
  RevokeIpRules'
    { groupId = pGroupId_,
      userRules = Core.mempty
    }

-- | The identifier of the group.
revokeIpRules_groupId :: Lens.Lens' RevokeIpRules Core.Text
revokeIpRules_groupId = Lens.lens (\RevokeIpRules' {groupId} -> groupId) (\s@RevokeIpRules' {} a -> s {groupId = a} :: RevokeIpRules)

-- | The rules to remove from the group.
revokeIpRules_userRules :: Lens.Lens' RevokeIpRules [Core.Text]
revokeIpRules_userRules = Lens.lens (\RevokeIpRules' {userRules} -> userRules) (\s@RevokeIpRules' {} a -> s {userRules = a} :: RevokeIpRules) Core.. Lens._Coerce

instance Core.AWSRequest RevokeIpRules where
  type
    AWSResponse RevokeIpRules =
      RevokeIpRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeIpRulesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RevokeIpRules

instance Core.NFData RevokeIpRules

instance Core.ToHeaders RevokeIpRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.RevokeIpRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RevokeIpRules where
  toJSON RevokeIpRules' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupId" Core..= groupId),
            Core.Just ("UserRules" Core..= userRules)
          ]
      )

instance Core.ToPath RevokeIpRules where
  toPath = Core.const "/"

instance Core.ToQuery RevokeIpRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRevokeIpRulesResponse' smart constructor.
data RevokeIpRulesResponse = RevokeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeIpRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'revokeIpRulesResponse_httpStatus' - The response's http status code.
newRevokeIpRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RevokeIpRulesResponse
newRevokeIpRulesResponse pHttpStatus_ =
  RevokeIpRulesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
revokeIpRulesResponse_httpStatus :: Lens.Lens' RevokeIpRulesResponse Core.Int
revokeIpRulesResponse_httpStatus = Lens.lens (\RevokeIpRulesResponse' {httpStatus} -> httpStatus) (\s@RevokeIpRulesResponse' {} a -> s {httpStatus = a} :: RevokeIpRulesResponse)

instance Core.NFData RevokeIpRulesResponse
