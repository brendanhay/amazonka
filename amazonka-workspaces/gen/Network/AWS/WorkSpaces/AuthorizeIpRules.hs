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
-- Module      : Network.AWS.WorkSpaces.AuthorizeIpRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more rules to the specified IP access control group.
--
-- This action gives users permission to access their WorkSpaces from the
-- CIDR address ranges specified in the rules.
module Network.AWS.WorkSpaces.AuthorizeIpRules
  ( -- * Creating a Request
    AuthorizeIpRules (..),
    newAuthorizeIpRules,

    -- * Request Lenses
    authorizeIpRules_groupId,
    authorizeIpRules_userRules,

    -- * Destructuring the Response
    AuthorizeIpRulesResponse (..),
    newAuthorizeIpRulesResponse,

    -- * Response Lenses
    authorizeIpRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newAuthorizeIpRules' smart constructor.
data AuthorizeIpRules = AuthorizeIpRules'
  { -- | The identifier of the group.
    groupId :: Core.Text,
    -- | The rules to add to the group.
    userRules :: [IpRuleItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeIpRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'authorizeIpRules_groupId' - The identifier of the group.
--
-- 'userRules', 'authorizeIpRules_userRules' - The rules to add to the group.
newAuthorizeIpRules ::
  -- | 'groupId'
  Core.Text ->
  AuthorizeIpRules
newAuthorizeIpRules pGroupId_ =
  AuthorizeIpRules'
    { groupId = pGroupId_,
      userRules = Core.mempty
    }

-- | The identifier of the group.
authorizeIpRules_groupId :: Lens.Lens' AuthorizeIpRules Core.Text
authorizeIpRules_groupId = Lens.lens (\AuthorizeIpRules' {groupId} -> groupId) (\s@AuthorizeIpRules' {} a -> s {groupId = a} :: AuthorizeIpRules)

-- | The rules to add to the group.
authorizeIpRules_userRules :: Lens.Lens' AuthorizeIpRules [IpRuleItem]
authorizeIpRules_userRules = Lens.lens (\AuthorizeIpRules' {userRules} -> userRules) (\s@AuthorizeIpRules' {} a -> s {userRules = a} :: AuthorizeIpRules) Core.. Lens._Coerce

instance Core.AWSRequest AuthorizeIpRules where
  type
    AWSResponse AuthorizeIpRules =
      AuthorizeIpRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AuthorizeIpRulesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AuthorizeIpRules

instance Core.NFData AuthorizeIpRules

instance Core.ToHeaders AuthorizeIpRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.AuthorizeIpRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AuthorizeIpRules where
  toJSON AuthorizeIpRules' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupId" Core..= groupId),
            Core.Just ("UserRules" Core..= userRules)
          ]
      )

instance Core.ToPath AuthorizeIpRules where
  toPath = Core.const "/"

instance Core.ToQuery AuthorizeIpRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAuthorizeIpRulesResponse' smart constructor.
data AuthorizeIpRulesResponse = AuthorizeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeIpRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'authorizeIpRulesResponse_httpStatus' - The response's http status code.
newAuthorizeIpRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AuthorizeIpRulesResponse
newAuthorizeIpRulesResponse pHttpStatus_ =
  AuthorizeIpRulesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
authorizeIpRulesResponse_httpStatus :: Lens.Lens' AuthorizeIpRulesResponse Core.Int
authorizeIpRulesResponse_httpStatus = Lens.lens (\AuthorizeIpRulesResponse' {httpStatus} -> httpStatus) (\s@AuthorizeIpRulesResponse' {} a -> s {httpStatus = a} :: AuthorizeIpRulesResponse)

instance Core.NFData AuthorizeIpRulesResponse
