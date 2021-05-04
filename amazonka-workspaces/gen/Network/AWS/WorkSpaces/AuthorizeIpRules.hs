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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newAuthorizeIpRules' smart constructor.
data AuthorizeIpRules = AuthorizeIpRules'
  { -- | The identifier of the group.
    groupId :: Prelude.Text,
    -- | The rules to add to the group.
    userRules :: [IpRuleItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AuthorizeIpRules
newAuthorizeIpRules pGroupId_ =
  AuthorizeIpRules'
    { groupId = pGroupId_,
      userRules = Prelude.mempty
    }

-- | The identifier of the group.
authorizeIpRules_groupId :: Lens.Lens' AuthorizeIpRules Prelude.Text
authorizeIpRules_groupId = Lens.lens (\AuthorizeIpRules' {groupId} -> groupId) (\s@AuthorizeIpRules' {} a -> s {groupId = a} :: AuthorizeIpRules)

-- | The rules to add to the group.
authorizeIpRules_userRules :: Lens.Lens' AuthorizeIpRules [IpRuleItem]
authorizeIpRules_userRules = Lens.lens (\AuthorizeIpRules' {userRules} -> userRules) (\s@AuthorizeIpRules' {} a -> s {userRules = a} :: AuthorizeIpRules) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AuthorizeIpRules where
  type Rs AuthorizeIpRules = AuthorizeIpRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AuthorizeIpRulesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AuthorizeIpRules

instance Prelude.NFData AuthorizeIpRules

instance Prelude.ToHeaders AuthorizeIpRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.AuthorizeIpRules" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AuthorizeIpRules where
  toJSON AuthorizeIpRules' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Prelude..= groupId),
            Prelude.Just ("UserRules" Prelude..= userRules)
          ]
      )

instance Prelude.ToPath AuthorizeIpRules where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AuthorizeIpRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAuthorizeIpRulesResponse' smart constructor.
data AuthorizeIpRulesResponse = AuthorizeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AuthorizeIpRulesResponse
newAuthorizeIpRulesResponse pHttpStatus_ =
  AuthorizeIpRulesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
authorizeIpRulesResponse_httpStatus :: Lens.Lens' AuthorizeIpRulesResponse Prelude.Int
authorizeIpRulesResponse_httpStatus = Lens.lens (\AuthorizeIpRulesResponse' {httpStatus} -> httpStatus) (\s@AuthorizeIpRulesResponse' {} a -> s {httpStatus = a} :: AuthorizeIpRulesResponse)

instance Prelude.NFData AuthorizeIpRulesResponse
