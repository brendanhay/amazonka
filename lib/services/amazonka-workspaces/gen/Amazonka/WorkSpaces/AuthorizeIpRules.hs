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
-- Module      : Amazonka.WorkSpaces.AuthorizeIpRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more rules to the specified IP access control group.
--
-- This action gives users permission to access their WorkSpaces from the
-- CIDR address ranges specified in the rules.
module Amazonka.WorkSpaces.AuthorizeIpRules
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newAuthorizeIpRules' smart constructor.
data AuthorizeIpRules = AuthorizeIpRules'
  { -- | The identifier of the group.
    groupId :: Prelude.Text,
    -- | The rules to add to the group.
    userRules :: [IpRuleItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
authorizeIpRules_userRules = Lens.lens (\AuthorizeIpRules' {userRules} -> userRules) (\s@AuthorizeIpRules' {} a -> s {userRules = a} :: AuthorizeIpRules) Prelude.. Lens.coerced

instance Core.AWSRequest AuthorizeIpRules where
  type
    AWSResponse AuthorizeIpRules =
      AuthorizeIpRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AuthorizeIpRulesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AuthorizeIpRules where
  hashWithSalt _salt AuthorizeIpRules' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` userRules

instance Prelude.NFData AuthorizeIpRules where
  rnf AuthorizeIpRules' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf userRules

instance Data.ToHeaders AuthorizeIpRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.AuthorizeIpRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AuthorizeIpRules where
  toJSON AuthorizeIpRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("UserRules" Data..= userRules)
          ]
      )

instance Data.ToPath AuthorizeIpRules where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeIpRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAuthorizeIpRulesResponse' smart constructor.
data AuthorizeIpRulesResponse = AuthorizeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AuthorizeIpRulesResponse where
  rnf AuthorizeIpRulesResponse' {..} =
    Prelude.rnf httpStatus
