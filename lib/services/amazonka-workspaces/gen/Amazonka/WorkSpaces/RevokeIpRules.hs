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
-- Module      : Amazonka.WorkSpaces.RevokeIpRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more rules from the specified IP access control group.
module Amazonka.WorkSpaces.RevokeIpRules
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newRevokeIpRules' smart constructor.
data RevokeIpRules = RevokeIpRules'
  { -- | The identifier of the group.
    groupId :: Prelude.Text,
    -- | The rules to remove from the group.
    userRules :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RevokeIpRules
newRevokeIpRules pGroupId_ =
  RevokeIpRules'
    { groupId = pGroupId_,
      userRules = Prelude.mempty
    }

-- | The identifier of the group.
revokeIpRules_groupId :: Lens.Lens' RevokeIpRules Prelude.Text
revokeIpRules_groupId = Lens.lens (\RevokeIpRules' {groupId} -> groupId) (\s@RevokeIpRules' {} a -> s {groupId = a} :: RevokeIpRules)

-- | The rules to remove from the group.
revokeIpRules_userRules :: Lens.Lens' RevokeIpRules [Prelude.Text]
revokeIpRules_userRules = Lens.lens (\RevokeIpRules' {userRules} -> userRules) (\s@RevokeIpRules' {} a -> s {userRules = a} :: RevokeIpRules) Prelude.. Lens.coerced

instance Core.AWSRequest RevokeIpRules where
  type
    AWSResponse RevokeIpRules =
      RevokeIpRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeIpRulesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeIpRules where
  hashWithSalt _salt RevokeIpRules' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` userRules

instance Prelude.NFData RevokeIpRules where
  rnf RevokeIpRules' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf userRules

instance Data.ToHeaders RevokeIpRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.RevokeIpRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RevokeIpRules where
  toJSON RevokeIpRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("UserRules" Data..= userRules)
          ]
      )

instance Data.ToPath RevokeIpRules where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeIpRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeIpRulesResponse' smart constructor.
data RevokeIpRulesResponse = RevokeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RevokeIpRulesResponse
newRevokeIpRulesResponse pHttpStatus_ =
  RevokeIpRulesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
revokeIpRulesResponse_httpStatus :: Lens.Lens' RevokeIpRulesResponse Prelude.Int
revokeIpRulesResponse_httpStatus = Lens.lens (\RevokeIpRulesResponse' {httpStatus} -> httpStatus) (\s@RevokeIpRulesResponse' {} a -> s {httpStatus = a} :: RevokeIpRulesResponse)

instance Prelude.NFData RevokeIpRulesResponse where
  rnf RevokeIpRulesResponse' {..} =
    Prelude.rnf httpStatus
