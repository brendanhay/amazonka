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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRevokeIpRules' smart constructor.
data RevokeIpRules = RevokeIpRules'
  { -- | The identifier of the group.
    groupId :: Prelude.Text,
    -- | The rules to remove from the group.
    userRules :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
revokeIpRules_userRules = Lens.lens (\RevokeIpRules' {userRules} -> userRules) (\s@RevokeIpRules' {} a -> s {userRules = a} :: RevokeIpRules) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RevokeIpRules where
  type Rs RevokeIpRules = RevokeIpRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeIpRulesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeIpRules

instance Prelude.NFData RevokeIpRules

instance Prelude.ToHeaders RevokeIpRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.RevokeIpRules" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RevokeIpRules where
  toJSON RevokeIpRules' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Prelude..= groupId),
            Prelude.Just ("UserRules" Prelude..= userRules)
          ]
      )

instance Prelude.ToPath RevokeIpRules where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RevokeIpRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeIpRulesResponse' smart constructor.
data RevokeIpRulesResponse = RevokeIpRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData RevokeIpRulesResponse
