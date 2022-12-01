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
-- Module      : Amazonka.EC2.ModifySecurityGroupRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the rules of a security group.
module Amazonka.EC2.ModifySecurityGroupRules
  ( -- * Creating a Request
    ModifySecurityGroupRules (..),
    newModifySecurityGroupRules,

    -- * Request Lenses
    modifySecurityGroupRules_dryRun,
    modifySecurityGroupRules_groupId,
    modifySecurityGroupRules_securityGroupRules,

    -- * Destructuring the Response
    ModifySecurityGroupRulesResponse (..),
    newModifySecurityGroupRulesResponse,

    -- * Response Lenses
    modifySecurityGroupRulesResponse_return,
    modifySecurityGroupRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySecurityGroupRules' smart constructor.
data ModifySecurityGroupRules = ModifySecurityGroupRules'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the security group.
    groupId :: Prelude.Text,
    -- | Information about the security group properties to update.
    securityGroupRules :: [SecurityGroupRuleUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySecurityGroupRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifySecurityGroupRules_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupId', 'modifySecurityGroupRules_groupId' - The ID of the security group.
--
-- 'securityGroupRules', 'modifySecurityGroupRules_securityGroupRules' - Information about the security group properties to update.
newModifySecurityGroupRules ::
  -- | 'groupId'
  Prelude.Text ->
  ModifySecurityGroupRules
newModifySecurityGroupRules pGroupId_ =
  ModifySecurityGroupRules'
    { dryRun = Prelude.Nothing,
      groupId = pGroupId_,
      securityGroupRules = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifySecurityGroupRules_dryRun :: Lens.Lens' ModifySecurityGroupRules (Prelude.Maybe Prelude.Bool)
modifySecurityGroupRules_dryRun = Lens.lens (\ModifySecurityGroupRules' {dryRun} -> dryRun) (\s@ModifySecurityGroupRules' {} a -> s {dryRun = a} :: ModifySecurityGroupRules)

-- | The ID of the security group.
modifySecurityGroupRules_groupId :: Lens.Lens' ModifySecurityGroupRules Prelude.Text
modifySecurityGroupRules_groupId = Lens.lens (\ModifySecurityGroupRules' {groupId} -> groupId) (\s@ModifySecurityGroupRules' {} a -> s {groupId = a} :: ModifySecurityGroupRules)

-- | Information about the security group properties to update.
modifySecurityGroupRules_securityGroupRules :: Lens.Lens' ModifySecurityGroupRules [SecurityGroupRuleUpdate]
modifySecurityGroupRules_securityGroupRules = Lens.lens (\ModifySecurityGroupRules' {securityGroupRules} -> securityGroupRules) (\s@ModifySecurityGroupRules' {} a -> s {securityGroupRules = a} :: ModifySecurityGroupRules) Prelude.. Lens.coerced

instance Core.AWSRequest ModifySecurityGroupRules where
  type
    AWSResponse ModifySecurityGroupRules =
      ModifySecurityGroupRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifySecurityGroupRulesResponse'
            Prelude.<$> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifySecurityGroupRules where
  hashWithSalt _salt ModifySecurityGroupRules' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` securityGroupRules

instance Prelude.NFData ModifySecurityGroupRules where
  rnf ModifySecurityGroupRules' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf securityGroupRules

instance Core.ToHeaders ModifySecurityGroupRules where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifySecurityGroupRules where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifySecurityGroupRules where
  toQuery ModifySecurityGroupRules' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifySecurityGroupRules" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "GroupId" Core.=: groupId,
        Core.toQueryList
          "SecurityGroupRule"
          securityGroupRules
      ]

-- | /See:/ 'newModifySecurityGroupRulesResponse' smart constructor.
data ModifySecurityGroupRulesResponse = ModifySecurityGroupRulesResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySecurityGroupRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifySecurityGroupRulesResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'modifySecurityGroupRulesResponse_httpStatus' - The response's http status code.
newModifySecurityGroupRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifySecurityGroupRulesResponse
newModifySecurityGroupRulesResponse pHttpStatus_ =
  ModifySecurityGroupRulesResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
modifySecurityGroupRulesResponse_return :: Lens.Lens' ModifySecurityGroupRulesResponse (Prelude.Maybe Prelude.Bool)
modifySecurityGroupRulesResponse_return = Lens.lens (\ModifySecurityGroupRulesResponse' {return'} -> return') (\s@ModifySecurityGroupRulesResponse' {} a -> s {return' = a} :: ModifySecurityGroupRulesResponse)

-- | The response's http status code.
modifySecurityGroupRulesResponse_httpStatus :: Lens.Lens' ModifySecurityGroupRulesResponse Prelude.Int
modifySecurityGroupRulesResponse_httpStatus = Lens.lens (\ModifySecurityGroupRulesResponse' {httpStatus} -> httpStatus) (\s@ModifySecurityGroupRulesResponse' {} a -> s {httpStatus = a} :: ModifySecurityGroupRulesResponse)

instance
  Prelude.NFData
    ModifySecurityGroupRulesResponse
  where
  rnf ModifySecurityGroupRulesResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
