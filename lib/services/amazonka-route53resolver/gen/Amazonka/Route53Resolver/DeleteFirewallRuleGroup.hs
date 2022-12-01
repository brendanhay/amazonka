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
-- Module      : Amazonka.Route53Resolver.DeleteFirewallRuleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified firewall rule group.
module Amazonka.Route53Resolver.DeleteFirewallRuleGroup
  ( -- * Creating a Request
    DeleteFirewallRuleGroup (..),
    newDeleteFirewallRuleGroup,

    -- * Request Lenses
    deleteFirewallRuleGroup_firewallRuleGroupId,

    -- * Destructuring the Response
    DeleteFirewallRuleGroupResponse (..),
    newDeleteFirewallRuleGroupResponse,

    -- * Response Lenses
    deleteFirewallRuleGroupResponse_firewallRuleGroup,
    deleteFirewallRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDeleteFirewallRuleGroup' smart constructor.
data DeleteFirewallRuleGroup = DeleteFirewallRuleGroup'
  { -- | The unique identifier of the firewall rule group that you want to
    -- delete.
    firewallRuleGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupId', 'deleteFirewallRuleGroup_firewallRuleGroupId' - The unique identifier of the firewall rule group that you want to
-- delete.
newDeleteFirewallRuleGroup ::
  -- | 'firewallRuleGroupId'
  Prelude.Text ->
  DeleteFirewallRuleGroup
newDeleteFirewallRuleGroup pFirewallRuleGroupId_ =
  DeleteFirewallRuleGroup'
    { firewallRuleGroupId =
        pFirewallRuleGroupId_
    }

-- | The unique identifier of the firewall rule group that you want to
-- delete.
deleteFirewallRuleGroup_firewallRuleGroupId :: Lens.Lens' DeleteFirewallRuleGroup Prelude.Text
deleteFirewallRuleGroup_firewallRuleGroupId = Lens.lens (\DeleteFirewallRuleGroup' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@DeleteFirewallRuleGroup' {} a -> s {firewallRuleGroupId = a} :: DeleteFirewallRuleGroup)

instance Core.AWSRequest DeleteFirewallRuleGroup where
  type
    AWSResponse DeleteFirewallRuleGroup =
      DeleteFirewallRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFirewallRuleGroupResponse'
            Prelude.<$> (x Core..?> "FirewallRuleGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFirewallRuleGroup where
  hashWithSalt _salt DeleteFirewallRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` firewallRuleGroupId

instance Prelude.NFData DeleteFirewallRuleGroup where
  rnf DeleteFirewallRuleGroup' {..} =
    Prelude.rnf firewallRuleGroupId

instance Core.ToHeaders DeleteFirewallRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.DeleteFirewallRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFirewallRuleGroup where
  toJSON DeleteFirewallRuleGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FirewallRuleGroupId" Core..= firewallRuleGroupId)
          ]
      )

instance Core.ToPath DeleteFirewallRuleGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFirewallRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFirewallRuleGroupResponse' smart constructor.
data DeleteFirewallRuleGroupResponse = DeleteFirewallRuleGroupResponse'
  { -- | A collection of rules used to filter DNS network traffic.
    firewallRuleGroup :: Prelude.Maybe FirewallRuleGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroup', 'deleteFirewallRuleGroupResponse_firewallRuleGroup' - A collection of rules used to filter DNS network traffic.
--
-- 'httpStatus', 'deleteFirewallRuleGroupResponse_httpStatus' - The response's http status code.
newDeleteFirewallRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFirewallRuleGroupResponse
newDeleteFirewallRuleGroupResponse pHttpStatus_ =
  DeleteFirewallRuleGroupResponse'
    { firewallRuleGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of rules used to filter DNS network traffic.
deleteFirewallRuleGroupResponse_firewallRuleGroup :: Lens.Lens' DeleteFirewallRuleGroupResponse (Prelude.Maybe FirewallRuleGroup)
deleteFirewallRuleGroupResponse_firewallRuleGroup = Lens.lens (\DeleteFirewallRuleGroupResponse' {firewallRuleGroup} -> firewallRuleGroup) (\s@DeleteFirewallRuleGroupResponse' {} a -> s {firewallRuleGroup = a} :: DeleteFirewallRuleGroupResponse)

-- | The response's http status code.
deleteFirewallRuleGroupResponse_httpStatus :: Lens.Lens' DeleteFirewallRuleGroupResponse Prelude.Int
deleteFirewallRuleGroupResponse_httpStatus = Lens.lens (\DeleteFirewallRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteFirewallRuleGroupResponse' {} a -> s {httpStatus = a} :: DeleteFirewallRuleGroupResponse)

instance
  Prelude.NFData
    DeleteFirewallRuleGroupResponse
  where
  rnf DeleteFirewallRuleGroupResponse' {..} =
    Prelude.rnf firewallRuleGroup
      `Prelude.seq` Prelude.rnf httpStatus
