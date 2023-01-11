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
-- Module      : Amazonka.Route53Resolver.DisassociateFirewallRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a FirewallRuleGroup from a VPC, to remove DNS filtering
-- from the VPC.
module Amazonka.Route53Resolver.DisassociateFirewallRuleGroup
  ( -- * Creating a Request
    DisassociateFirewallRuleGroup (..),
    newDisassociateFirewallRuleGroup,

    -- * Request Lenses
    disassociateFirewallRuleGroup_firewallRuleGroupAssociationId,

    -- * Destructuring the Response
    DisassociateFirewallRuleGroupResponse (..),
    newDisassociateFirewallRuleGroupResponse,

    -- * Response Lenses
    disassociateFirewallRuleGroupResponse_firewallRuleGroupAssociation,
    disassociateFirewallRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDisassociateFirewallRuleGroup' smart constructor.
data DisassociateFirewallRuleGroup = DisassociateFirewallRuleGroup'
  { -- | The identifier of the FirewallRuleGroupAssociation.
    firewallRuleGroupAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFirewallRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupAssociationId', 'disassociateFirewallRuleGroup_firewallRuleGroupAssociationId' - The identifier of the FirewallRuleGroupAssociation.
newDisassociateFirewallRuleGroup ::
  -- | 'firewallRuleGroupAssociationId'
  Prelude.Text ->
  DisassociateFirewallRuleGroup
newDisassociateFirewallRuleGroup
  pFirewallRuleGroupAssociationId_ =
    DisassociateFirewallRuleGroup'
      { firewallRuleGroupAssociationId =
          pFirewallRuleGroupAssociationId_
      }

-- | The identifier of the FirewallRuleGroupAssociation.
disassociateFirewallRuleGroup_firewallRuleGroupAssociationId :: Lens.Lens' DisassociateFirewallRuleGroup Prelude.Text
disassociateFirewallRuleGroup_firewallRuleGroupAssociationId = Lens.lens (\DisassociateFirewallRuleGroup' {firewallRuleGroupAssociationId} -> firewallRuleGroupAssociationId) (\s@DisassociateFirewallRuleGroup' {} a -> s {firewallRuleGroupAssociationId = a} :: DisassociateFirewallRuleGroup)

instance
  Core.AWSRequest
    DisassociateFirewallRuleGroup
  where
  type
    AWSResponse DisassociateFirewallRuleGroup =
      DisassociateFirewallRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateFirewallRuleGroupResponse'
            Prelude.<$> (x Data..?> "FirewallRuleGroupAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateFirewallRuleGroup
  where
  hashWithSalt _salt DisassociateFirewallRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` firewallRuleGroupAssociationId

instance Prelude.NFData DisassociateFirewallRuleGroup where
  rnf DisassociateFirewallRuleGroup' {..} =
    Prelude.rnf firewallRuleGroupAssociationId

instance Data.ToHeaders DisassociateFirewallRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DisassociateFirewallRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFirewallRuleGroup where
  toJSON DisassociateFirewallRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FirewallRuleGroupAssociationId"
                  Data..= firewallRuleGroupAssociationId
              )
          ]
      )

instance Data.ToPath DisassociateFirewallRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFirewallRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFirewallRuleGroupResponse' smart constructor.
data DisassociateFirewallRuleGroupResponse = DisassociateFirewallRuleGroupResponse'
  { -- | The firewall rule group association that you just removed.
    firewallRuleGroupAssociation :: Prelude.Maybe FirewallRuleGroupAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFirewallRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupAssociation', 'disassociateFirewallRuleGroupResponse_firewallRuleGroupAssociation' - The firewall rule group association that you just removed.
--
-- 'httpStatus', 'disassociateFirewallRuleGroupResponse_httpStatus' - The response's http status code.
newDisassociateFirewallRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFirewallRuleGroupResponse
newDisassociateFirewallRuleGroupResponse pHttpStatus_ =
  DisassociateFirewallRuleGroupResponse'
    { firewallRuleGroupAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The firewall rule group association that you just removed.
disassociateFirewallRuleGroupResponse_firewallRuleGroupAssociation :: Lens.Lens' DisassociateFirewallRuleGroupResponse (Prelude.Maybe FirewallRuleGroupAssociation)
disassociateFirewallRuleGroupResponse_firewallRuleGroupAssociation = Lens.lens (\DisassociateFirewallRuleGroupResponse' {firewallRuleGroupAssociation} -> firewallRuleGroupAssociation) (\s@DisassociateFirewallRuleGroupResponse' {} a -> s {firewallRuleGroupAssociation = a} :: DisassociateFirewallRuleGroupResponse)

-- | The response's http status code.
disassociateFirewallRuleGroupResponse_httpStatus :: Lens.Lens' DisassociateFirewallRuleGroupResponse Prelude.Int
disassociateFirewallRuleGroupResponse_httpStatus = Lens.lens (\DisassociateFirewallRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateFirewallRuleGroupResponse' {} a -> s {httpStatus = a} :: DisassociateFirewallRuleGroupResponse)

instance
  Prelude.NFData
    DisassociateFirewallRuleGroupResponse
  where
  rnf DisassociateFirewallRuleGroupResponse' {..} =
    Prelude.rnf firewallRuleGroupAssociation
      `Prelude.seq` Prelude.rnf httpStatus
