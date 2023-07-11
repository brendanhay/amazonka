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
-- Module      : Amazonka.Route53Resolver.UpdateFirewallRuleGroupAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the association of a FirewallRuleGroup with a VPC. The
-- association enables DNS filtering for the VPC.
module Amazonka.Route53Resolver.UpdateFirewallRuleGroupAssociation
  ( -- * Creating a Request
    UpdateFirewallRuleGroupAssociation (..),
    newUpdateFirewallRuleGroupAssociation,

    -- * Request Lenses
    updateFirewallRuleGroupAssociation_mutationProtection,
    updateFirewallRuleGroupAssociation_name,
    updateFirewallRuleGroupAssociation_priority,
    updateFirewallRuleGroupAssociation_firewallRuleGroupAssociationId,

    -- * Destructuring the Response
    UpdateFirewallRuleGroupAssociationResponse (..),
    newUpdateFirewallRuleGroupAssociationResponse,

    -- * Response Lenses
    updateFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation,
    updateFirewallRuleGroupAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newUpdateFirewallRuleGroupAssociation' smart constructor.
data UpdateFirewallRuleGroupAssociation = UpdateFirewallRuleGroupAssociation'
  { -- | If enabled, this setting disallows modification or removal of the
    -- association, to help prevent against accidentally altering DNS firewall
    -- protections.
    mutationProtection :: Prelude.Maybe MutationProtectionStatus,
    -- | The name of the rule group association.
    name :: Prelude.Maybe Prelude.Text,
    -- | The setting that determines the processing order of the rule group among
    -- the rule groups that you associate with the specified VPC. DNS Firewall
    -- filters VPC traffic starting from the rule group with the lowest numeric
    -- priority setting.
    --
    -- You must specify a unique priority for each rule group that you
    -- associate with a single VPC. To make it easier to insert rule groups
    -- later, leave space between the numbers, for example, use 100, 200, and
    -- so on. You can change the priority setting for a rule group association
    -- after you create it.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the FirewallRuleGroupAssociation.
    firewallRuleGroupAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallRuleGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mutationProtection', 'updateFirewallRuleGroupAssociation_mutationProtection' - If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections.
--
-- 'name', 'updateFirewallRuleGroupAssociation_name' - The name of the rule group association.
--
-- 'priority', 'updateFirewallRuleGroupAssociation_priority' - The setting that determines the processing order of the rule group among
-- the rule groups that you associate with the specified VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
--
-- You must specify a unique priority for each rule group that you
-- associate with a single VPC. To make it easier to insert rule groups
-- later, leave space between the numbers, for example, use 100, 200, and
-- so on. You can change the priority setting for a rule group association
-- after you create it.
--
-- 'firewallRuleGroupAssociationId', 'updateFirewallRuleGroupAssociation_firewallRuleGroupAssociationId' - The identifier of the FirewallRuleGroupAssociation.
newUpdateFirewallRuleGroupAssociation ::
  -- | 'firewallRuleGroupAssociationId'
  Prelude.Text ->
  UpdateFirewallRuleGroupAssociation
newUpdateFirewallRuleGroupAssociation
  pFirewallRuleGroupAssociationId_ =
    UpdateFirewallRuleGroupAssociation'
      { mutationProtection =
          Prelude.Nothing,
        name = Prelude.Nothing,
        priority = Prelude.Nothing,
        firewallRuleGroupAssociationId =
          pFirewallRuleGroupAssociationId_
      }

-- | If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections.
updateFirewallRuleGroupAssociation_mutationProtection :: Lens.Lens' UpdateFirewallRuleGroupAssociation (Prelude.Maybe MutationProtectionStatus)
updateFirewallRuleGroupAssociation_mutationProtection = Lens.lens (\UpdateFirewallRuleGroupAssociation' {mutationProtection} -> mutationProtection) (\s@UpdateFirewallRuleGroupAssociation' {} a -> s {mutationProtection = a} :: UpdateFirewallRuleGroupAssociation)

-- | The name of the rule group association.
updateFirewallRuleGroupAssociation_name :: Lens.Lens' UpdateFirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
updateFirewallRuleGroupAssociation_name = Lens.lens (\UpdateFirewallRuleGroupAssociation' {name} -> name) (\s@UpdateFirewallRuleGroupAssociation' {} a -> s {name = a} :: UpdateFirewallRuleGroupAssociation)

-- | The setting that determines the processing order of the rule group among
-- the rule groups that you associate with the specified VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
--
-- You must specify a unique priority for each rule group that you
-- associate with a single VPC. To make it easier to insert rule groups
-- later, leave space between the numbers, for example, use 100, 200, and
-- so on. You can change the priority setting for a rule group association
-- after you create it.
updateFirewallRuleGroupAssociation_priority :: Lens.Lens' UpdateFirewallRuleGroupAssociation (Prelude.Maybe Prelude.Int)
updateFirewallRuleGroupAssociation_priority = Lens.lens (\UpdateFirewallRuleGroupAssociation' {priority} -> priority) (\s@UpdateFirewallRuleGroupAssociation' {} a -> s {priority = a} :: UpdateFirewallRuleGroupAssociation)

-- | The identifier of the FirewallRuleGroupAssociation.
updateFirewallRuleGroupAssociation_firewallRuleGroupAssociationId :: Lens.Lens' UpdateFirewallRuleGroupAssociation Prelude.Text
updateFirewallRuleGroupAssociation_firewallRuleGroupAssociationId = Lens.lens (\UpdateFirewallRuleGroupAssociation' {firewallRuleGroupAssociationId} -> firewallRuleGroupAssociationId) (\s@UpdateFirewallRuleGroupAssociation' {} a -> s {firewallRuleGroupAssociationId = a} :: UpdateFirewallRuleGroupAssociation)

instance
  Core.AWSRequest
    UpdateFirewallRuleGroupAssociation
  where
  type
    AWSResponse UpdateFirewallRuleGroupAssociation =
      UpdateFirewallRuleGroupAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallRuleGroupAssociationResponse'
            Prelude.<$> (x Data..?> "FirewallRuleGroupAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFirewallRuleGroupAssociation
  where
  hashWithSalt
    _salt
    UpdateFirewallRuleGroupAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` mutationProtection
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` firewallRuleGroupAssociationId

instance
  Prelude.NFData
    UpdateFirewallRuleGroupAssociation
  where
  rnf UpdateFirewallRuleGroupAssociation' {..} =
    Prelude.rnf mutationProtection
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf firewallRuleGroupAssociationId

instance
  Data.ToHeaders
    UpdateFirewallRuleGroupAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.UpdateFirewallRuleGroupAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateFirewallRuleGroupAssociation
  where
  toJSON UpdateFirewallRuleGroupAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MutationProtection" Data..=)
              Prelude.<$> mutationProtection,
            ("Name" Data..=) Prelude.<$> name,
            ("Priority" Data..=) Prelude.<$> priority,
            Prelude.Just
              ( "FirewallRuleGroupAssociationId"
                  Data..= firewallRuleGroupAssociationId
              )
          ]
      )

instance
  Data.ToPath
    UpdateFirewallRuleGroupAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateFirewallRuleGroupAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallRuleGroupAssociationResponse' smart constructor.
data UpdateFirewallRuleGroupAssociationResponse = UpdateFirewallRuleGroupAssociationResponse'
  { -- | The association that you just updated.
    firewallRuleGroupAssociation :: Prelude.Maybe FirewallRuleGroupAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallRuleGroupAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupAssociation', 'updateFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation' - The association that you just updated.
--
-- 'httpStatus', 'updateFirewallRuleGroupAssociationResponse_httpStatus' - The response's http status code.
newUpdateFirewallRuleGroupAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallRuleGroupAssociationResponse
newUpdateFirewallRuleGroupAssociationResponse
  pHttpStatus_ =
    UpdateFirewallRuleGroupAssociationResponse'
      { firewallRuleGroupAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The association that you just updated.
updateFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation :: Lens.Lens' UpdateFirewallRuleGroupAssociationResponse (Prelude.Maybe FirewallRuleGroupAssociation)
updateFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation = Lens.lens (\UpdateFirewallRuleGroupAssociationResponse' {firewallRuleGroupAssociation} -> firewallRuleGroupAssociation) (\s@UpdateFirewallRuleGroupAssociationResponse' {} a -> s {firewallRuleGroupAssociation = a} :: UpdateFirewallRuleGroupAssociationResponse)

-- | The response's http status code.
updateFirewallRuleGroupAssociationResponse_httpStatus :: Lens.Lens' UpdateFirewallRuleGroupAssociationResponse Prelude.Int
updateFirewallRuleGroupAssociationResponse_httpStatus = Lens.lens (\UpdateFirewallRuleGroupAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallRuleGroupAssociationResponse' {} a -> s {httpStatus = a} :: UpdateFirewallRuleGroupAssociationResponse)

instance
  Prelude.NFData
    UpdateFirewallRuleGroupAssociationResponse
  where
  rnf UpdateFirewallRuleGroupAssociationResponse' {..} =
    Prelude.rnf firewallRuleGroupAssociation
      `Prelude.seq` Prelude.rnf httpStatus
