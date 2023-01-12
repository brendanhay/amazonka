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
-- Module      : Amazonka.Route53Resolver.AssociateFirewallRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a FirewallRuleGroup with a VPC, to provide DNS filtering for
-- the VPC.
module Amazonka.Route53Resolver.AssociateFirewallRuleGroup
  ( -- * Creating a Request
    AssociateFirewallRuleGroup (..),
    newAssociateFirewallRuleGroup,

    -- * Request Lenses
    associateFirewallRuleGroup_mutationProtection,
    associateFirewallRuleGroup_tags,
    associateFirewallRuleGroup_creatorRequestId,
    associateFirewallRuleGroup_firewallRuleGroupId,
    associateFirewallRuleGroup_vpcId,
    associateFirewallRuleGroup_priority,
    associateFirewallRuleGroup_name,

    -- * Destructuring the Response
    AssociateFirewallRuleGroupResponse (..),
    newAssociateFirewallRuleGroupResponse,

    -- * Response Lenses
    associateFirewallRuleGroupResponse_firewallRuleGroupAssociation,
    associateFirewallRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newAssociateFirewallRuleGroup' smart constructor.
data AssociateFirewallRuleGroup = AssociateFirewallRuleGroup'
  { -- | If enabled, this setting disallows modification or removal of the
    -- association, to help prevent against accidentally altering DNS firewall
    -- protections. When you create the association, the default setting is
    -- @DISABLED@.
    mutationProtection :: Prelude.Maybe MutationProtectionStatus,
    -- | A list of the tag keys and values that you want to associate with the
    -- rule group association.
    tags :: Prelude.Maybe [Tag],
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of running the operation twice.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Text,
    -- | The unique identifier of the firewall rule group.
    firewallRuleGroupId :: Prelude.Text,
    -- | The unique identifier of the VPC that you want to associate with the
    -- rule group.
    vpcId :: Prelude.Text,
    -- | The setting that determines the processing order of the rule group among
    -- the rule groups that you associate with the specified VPC. DNS Firewall
    -- filters VPC traffic starting from the rule group with the lowest numeric
    -- priority setting.
    --
    -- You must specify a unique priority for each rule group that you
    -- associate with a single VPC. To make it easier to insert rule groups
    -- later, leave space between the numbers, for example, use 101, 200, and
    -- so on. You can change the priority setting for a rule group association
    -- after you create it.
    --
    -- The allowed values for @Priority@ are between 100 and 9900.
    priority :: Prelude.Int,
    -- | A name that lets you identify the association, to manage and use it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFirewallRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mutationProtection', 'associateFirewallRuleGroup_mutationProtection' - If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections. When you create the association, the default setting is
-- @DISABLED@.
--
-- 'tags', 'associateFirewallRuleGroup_tags' - A list of the tag keys and values that you want to associate with the
-- rule group association.
--
-- 'creatorRequestId', 'associateFirewallRuleGroup_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
--
-- 'firewallRuleGroupId', 'associateFirewallRuleGroup_firewallRuleGroupId' - The unique identifier of the firewall rule group.
--
-- 'vpcId', 'associateFirewallRuleGroup_vpcId' - The unique identifier of the VPC that you want to associate with the
-- rule group.
--
-- 'priority', 'associateFirewallRuleGroup_priority' - The setting that determines the processing order of the rule group among
-- the rule groups that you associate with the specified VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
--
-- You must specify a unique priority for each rule group that you
-- associate with a single VPC. To make it easier to insert rule groups
-- later, leave space between the numbers, for example, use 101, 200, and
-- so on. You can change the priority setting for a rule group association
-- after you create it.
--
-- The allowed values for @Priority@ are between 100 and 9900.
--
-- 'name', 'associateFirewallRuleGroup_name' - A name that lets you identify the association, to manage and use it.
newAssociateFirewallRuleGroup ::
  -- | 'creatorRequestId'
  Prelude.Text ->
  -- | 'firewallRuleGroupId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  AssociateFirewallRuleGroup
newAssociateFirewallRuleGroup
  pCreatorRequestId_
  pFirewallRuleGroupId_
  pVpcId_
  pPriority_
  pName_ =
    AssociateFirewallRuleGroup'
      { mutationProtection =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        creatorRequestId = pCreatorRequestId_,
        firewallRuleGroupId = pFirewallRuleGroupId_,
        vpcId = pVpcId_,
        priority = pPriority_,
        name = pName_
      }

-- | If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections. When you create the association, the default setting is
-- @DISABLED@.
associateFirewallRuleGroup_mutationProtection :: Lens.Lens' AssociateFirewallRuleGroup (Prelude.Maybe MutationProtectionStatus)
associateFirewallRuleGroup_mutationProtection = Lens.lens (\AssociateFirewallRuleGroup' {mutationProtection} -> mutationProtection) (\s@AssociateFirewallRuleGroup' {} a -> s {mutationProtection = a} :: AssociateFirewallRuleGroup)

-- | A list of the tag keys and values that you want to associate with the
-- rule group association.
associateFirewallRuleGroup_tags :: Lens.Lens' AssociateFirewallRuleGroup (Prelude.Maybe [Tag])
associateFirewallRuleGroup_tags = Lens.lens (\AssociateFirewallRuleGroup' {tags} -> tags) (\s@AssociateFirewallRuleGroup' {} a -> s {tags = a} :: AssociateFirewallRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
associateFirewallRuleGroup_creatorRequestId :: Lens.Lens' AssociateFirewallRuleGroup Prelude.Text
associateFirewallRuleGroup_creatorRequestId = Lens.lens (\AssociateFirewallRuleGroup' {creatorRequestId} -> creatorRequestId) (\s@AssociateFirewallRuleGroup' {} a -> s {creatorRequestId = a} :: AssociateFirewallRuleGroup)

-- | The unique identifier of the firewall rule group.
associateFirewallRuleGroup_firewallRuleGroupId :: Lens.Lens' AssociateFirewallRuleGroup Prelude.Text
associateFirewallRuleGroup_firewallRuleGroupId = Lens.lens (\AssociateFirewallRuleGroup' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@AssociateFirewallRuleGroup' {} a -> s {firewallRuleGroupId = a} :: AssociateFirewallRuleGroup)

-- | The unique identifier of the VPC that you want to associate with the
-- rule group.
associateFirewallRuleGroup_vpcId :: Lens.Lens' AssociateFirewallRuleGroup Prelude.Text
associateFirewallRuleGroup_vpcId = Lens.lens (\AssociateFirewallRuleGroup' {vpcId} -> vpcId) (\s@AssociateFirewallRuleGroup' {} a -> s {vpcId = a} :: AssociateFirewallRuleGroup)

-- | The setting that determines the processing order of the rule group among
-- the rule groups that you associate with the specified VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
--
-- You must specify a unique priority for each rule group that you
-- associate with a single VPC. To make it easier to insert rule groups
-- later, leave space between the numbers, for example, use 101, 200, and
-- so on. You can change the priority setting for a rule group association
-- after you create it.
--
-- The allowed values for @Priority@ are between 100 and 9900.
associateFirewallRuleGroup_priority :: Lens.Lens' AssociateFirewallRuleGroup Prelude.Int
associateFirewallRuleGroup_priority = Lens.lens (\AssociateFirewallRuleGroup' {priority} -> priority) (\s@AssociateFirewallRuleGroup' {} a -> s {priority = a} :: AssociateFirewallRuleGroup)

-- | A name that lets you identify the association, to manage and use it.
associateFirewallRuleGroup_name :: Lens.Lens' AssociateFirewallRuleGroup Prelude.Text
associateFirewallRuleGroup_name = Lens.lens (\AssociateFirewallRuleGroup' {name} -> name) (\s@AssociateFirewallRuleGroup' {} a -> s {name = a} :: AssociateFirewallRuleGroup)

instance Core.AWSRequest AssociateFirewallRuleGroup where
  type
    AWSResponse AssociateFirewallRuleGroup =
      AssociateFirewallRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFirewallRuleGroupResponse'
            Prelude.<$> (x Data..?> "FirewallRuleGroupAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFirewallRuleGroup where
  hashWithSalt _salt AssociateFirewallRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` mutationProtection
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` firewallRuleGroupId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssociateFirewallRuleGroup where
  rnf AssociateFirewallRuleGroup' {..} =
    Prelude.rnf mutationProtection
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf firewallRuleGroupId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders AssociateFirewallRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.AssociateFirewallRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFirewallRuleGroup where
  toJSON AssociateFirewallRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MutationProtection" Data..=)
              Prelude.<$> mutationProtection,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId),
            Prelude.Just
              ("FirewallRuleGroupId" Data..= firewallRuleGroupId),
            Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath AssociateFirewallRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFirewallRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFirewallRuleGroupResponse' smart constructor.
data AssociateFirewallRuleGroupResponse = AssociateFirewallRuleGroupResponse'
  { -- | The association that you just created. The association has an ID that
    -- you can use to identify it in other requests, like update and delete.
    firewallRuleGroupAssociation :: Prelude.Maybe FirewallRuleGroupAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFirewallRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupAssociation', 'associateFirewallRuleGroupResponse_firewallRuleGroupAssociation' - The association that you just created. The association has an ID that
-- you can use to identify it in other requests, like update and delete.
--
-- 'httpStatus', 'associateFirewallRuleGroupResponse_httpStatus' - The response's http status code.
newAssociateFirewallRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFirewallRuleGroupResponse
newAssociateFirewallRuleGroupResponse pHttpStatus_ =
  AssociateFirewallRuleGroupResponse'
    { firewallRuleGroupAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The association that you just created. The association has an ID that
-- you can use to identify it in other requests, like update and delete.
associateFirewallRuleGroupResponse_firewallRuleGroupAssociation :: Lens.Lens' AssociateFirewallRuleGroupResponse (Prelude.Maybe FirewallRuleGroupAssociation)
associateFirewallRuleGroupResponse_firewallRuleGroupAssociation = Lens.lens (\AssociateFirewallRuleGroupResponse' {firewallRuleGroupAssociation} -> firewallRuleGroupAssociation) (\s@AssociateFirewallRuleGroupResponse' {} a -> s {firewallRuleGroupAssociation = a} :: AssociateFirewallRuleGroupResponse)

-- | The response's http status code.
associateFirewallRuleGroupResponse_httpStatus :: Lens.Lens' AssociateFirewallRuleGroupResponse Prelude.Int
associateFirewallRuleGroupResponse_httpStatus = Lens.lens (\AssociateFirewallRuleGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateFirewallRuleGroupResponse' {} a -> s {httpStatus = a} :: AssociateFirewallRuleGroupResponse)

instance
  Prelude.NFData
    AssociateFirewallRuleGroupResponse
  where
  rnf AssociateFirewallRuleGroupResponse' {..} =
    Prelude.rnf firewallRuleGroupAssociation
      `Prelude.seq` Prelude.rnf httpStatus
