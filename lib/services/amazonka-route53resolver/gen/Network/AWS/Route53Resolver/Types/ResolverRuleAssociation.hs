{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Resolver.Types.ResolverRuleAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Types.ResolverRuleAssociation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53Resolver.Types.ResolverRuleAssociationStatus

-- | In the response to an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DisassociateResolverRule.html DisassociateResolverRule>,
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverRuleAssociations.html ListResolverRuleAssociations>
-- request, provides information about an association between a Resolver
-- rule and a VPC. The association determines which DNS queries that
-- originate in the VPC are forwarded to your network.
--
-- /See:/ 'newResolverRuleAssociation' smart constructor.
data ResolverRuleAssociation = ResolverRuleAssociation'
  { -- | A code that specifies the current status of the association between a
    -- Resolver rule and a VPC.
    status :: Prelude.Maybe ResolverRuleAssociationStatus,
    -- | The ID of the Resolver rule that you associated with the VPC that is
    -- specified by @VPCId@.
    resolverRuleId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that you associated the Resolver rule with.
    vPCId :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the status of the association between a
    -- Resolver rule and a VPC.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of an association between a Resolver rule and a VPC.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association between a Resolver rule and a VPC. Resolver
    -- assigns this value when you submit an
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
    -- request.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverRuleAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'resolverRuleAssociation_status' - A code that specifies the current status of the association between a
-- Resolver rule and a VPC.
--
-- 'resolverRuleId', 'resolverRuleAssociation_resolverRuleId' - The ID of the Resolver rule that you associated with the VPC that is
-- specified by @VPCId@.
--
-- 'vPCId', 'resolverRuleAssociation_vPCId' - The ID of the VPC that you associated the Resolver rule with.
--
-- 'statusMessage', 'resolverRuleAssociation_statusMessage' - A detailed description of the status of the association between a
-- Resolver rule and a VPC.
--
-- 'name', 'resolverRuleAssociation_name' - The name of an association between a Resolver rule and a VPC.
--
-- 'id', 'resolverRuleAssociation_id' - The ID of the association between a Resolver rule and a VPC. Resolver
-- assigns this value when you submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
-- request.
newResolverRuleAssociation ::
  ResolverRuleAssociation
newResolverRuleAssociation =
  ResolverRuleAssociation'
    { status = Prelude.Nothing,
      resolverRuleId = Prelude.Nothing,
      vPCId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A code that specifies the current status of the association between a
-- Resolver rule and a VPC.
resolverRuleAssociation_status :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe ResolverRuleAssociationStatus)
resolverRuleAssociation_status = Lens.lens (\ResolverRuleAssociation' {status} -> status) (\s@ResolverRuleAssociation' {} a -> s {status = a} :: ResolverRuleAssociation)

-- | The ID of the Resolver rule that you associated with the VPC that is
-- specified by @VPCId@.
resolverRuleAssociation_resolverRuleId :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_resolverRuleId = Lens.lens (\ResolverRuleAssociation' {resolverRuleId} -> resolverRuleId) (\s@ResolverRuleAssociation' {} a -> s {resolverRuleId = a} :: ResolverRuleAssociation)

-- | The ID of the VPC that you associated the Resolver rule with.
resolverRuleAssociation_vPCId :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_vPCId = Lens.lens (\ResolverRuleAssociation' {vPCId} -> vPCId) (\s@ResolverRuleAssociation' {} a -> s {vPCId = a} :: ResolverRuleAssociation)

-- | A detailed description of the status of the association between a
-- Resolver rule and a VPC.
resolverRuleAssociation_statusMessage :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_statusMessage = Lens.lens (\ResolverRuleAssociation' {statusMessage} -> statusMessage) (\s@ResolverRuleAssociation' {} a -> s {statusMessage = a} :: ResolverRuleAssociation)

-- | The name of an association between a Resolver rule and a VPC.
resolverRuleAssociation_name :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_name = Lens.lens (\ResolverRuleAssociation' {name} -> name) (\s@ResolverRuleAssociation' {} a -> s {name = a} :: ResolverRuleAssociation)

-- | The ID of the association between a Resolver rule and a VPC. Resolver
-- assigns this value when you submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
-- request.
resolverRuleAssociation_id :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_id = Lens.lens (\ResolverRuleAssociation' {id} -> id) (\s@ResolverRuleAssociation' {} a -> s {id = a} :: ResolverRuleAssociation)

instance Core.FromJSON ResolverRuleAssociation where
  parseJSON =
    Core.withObject
      "ResolverRuleAssociation"
      ( \x ->
          ResolverRuleAssociation'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResolverRuleId")
            Prelude.<*> (x Core..:? "VPCId")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ResolverRuleAssociation

instance Prelude.NFData ResolverRuleAssociation
