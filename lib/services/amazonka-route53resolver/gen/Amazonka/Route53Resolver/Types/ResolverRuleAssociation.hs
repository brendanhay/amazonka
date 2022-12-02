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
-- Module      : Amazonka.Route53Resolver.Types.ResolverRuleAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverRuleAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverRuleAssociationStatus

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
  { -- | The name of an association between a Resolver rule and a VPC.
    name :: Prelude.Maybe Prelude.Text,
    -- | A code that specifies the current status of the association between a
    -- Resolver rule and a VPC.
    status :: Prelude.Maybe ResolverRuleAssociationStatus,
    -- | The ID of the association between a Resolver rule and a VPC. Resolver
    -- assigns this value when you submit an
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
    -- request.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Resolver rule that you associated with the VPC that is
    -- specified by @VPCId@.
    resolverRuleId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that you associated the Resolver rule with.
    vPCId :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the status of the association between a
    -- Resolver rule and a VPC.
    statusMessage :: Prelude.Maybe Prelude.Text
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
-- 'name', 'resolverRuleAssociation_name' - The name of an association between a Resolver rule and a VPC.
--
-- 'status', 'resolverRuleAssociation_status' - A code that specifies the current status of the association between a
-- Resolver rule and a VPC.
--
-- 'id', 'resolverRuleAssociation_id' - The ID of the association between a Resolver rule and a VPC. Resolver
-- assigns this value when you submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
-- request.
--
-- 'resolverRuleId', 'resolverRuleAssociation_resolverRuleId' - The ID of the Resolver rule that you associated with the VPC that is
-- specified by @VPCId@.
--
-- 'vPCId', 'resolverRuleAssociation_vPCId' - The ID of the VPC that you associated the Resolver rule with.
--
-- 'statusMessage', 'resolverRuleAssociation_statusMessage' - A detailed description of the status of the association between a
-- Resolver rule and a VPC.
newResolverRuleAssociation ::
  ResolverRuleAssociation
newResolverRuleAssociation =
  ResolverRuleAssociation'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      resolverRuleId = Prelude.Nothing,
      vPCId = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The name of an association between a Resolver rule and a VPC.
resolverRuleAssociation_name :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_name = Lens.lens (\ResolverRuleAssociation' {name} -> name) (\s@ResolverRuleAssociation' {} a -> s {name = a} :: ResolverRuleAssociation)

-- | A code that specifies the current status of the association between a
-- Resolver rule and a VPC.
resolverRuleAssociation_status :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe ResolverRuleAssociationStatus)
resolverRuleAssociation_status = Lens.lens (\ResolverRuleAssociation' {status} -> status) (\s@ResolverRuleAssociation' {} a -> s {status = a} :: ResolverRuleAssociation)

-- | The ID of the association between a Resolver rule and a VPC. Resolver
-- assigns this value when you submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>
-- request.
resolverRuleAssociation_id :: Lens.Lens' ResolverRuleAssociation (Prelude.Maybe Prelude.Text)
resolverRuleAssociation_id = Lens.lens (\ResolverRuleAssociation' {id} -> id) (\s@ResolverRuleAssociation' {} a -> s {id = a} :: ResolverRuleAssociation)

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

instance Data.FromJSON ResolverRuleAssociation where
  parseJSON =
    Data.withObject
      "ResolverRuleAssociation"
      ( \x ->
          ResolverRuleAssociation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "ResolverRuleId")
            Prelude.<*> (x Data..:? "VPCId")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ResolverRuleAssociation where
  hashWithSalt _salt ResolverRuleAssociation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resolverRuleId
      `Prelude.hashWithSalt` vPCId
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ResolverRuleAssociation where
  rnf ResolverRuleAssociation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resolverRuleId
      `Prelude.seq` Prelude.rnf vPCId
      `Prelude.seq` Prelude.rnf statusMessage
