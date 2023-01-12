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
-- Module      : Amazonka.Route53.Types.HostedZoneSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HostedZoneSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.HostedZoneOwner

-- | In the response to a @ListHostedZonesByVPC@ request, the
-- @HostedZoneSummaries@ element contains one @HostedZoneSummary@ element
-- for each hosted zone that the specified Amazon VPC is associated with.
-- Each @HostedZoneSummary@ element contains the hosted zone name and ID,
-- and information about who owns the hosted zone.
--
-- /See:/ 'newHostedZoneSummary' smart constructor.
data HostedZoneSummary = HostedZoneSummary'
  { -- | The Route 53 hosted zone ID of a private hosted zone that the specified
    -- VPC is associated with.
    hostedZoneId :: ResourceId,
    -- | The name of the private hosted zone, such as @example.com@.
    name :: Prelude.Text,
    -- | The owner of a private hosted zone that the specified VPC is associated
    -- with. The owner can be either an Amazon Web Services account or an
    -- Amazon Web Services service.
    owner :: HostedZoneOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostedZoneSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'hostedZoneSummary_hostedZoneId' - The Route 53 hosted zone ID of a private hosted zone that the specified
-- VPC is associated with.
--
-- 'name', 'hostedZoneSummary_name' - The name of the private hosted zone, such as @example.com@.
--
-- 'owner', 'hostedZoneSummary_owner' - The owner of a private hosted zone that the specified VPC is associated
-- with. The owner can be either an Amazon Web Services account or an
-- Amazon Web Services service.
newHostedZoneSummary ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  -- | 'owner'
  HostedZoneOwner ->
  HostedZoneSummary
newHostedZoneSummary pHostedZoneId_ pName_ pOwner_ =
  HostedZoneSummary'
    { hostedZoneId = pHostedZoneId_,
      name = pName_,
      owner = pOwner_
    }

-- | The Route 53 hosted zone ID of a private hosted zone that the specified
-- VPC is associated with.
hostedZoneSummary_hostedZoneId :: Lens.Lens' HostedZoneSummary ResourceId
hostedZoneSummary_hostedZoneId = Lens.lens (\HostedZoneSummary' {hostedZoneId} -> hostedZoneId) (\s@HostedZoneSummary' {} a -> s {hostedZoneId = a} :: HostedZoneSummary)

-- | The name of the private hosted zone, such as @example.com@.
hostedZoneSummary_name :: Lens.Lens' HostedZoneSummary Prelude.Text
hostedZoneSummary_name = Lens.lens (\HostedZoneSummary' {name} -> name) (\s@HostedZoneSummary' {} a -> s {name = a} :: HostedZoneSummary)

-- | The owner of a private hosted zone that the specified VPC is associated
-- with. The owner can be either an Amazon Web Services account or an
-- Amazon Web Services service.
hostedZoneSummary_owner :: Lens.Lens' HostedZoneSummary HostedZoneOwner
hostedZoneSummary_owner = Lens.lens (\HostedZoneSummary' {owner} -> owner) (\s@HostedZoneSummary' {} a -> s {owner = a} :: HostedZoneSummary)

instance Data.FromXML HostedZoneSummary where
  parseXML x =
    HostedZoneSummary'
      Prelude.<$> (x Data..@ "HostedZoneId")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "Owner")

instance Prelude.Hashable HostedZoneSummary where
  hashWithSalt _salt HostedZoneSummary' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner

instance Prelude.NFData HostedZoneSummary where
  rnf HostedZoneSummary' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
