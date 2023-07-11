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
-- Module      : Amazonka.Route53.Types.HostedZoneOwner
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HostedZoneOwner where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that identifies a hosted zone that a specified Amazon VPC
-- is associated with and the owner of the hosted zone. If there is a value
-- for @OwningAccount@, there is no value for @OwningService@, and vice
-- versa.
--
-- /See:/ 'newHostedZoneOwner' smart constructor.
data HostedZoneOwner = HostedZoneOwner'
  { -- | If the hosted zone was created by an Amazon Web Services account, or was
    -- created by an Amazon Web Services service that creates hosted zones
    -- using the current account, @OwningAccount@ contains the account ID of
    -- that account. For example, when you use Cloud Map to create a hosted
    -- zone, Cloud Map creates the hosted zone using the current Amazon Web
    -- Services account.
    owningAccount :: Prelude.Maybe Prelude.Text,
    -- | If an Amazon Web Services service uses its own account to create a
    -- hosted zone and associate the specified VPC with that hosted zone,
    -- @OwningService@ contains an abbreviation that identifies the service.
    -- For example, if Amazon Elastic File System (Amazon EFS) created a hosted
    -- zone and associated a VPC with the hosted zone, the value of
    -- @OwningService@ is @efs.amazonaws.com@.
    owningService :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostedZoneOwner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owningAccount', 'hostedZoneOwner_owningAccount' - If the hosted zone was created by an Amazon Web Services account, or was
-- created by an Amazon Web Services service that creates hosted zones
-- using the current account, @OwningAccount@ contains the account ID of
-- that account. For example, when you use Cloud Map to create a hosted
-- zone, Cloud Map creates the hosted zone using the current Amazon Web
-- Services account.
--
-- 'owningService', 'hostedZoneOwner_owningService' - If an Amazon Web Services service uses its own account to create a
-- hosted zone and associate the specified VPC with that hosted zone,
-- @OwningService@ contains an abbreviation that identifies the service.
-- For example, if Amazon Elastic File System (Amazon EFS) created a hosted
-- zone and associated a VPC with the hosted zone, the value of
-- @OwningService@ is @efs.amazonaws.com@.
newHostedZoneOwner ::
  HostedZoneOwner
newHostedZoneOwner =
  HostedZoneOwner'
    { owningAccount = Prelude.Nothing,
      owningService = Prelude.Nothing
    }

-- | If the hosted zone was created by an Amazon Web Services account, or was
-- created by an Amazon Web Services service that creates hosted zones
-- using the current account, @OwningAccount@ contains the account ID of
-- that account. For example, when you use Cloud Map to create a hosted
-- zone, Cloud Map creates the hosted zone using the current Amazon Web
-- Services account.
hostedZoneOwner_owningAccount :: Lens.Lens' HostedZoneOwner (Prelude.Maybe Prelude.Text)
hostedZoneOwner_owningAccount = Lens.lens (\HostedZoneOwner' {owningAccount} -> owningAccount) (\s@HostedZoneOwner' {} a -> s {owningAccount = a} :: HostedZoneOwner)

-- | If an Amazon Web Services service uses its own account to create a
-- hosted zone and associate the specified VPC with that hosted zone,
-- @OwningService@ contains an abbreviation that identifies the service.
-- For example, if Amazon Elastic File System (Amazon EFS) created a hosted
-- zone and associated a VPC with the hosted zone, the value of
-- @OwningService@ is @efs.amazonaws.com@.
hostedZoneOwner_owningService :: Lens.Lens' HostedZoneOwner (Prelude.Maybe Prelude.Text)
hostedZoneOwner_owningService = Lens.lens (\HostedZoneOwner' {owningService} -> owningService) (\s@HostedZoneOwner' {} a -> s {owningService = a} :: HostedZoneOwner)

instance Data.FromXML HostedZoneOwner where
  parseXML x =
    HostedZoneOwner'
      Prelude.<$> (x Data..@? "OwningAccount")
      Prelude.<*> (x Data..@? "OwningService")

instance Prelude.Hashable HostedZoneOwner where
  hashWithSalt _salt HostedZoneOwner' {..} =
    _salt
      `Prelude.hashWithSalt` owningAccount
      `Prelude.hashWithSalt` owningService

instance Prelude.NFData HostedZoneOwner where
  rnf HostedZoneOwner' {..} =
    Prelude.rnf owningAccount
      `Prelude.seq` Prelude.rnf owningService
