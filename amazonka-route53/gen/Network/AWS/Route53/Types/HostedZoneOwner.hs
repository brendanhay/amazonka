{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53.Types.HostedZoneOwner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneOwner where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

-- | A complex type that identifies a hosted zone that a specified Amazon VPC
-- is associated with and the owner of the hosted zone. If there is a value
-- for @OwningAccount@, there is no value for @OwningService@, and vice
-- versa.
--
-- /See:/ 'newHostedZoneOwner' smart constructor.
data HostedZoneOwner = HostedZoneOwner'
  { -- | If the hosted zone was created by an AWS account, or was created by an
    -- AWS service that creates hosted zones using the current account,
    -- @OwningAccount@ contains the account ID of that account. For example,
    -- when you use AWS Cloud Map to create a hosted zone, Cloud Map creates
    -- the hosted zone using the current AWS account.
    owningAccount :: Prelude.Maybe Prelude.Text,
    -- | If an AWS service uses its own account to create a hosted zone and
    -- associate the specified VPC with that hosted zone, @OwningService@
    -- contains an abbreviation that identifies the service. For example, if
    -- Amazon Elastic File System (Amazon EFS) created a hosted zone and
    -- associated a VPC with the hosted zone, the value of @OwningService@ is
    -- @efs.amazonaws.com@.
    owningService :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HostedZoneOwner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owningAccount', 'hostedZoneOwner_owningAccount' - If the hosted zone was created by an AWS account, or was created by an
-- AWS service that creates hosted zones using the current account,
-- @OwningAccount@ contains the account ID of that account. For example,
-- when you use AWS Cloud Map to create a hosted zone, Cloud Map creates
-- the hosted zone using the current AWS account.
--
-- 'owningService', 'hostedZoneOwner_owningService' - If an AWS service uses its own account to create a hosted zone and
-- associate the specified VPC with that hosted zone, @OwningService@
-- contains an abbreviation that identifies the service. For example, if
-- Amazon Elastic File System (Amazon EFS) created a hosted zone and
-- associated a VPC with the hosted zone, the value of @OwningService@ is
-- @efs.amazonaws.com@.
newHostedZoneOwner ::
  HostedZoneOwner
newHostedZoneOwner =
  HostedZoneOwner'
    { owningAccount = Prelude.Nothing,
      owningService = Prelude.Nothing
    }

-- | If the hosted zone was created by an AWS account, or was created by an
-- AWS service that creates hosted zones using the current account,
-- @OwningAccount@ contains the account ID of that account. For example,
-- when you use AWS Cloud Map to create a hosted zone, Cloud Map creates
-- the hosted zone using the current AWS account.
hostedZoneOwner_owningAccount :: Lens.Lens' HostedZoneOwner (Prelude.Maybe Prelude.Text)
hostedZoneOwner_owningAccount = Lens.lens (\HostedZoneOwner' {owningAccount} -> owningAccount) (\s@HostedZoneOwner' {} a -> s {owningAccount = a} :: HostedZoneOwner)

-- | If an AWS service uses its own account to create a hosted zone and
-- associate the specified VPC with that hosted zone, @OwningService@
-- contains an abbreviation that identifies the service. For example, if
-- Amazon Elastic File System (Amazon EFS) created a hosted zone and
-- associated a VPC with the hosted zone, the value of @OwningService@ is
-- @efs.amazonaws.com@.
hostedZoneOwner_owningService :: Lens.Lens' HostedZoneOwner (Prelude.Maybe Prelude.Text)
hostedZoneOwner_owningService = Lens.lens (\HostedZoneOwner' {owningService} -> owningService) (\s@HostedZoneOwner' {} a -> s {owningService = a} :: HostedZoneOwner)

instance Prelude.FromXML HostedZoneOwner where
  parseXML x =
    HostedZoneOwner'
      Prelude.<$> (x Prelude..@? "OwningAccount")
      Prelude.<*> (x Prelude..@? "OwningService")

instance Prelude.Hashable HostedZoneOwner

instance Prelude.NFData HostedZoneOwner
