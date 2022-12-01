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
-- Module      : Amazonka.FMS.Types.ThirdPartyFirewallMissingExpectedRouteTableViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ThirdPartyFirewallMissingExpectedRouteTableViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The violation details for a third-party firewall that\'s not associated
-- with an Firewall Manager managed route table.
--
-- /See:/ 'newThirdPartyFirewallMissingExpectedRouteTableViolation' smart constructor.
data ThirdPartyFirewallMissingExpectedRouteTableViolation = ThirdPartyFirewallMissingExpectedRouteTableViolation'
  { -- | The resource ID of the VPC associated with a fireawll subnet that\'s
    -- causing the violation.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the third-party firewall or VPC resource that\'s causing the
    -- violation.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the current route table that\'s associated with the
    -- subnet, if one is available.
    currentRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the firewall subnet that\'s causing the
    -- violation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the route table that should be associated with the
    -- subnet.
    expectedRouteTable :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyFirewallMissingExpectedRouteTableViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'thirdPartyFirewallMissingExpectedRouteTableViolation_vpc' - The resource ID of the VPC associated with a fireawll subnet that\'s
-- causing the violation.
--
-- 'violationTarget', 'thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget' - The ID of the third-party firewall or VPC resource that\'s causing the
-- violation.
--
-- 'currentRouteTable', 'thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable' - The resource ID of the current route table that\'s associated with the
-- subnet, if one is available.
--
-- 'availabilityZone', 'thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone' - The Availability Zone of the firewall subnet that\'s causing the
-- violation.
--
-- 'expectedRouteTable', 'thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable' - The resource ID of the route table that should be associated with the
-- subnet.
newThirdPartyFirewallMissingExpectedRouteTableViolation ::
  ThirdPartyFirewallMissingExpectedRouteTableViolation
newThirdPartyFirewallMissingExpectedRouteTableViolation =
  ThirdPartyFirewallMissingExpectedRouteTableViolation'
    { vpc =
        Prelude.Nothing,
      violationTarget =
        Prelude.Nothing,
      currentRouteTable =
        Prelude.Nothing,
      availabilityZone =
        Prelude.Nothing,
      expectedRouteTable =
        Prelude.Nothing
    }

-- | The resource ID of the VPC associated with a fireawll subnet that\'s
-- causing the violation.
thirdPartyFirewallMissingExpectedRouteTableViolation_vpc :: Lens.Lens' ThirdPartyFirewallMissingExpectedRouteTableViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingExpectedRouteTableViolation_vpc = Lens.lens (\ThirdPartyFirewallMissingExpectedRouteTableViolation' {vpc} -> vpc) (\s@ThirdPartyFirewallMissingExpectedRouteTableViolation' {} a -> s {vpc = a} :: ThirdPartyFirewallMissingExpectedRouteTableViolation)

-- | The ID of the third-party firewall or VPC resource that\'s causing the
-- violation.
thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget :: Lens.Lens' ThirdPartyFirewallMissingExpectedRouteTableViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget = Lens.lens (\ThirdPartyFirewallMissingExpectedRouteTableViolation' {violationTarget} -> violationTarget) (\s@ThirdPartyFirewallMissingExpectedRouteTableViolation' {} a -> s {violationTarget = a} :: ThirdPartyFirewallMissingExpectedRouteTableViolation)

-- | The resource ID of the current route table that\'s associated with the
-- subnet, if one is available.
thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable :: Lens.Lens' ThirdPartyFirewallMissingExpectedRouteTableViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable = Lens.lens (\ThirdPartyFirewallMissingExpectedRouteTableViolation' {currentRouteTable} -> currentRouteTable) (\s@ThirdPartyFirewallMissingExpectedRouteTableViolation' {} a -> s {currentRouteTable = a} :: ThirdPartyFirewallMissingExpectedRouteTableViolation)

-- | The Availability Zone of the firewall subnet that\'s causing the
-- violation.
thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone :: Lens.Lens' ThirdPartyFirewallMissingExpectedRouteTableViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone = Lens.lens (\ThirdPartyFirewallMissingExpectedRouteTableViolation' {availabilityZone} -> availabilityZone) (\s@ThirdPartyFirewallMissingExpectedRouteTableViolation' {} a -> s {availabilityZone = a} :: ThirdPartyFirewallMissingExpectedRouteTableViolation)

-- | The resource ID of the route table that should be associated with the
-- subnet.
thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable :: Lens.Lens' ThirdPartyFirewallMissingExpectedRouteTableViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable = Lens.lens (\ThirdPartyFirewallMissingExpectedRouteTableViolation' {expectedRouteTable} -> expectedRouteTable) (\s@ThirdPartyFirewallMissingExpectedRouteTableViolation' {} a -> s {expectedRouteTable = a} :: ThirdPartyFirewallMissingExpectedRouteTableViolation)

instance
  Core.FromJSON
    ThirdPartyFirewallMissingExpectedRouteTableViolation
  where
  parseJSON =
    Core.withObject
      "ThirdPartyFirewallMissingExpectedRouteTableViolation"
      ( \x ->
          ThirdPartyFirewallMissingExpectedRouteTableViolation'
            Prelude.<$> (x Core..:? "VPC")
              Prelude.<*> (x Core..:? "ViolationTarget")
              Prelude.<*> (x Core..:? "CurrentRouteTable")
              Prelude.<*> (x Core..:? "AvailabilityZone")
              Prelude.<*> (x Core..:? "ExpectedRouteTable")
      )

instance
  Prelude.Hashable
    ThirdPartyFirewallMissingExpectedRouteTableViolation
  where
  hashWithSalt
    _salt
    ThirdPartyFirewallMissingExpectedRouteTableViolation' {..} =
      _salt `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` currentRouteTable
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` expectedRouteTable

instance
  Prelude.NFData
    ThirdPartyFirewallMissingExpectedRouteTableViolation
  where
  rnf
    ThirdPartyFirewallMissingExpectedRouteTableViolation' {..} =
      Prelude.rnf vpc
        `Prelude.seq` Prelude.rnf violationTarget
        `Prelude.seq` Prelude.rnf currentRouteTable
        `Prelude.seq` Prelude.rnf availabilityZone
        `Prelude.seq` Prelude.rnf expectedRouteTable
