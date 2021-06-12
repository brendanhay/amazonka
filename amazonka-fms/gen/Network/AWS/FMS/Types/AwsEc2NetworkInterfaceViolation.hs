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
-- Module      : Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Violations for network interfaces associated with an EC2 instance.
--
-- /See:/ 'newAwsEc2NetworkInterfaceViolation' smart constructor.
data AwsEc2NetworkInterfaceViolation = AwsEc2NetworkInterfaceViolation'
  { -- | The resource ID of the network interface.
    violationTarget :: Core.Maybe Core.Text,
    -- | List of security groups that violate the rules specified in the master
    -- security group of the AWS Firewall Manager policy.
    violatingSecurityGroups :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTarget', 'awsEc2NetworkInterfaceViolation_violationTarget' - The resource ID of the network interface.
--
-- 'violatingSecurityGroups', 'awsEc2NetworkInterfaceViolation_violatingSecurityGroups' - List of security groups that violate the rules specified in the master
-- security group of the AWS Firewall Manager policy.
newAwsEc2NetworkInterfaceViolation ::
  AwsEc2NetworkInterfaceViolation
newAwsEc2NetworkInterfaceViolation =
  AwsEc2NetworkInterfaceViolation'
    { violationTarget =
        Core.Nothing,
      violatingSecurityGroups = Core.Nothing
    }

-- | The resource ID of the network interface.
awsEc2NetworkInterfaceViolation_violationTarget :: Lens.Lens' AwsEc2NetworkInterfaceViolation (Core.Maybe Core.Text)
awsEc2NetworkInterfaceViolation_violationTarget = Lens.lens (\AwsEc2NetworkInterfaceViolation' {violationTarget} -> violationTarget) (\s@AwsEc2NetworkInterfaceViolation' {} a -> s {violationTarget = a} :: AwsEc2NetworkInterfaceViolation)

-- | List of security groups that violate the rules specified in the master
-- security group of the AWS Firewall Manager policy.
awsEc2NetworkInterfaceViolation_violatingSecurityGroups :: Lens.Lens' AwsEc2NetworkInterfaceViolation (Core.Maybe [Core.Text])
awsEc2NetworkInterfaceViolation_violatingSecurityGroups = Lens.lens (\AwsEc2NetworkInterfaceViolation' {violatingSecurityGroups} -> violatingSecurityGroups) (\s@AwsEc2NetworkInterfaceViolation' {} a -> s {violatingSecurityGroups = a} :: AwsEc2NetworkInterfaceViolation) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    AwsEc2NetworkInterfaceViolation
  where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkInterfaceViolation"
      ( \x ->
          AwsEc2NetworkInterfaceViolation'
            Core.<$> (x Core..:? "ViolationTarget")
            Core.<*> ( x Core..:? "ViolatingSecurityGroups"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    AwsEc2NetworkInterfaceViolation

instance Core.NFData AwsEc2NetworkInterfaceViolation
