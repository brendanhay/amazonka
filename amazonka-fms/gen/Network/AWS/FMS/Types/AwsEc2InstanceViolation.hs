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
-- Module      : Network.AWS.FMS.Types.AwsEc2InstanceViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AwsEc2InstanceViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import qualified Network.AWS.Lens as Lens

-- | Violations for an EC2 instance resource.
--
-- /See:/ 'newAwsEc2InstanceViolation' smart constructor.
data AwsEc2InstanceViolation = AwsEc2InstanceViolation'
  { -- | The resource ID of the EC2 instance.
    violationTarget :: Core.Maybe Core.Text,
    -- | Violations for network interfaces associated with the EC2 instance.
    awsEc2NetworkInterfaceViolations :: Core.Maybe [AwsEc2NetworkInterfaceViolation]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AwsEc2InstanceViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTarget', 'awsEc2InstanceViolation_violationTarget' - The resource ID of the EC2 instance.
--
-- 'awsEc2NetworkInterfaceViolations', 'awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations' - Violations for network interfaces associated with the EC2 instance.
newAwsEc2InstanceViolation ::
  AwsEc2InstanceViolation
newAwsEc2InstanceViolation =
  AwsEc2InstanceViolation'
    { violationTarget =
        Core.Nothing,
      awsEc2NetworkInterfaceViolations = Core.Nothing
    }

-- | The resource ID of the EC2 instance.
awsEc2InstanceViolation_violationTarget :: Lens.Lens' AwsEc2InstanceViolation (Core.Maybe Core.Text)
awsEc2InstanceViolation_violationTarget = Lens.lens (\AwsEc2InstanceViolation' {violationTarget} -> violationTarget) (\s@AwsEc2InstanceViolation' {} a -> s {violationTarget = a} :: AwsEc2InstanceViolation)

-- | Violations for network interfaces associated with the EC2 instance.
awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations :: Lens.Lens' AwsEc2InstanceViolation (Core.Maybe [AwsEc2NetworkInterfaceViolation])
awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations = Lens.lens (\AwsEc2InstanceViolation' {awsEc2NetworkInterfaceViolations} -> awsEc2NetworkInterfaceViolations) (\s@AwsEc2InstanceViolation' {} a -> s {awsEc2NetworkInterfaceViolations = a} :: AwsEc2InstanceViolation) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AwsEc2InstanceViolation where
  parseJSON =
    Core.withObject
      "AwsEc2InstanceViolation"
      ( \x ->
          AwsEc2InstanceViolation'
            Core.<$> (x Core..:? "ViolationTarget")
            Core.<*> ( x Core..:? "AwsEc2NetworkInterfaceViolations"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable AwsEc2InstanceViolation

instance Core.NFData AwsEc2InstanceViolation
