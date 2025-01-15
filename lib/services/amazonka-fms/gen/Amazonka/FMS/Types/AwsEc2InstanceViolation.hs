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
-- Module      : Amazonka.FMS.Types.AwsEc2InstanceViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AwsEc2InstanceViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.AwsEc2NetworkInterfaceViolation
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for an EC2 instance resource.
--
-- /See:/ 'newAwsEc2InstanceViolation' smart constructor.
data AwsEc2InstanceViolation = AwsEc2InstanceViolation'
  { -- | Violation detail for network interfaces associated with the EC2
    -- instance.
    awsEc2NetworkInterfaceViolations :: Prelude.Maybe [AwsEc2NetworkInterfaceViolation],
    -- | The resource ID of the EC2 instance.
    violationTarget :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2InstanceViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsEc2NetworkInterfaceViolations', 'awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations' - Violation detail for network interfaces associated with the EC2
-- instance.
--
-- 'violationTarget', 'awsEc2InstanceViolation_violationTarget' - The resource ID of the EC2 instance.
newAwsEc2InstanceViolation ::
  AwsEc2InstanceViolation
newAwsEc2InstanceViolation =
  AwsEc2InstanceViolation'
    { awsEc2NetworkInterfaceViolations =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing
    }

-- | Violation detail for network interfaces associated with the EC2
-- instance.
awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations :: Lens.Lens' AwsEc2InstanceViolation (Prelude.Maybe [AwsEc2NetworkInterfaceViolation])
awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations = Lens.lens (\AwsEc2InstanceViolation' {awsEc2NetworkInterfaceViolations} -> awsEc2NetworkInterfaceViolations) (\s@AwsEc2InstanceViolation' {} a -> s {awsEc2NetworkInterfaceViolations = a} :: AwsEc2InstanceViolation) Prelude.. Lens.mapping Lens.coerced

-- | The resource ID of the EC2 instance.
awsEc2InstanceViolation_violationTarget :: Lens.Lens' AwsEc2InstanceViolation (Prelude.Maybe Prelude.Text)
awsEc2InstanceViolation_violationTarget = Lens.lens (\AwsEc2InstanceViolation' {violationTarget} -> violationTarget) (\s@AwsEc2InstanceViolation' {} a -> s {violationTarget = a} :: AwsEc2InstanceViolation)

instance Data.FromJSON AwsEc2InstanceViolation where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceViolation"
      ( \x ->
          AwsEc2InstanceViolation'
            Prelude.<$> ( x
                            Data..:? "AwsEc2NetworkInterfaceViolations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ViolationTarget")
      )

instance Prelude.Hashable AwsEc2InstanceViolation where
  hashWithSalt _salt AwsEc2InstanceViolation' {..} =
    _salt
      `Prelude.hashWithSalt` awsEc2NetworkInterfaceViolations
      `Prelude.hashWithSalt` violationTarget

instance Prelude.NFData AwsEc2InstanceViolation where
  rnf AwsEc2InstanceViolation' {..} =
    Prelude.rnf awsEc2NetworkInterfaceViolations `Prelude.seq`
      Prelude.rnf violationTarget
