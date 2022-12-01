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
-- Module      : Amazonka.GameLift.Types.EC2InstanceLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.EC2InstanceLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.EC2InstanceType
import qualified Amazonka.Prelude as Prelude

-- | The GameLift service limits for an Amazon EC2 instance type and current
-- utilization. GameLift allows Amazon Web Services accounts a maximum
-- number of instances, per instance type, per Amazon Web Services Region
-- or location, for use with GameLift. You can request an limit increase
-- for your account by using the __Service limits__ page in the GameLift
-- console.
--
-- __Related actions__
--
-- DescribeEC2InstanceLimits
--
-- /See:/ 'newEC2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { -- | The number of instances that is allowed for the specified instance type
    -- and location.
    instanceLimit :: Prelude.Maybe Prelude.Natural,
    -- | The name of an Amazon EC2 instance type. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
    -- for detailed descriptions.
    eC2InstanceType :: Prelude.Maybe EC2InstanceType,
    -- | The number of instances for the specified type and location that are
    -- currently being used by the Amazon Web Services account.
    currentInstances :: Prelude.Maybe Prelude.Natural,
    -- | An Amazon Web Services Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2InstanceLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceLimit', 'eC2InstanceLimit_instanceLimit' - The number of instances that is allowed for the specified instance type
-- and location.
--
-- 'eC2InstanceType', 'eC2InstanceLimit_eC2InstanceType' - The name of an Amazon EC2 instance type. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
--
-- 'currentInstances', 'eC2InstanceLimit_currentInstances' - The number of instances for the specified type and location that are
-- currently being used by the Amazon Web Services account.
--
-- 'location', 'eC2InstanceLimit_location' - An Amazon Web Services Region code, such as @us-west-2@.
newEC2InstanceLimit ::
  EC2InstanceLimit
newEC2InstanceLimit =
  EC2InstanceLimit'
    { instanceLimit = Prelude.Nothing,
      eC2InstanceType = Prelude.Nothing,
      currentInstances = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The number of instances that is allowed for the specified instance type
-- and location.
eC2InstanceLimit_instanceLimit :: Lens.Lens' EC2InstanceLimit (Prelude.Maybe Prelude.Natural)
eC2InstanceLimit_instanceLimit = Lens.lens (\EC2InstanceLimit' {instanceLimit} -> instanceLimit) (\s@EC2InstanceLimit' {} a -> s {instanceLimit = a} :: EC2InstanceLimit)

-- | The name of an Amazon EC2 instance type. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
eC2InstanceLimit_eC2InstanceType :: Lens.Lens' EC2InstanceLimit (Prelude.Maybe EC2InstanceType)
eC2InstanceLimit_eC2InstanceType = Lens.lens (\EC2InstanceLimit' {eC2InstanceType} -> eC2InstanceType) (\s@EC2InstanceLimit' {} a -> s {eC2InstanceType = a} :: EC2InstanceLimit)

-- | The number of instances for the specified type and location that are
-- currently being used by the Amazon Web Services account.
eC2InstanceLimit_currentInstances :: Lens.Lens' EC2InstanceLimit (Prelude.Maybe Prelude.Natural)
eC2InstanceLimit_currentInstances = Lens.lens (\EC2InstanceLimit' {currentInstances} -> currentInstances) (\s@EC2InstanceLimit' {} a -> s {currentInstances = a} :: EC2InstanceLimit)

-- | An Amazon Web Services Region code, such as @us-west-2@.
eC2InstanceLimit_location :: Lens.Lens' EC2InstanceLimit (Prelude.Maybe Prelude.Text)
eC2InstanceLimit_location = Lens.lens (\EC2InstanceLimit' {location} -> location) (\s@EC2InstanceLimit' {} a -> s {location = a} :: EC2InstanceLimit)

instance Core.FromJSON EC2InstanceLimit where
  parseJSON =
    Core.withObject
      "EC2InstanceLimit"
      ( \x ->
          EC2InstanceLimit'
            Prelude.<$> (x Core..:? "InstanceLimit")
            Prelude.<*> (x Core..:? "EC2InstanceType")
            Prelude.<*> (x Core..:? "CurrentInstances")
            Prelude.<*> (x Core..:? "Location")
      )

instance Prelude.Hashable EC2InstanceLimit where
  hashWithSalt _salt EC2InstanceLimit' {..} =
    _salt `Prelude.hashWithSalt` instanceLimit
      `Prelude.hashWithSalt` eC2InstanceType
      `Prelude.hashWithSalt` currentInstances
      `Prelude.hashWithSalt` location

instance Prelude.NFData EC2InstanceLimit where
  rnf EC2InstanceLimit' {..} =
    Prelude.rnf instanceLimit
      `Prelude.seq` Prelude.rnf eC2InstanceType
      `Prelude.seq` Prelude.rnf currentInstances
      `Prelude.seq` Prelude.rnf location
