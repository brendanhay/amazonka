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
-- Module      : Amazonka.GameLift.Types.InstanceDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.InstanceDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.GameServerGroupInstanceType
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the GameLift FleetIQ and game server
-- groups.__
--
-- An allowed instance type for a game server group. All game server groups
-- must have at least two instance types defined for it. GameLift FleetIQ
-- periodically evaluates each defined instance type for viability. It then
-- updates the Auto Scaling group with the list of viable instance types.
--
-- /See:/ 'newInstanceDefinition' smart constructor.
data InstanceDefinition = InstanceDefinition'
  { -- | Instance weighting that indicates how much this instance type
    -- contributes to the total capacity of a game server group. Instance
    -- weights are used by GameLift FleetIQ to calculate the instance type\'s
    -- cost per unit hour and better identify the most cost-effective options.
    -- For detailed information on weighting instance capacity, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting>
    -- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. Default
    -- value is \"1\".
    weightedCapacity :: Prelude.Maybe Prelude.Text,
    -- | An Amazon EC2 instance type designation.
    instanceType :: GameServerGroupInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weightedCapacity', 'instanceDefinition_weightedCapacity' - Instance weighting that indicates how much this instance type
-- contributes to the total capacity of a game server group. Instance
-- weights are used by GameLift FleetIQ to calculate the instance type\'s
-- cost per unit hour and better identify the most cost-effective options.
-- For detailed information on weighting instance capacity, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting>
-- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. Default
-- value is \"1\".
--
-- 'instanceType', 'instanceDefinition_instanceType' - An Amazon EC2 instance type designation.
newInstanceDefinition ::
  -- | 'instanceType'
  GameServerGroupInstanceType ->
  InstanceDefinition
newInstanceDefinition pInstanceType_ =
  InstanceDefinition'
    { weightedCapacity =
        Prelude.Nothing,
      instanceType = pInstanceType_
    }

-- | Instance weighting that indicates how much this instance type
-- contributes to the total capacity of a game server group. Instance
-- weights are used by GameLift FleetIQ to calculate the instance type\'s
-- cost per unit hour and better identify the most cost-effective options.
-- For detailed information on weighting instance capacity, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting>
-- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. Default
-- value is \"1\".
instanceDefinition_weightedCapacity :: Lens.Lens' InstanceDefinition (Prelude.Maybe Prelude.Text)
instanceDefinition_weightedCapacity = Lens.lens (\InstanceDefinition' {weightedCapacity} -> weightedCapacity) (\s@InstanceDefinition' {} a -> s {weightedCapacity = a} :: InstanceDefinition)

-- | An Amazon EC2 instance type designation.
instanceDefinition_instanceType :: Lens.Lens' InstanceDefinition GameServerGroupInstanceType
instanceDefinition_instanceType = Lens.lens (\InstanceDefinition' {instanceType} -> instanceType) (\s@InstanceDefinition' {} a -> s {instanceType = a} :: InstanceDefinition)

instance Data.FromJSON InstanceDefinition where
  parseJSON =
    Data.withObject
      "InstanceDefinition"
      ( \x ->
          InstanceDefinition'
            Prelude.<$> (x Data..:? "WeightedCapacity")
            Prelude.<*> (x Data..: "InstanceType")
      )

instance Prelude.Hashable InstanceDefinition where
  hashWithSalt _salt InstanceDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` weightedCapacity
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData InstanceDefinition where
  rnf InstanceDefinition' {..} =
    Prelude.rnf weightedCapacity `Prelude.seq`
      Prelude.rnf instanceType

instance Data.ToJSON InstanceDefinition where
  toJSON InstanceDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WeightedCapacity" Data..=)
              Prelude.<$> weightedCapacity,
            Prelude.Just ("InstanceType" Data..= instanceType)
          ]
      )
