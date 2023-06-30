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
-- Module      : Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.OnDemandProvisioningSpecification
import Amazonka.EMR.Types.SpotProvisioningSpecification
import qualified Amazonka.Prelude as Prelude

-- | The launch specification for Spot Instances in the fleet, which
-- determines the defined duration, provisioning timeout behavior, and
-- allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand and Spot
-- Instance allocation strategies are available in Amazon EMR version
-- 5.12.1 and later.
--
-- /See:/ 'newInstanceFleetProvisioningSpecifications' smart constructor.
data InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'
  { -- | The launch specification for On-Demand Instances in the instance fleet,
    -- which determines the allocation strategy.
    --
    -- The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand Instances
    -- allocation strategy is available in Amazon EMR version 5.12.1 and later.
    onDemandSpecification :: Prelude.Maybe OnDemandProvisioningSpecification,
    -- | The launch specification for Spot Instances in the fleet, which
    -- determines the defined duration, provisioning timeout behavior, and
    -- allocation strategy.
    spotSpecification :: Prelude.Maybe SpotProvisioningSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetProvisioningSpecifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandSpecification', 'instanceFleetProvisioningSpecifications_onDemandSpecification' - The launch specification for On-Demand Instances in the instance fleet,
-- which determines the allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand Instances
-- allocation strategy is available in Amazon EMR version 5.12.1 and later.
--
-- 'spotSpecification', 'instanceFleetProvisioningSpecifications_spotSpecification' - The launch specification for Spot Instances in the fleet, which
-- determines the defined duration, provisioning timeout behavior, and
-- allocation strategy.
newInstanceFleetProvisioningSpecifications ::
  InstanceFleetProvisioningSpecifications
newInstanceFleetProvisioningSpecifications =
  InstanceFleetProvisioningSpecifications'
    { onDemandSpecification =
        Prelude.Nothing,
      spotSpecification =
        Prelude.Nothing
    }

-- | The launch specification for On-Demand Instances in the instance fleet,
-- which determines the allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand Instances
-- allocation strategy is available in Amazon EMR version 5.12.1 and later.
instanceFleetProvisioningSpecifications_onDemandSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Prelude.Maybe OnDemandProvisioningSpecification)
instanceFleetProvisioningSpecifications_onDemandSpecification = Lens.lens (\InstanceFleetProvisioningSpecifications' {onDemandSpecification} -> onDemandSpecification) (\s@InstanceFleetProvisioningSpecifications' {} a -> s {onDemandSpecification = a} :: InstanceFleetProvisioningSpecifications)

-- | The launch specification for Spot Instances in the fleet, which
-- determines the defined duration, provisioning timeout behavior, and
-- allocation strategy.
instanceFleetProvisioningSpecifications_spotSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Prelude.Maybe SpotProvisioningSpecification)
instanceFleetProvisioningSpecifications_spotSpecification = Lens.lens (\InstanceFleetProvisioningSpecifications' {spotSpecification} -> spotSpecification) (\s@InstanceFleetProvisioningSpecifications' {} a -> s {spotSpecification = a} :: InstanceFleetProvisioningSpecifications)

instance
  Data.FromJSON
    InstanceFleetProvisioningSpecifications
  where
  parseJSON =
    Data.withObject
      "InstanceFleetProvisioningSpecifications"
      ( \x ->
          InstanceFleetProvisioningSpecifications'
            Prelude.<$> (x Data..:? "OnDemandSpecification")
            Prelude.<*> (x Data..:? "SpotSpecification")
      )

instance
  Prelude.Hashable
    InstanceFleetProvisioningSpecifications
  where
  hashWithSalt
    _salt
    InstanceFleetProvisioningSpecifications' {..} =
      _salt
        `Prelude.hashWithSalt` onDemandSpecification
        `Prelude.hashWithSalt` spotSpecification

instance
  Prelude.NFData
    InstanceFleetProvisioningSpecifications
  where
  rnf InstanceFleetProvisioningSpecifications' {..} =
    Prelude.rnf onDemandSpecification
      `Prelude.seq` Prelude.rnf spotSpecification

instance
  Data.ToJSON
    InstanceFleetProvisioningSpecifications
  where
  toJSON InstanceFleetProvisioningSpecifications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OnDemandSpecification" Data..=)
              Prelude.<$> onDemandSpecification,
            ("SpotSpecification" Data..=)
              Prelude.<$> spotSpecification
          ]
      )
