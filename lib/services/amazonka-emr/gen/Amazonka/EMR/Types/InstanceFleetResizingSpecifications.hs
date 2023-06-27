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
-- Module      : Amazonka.EMR.Types.InstanceFleetResizingSpecifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetResizingSpecifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.OnDemandResizingSpecification
import Amazonka.EMR.Types.SpotResizingSpecification
import qualified Amazonka.Prelude as Prelude

-- | The resize specification for On-Demand and Spot Instances in the fleet.
--
-- /See:/ 'newInstanceFleetResizingSpecifications' smart constructor.
data InstanceFleetResizingSpecifications = InstanceFleetResizingSpecifications'
  { -- | The resize specification for On-Demand Instances in the instance fleet,
    -- which contains the resize timeout period.
    onDemandResizeSpecification :: Prelude.Maybe OnDemandResizingSpecification,
    -- | The resize specification for Spot Instances in the instance fleet, which
    -- contains the resize timeout period.
    spotResizeSpecification :: Prelude.Maybe SpotResizingSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetResizingSpecifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandResizeSpecification', 'instanceFleetResizingSpecifications_onDemandResizeSpecification' - The resize specification for On-Demand Instances in the instance fleet,
-- which contains the resize timeout period.
--
-- 'spotResizeSpecification', 'instanceFleetResizingSpecifications_spotResizeSpecification' - The resize specification for Spot Instances in the instance fleet, which
-- contains the resize timeout period.
newInstanceFleetResizingSpecifications ::
  InstanceFleetResizingSpecifications
newInstanceFleetResizingSpecifications =
  InstanceFleetResizingSpecifications'
    { onDemandResizeSpecification =
        Prelude.Nothing,
      spotResizeSpecification =
        Prelude.Nothing
    }

-- | The resize specification for On-Demand Instances in the instance fleet,
-- which contains the resize timeout period.
instanceFleetResizingSpecifications_onDemandResizeSpecification :: Lens.Lens' InstanceFleetResizingSpecifications (Prelude.Maybe OnDemandResizingSpecification)
instanceFleetResizingSpecifications_onDemandResizeSpecification = Lens.lens (\InstanceFleetResizingSpecifications' {onDemandResizeSpecification} -> onDemandResizeSpecification) (\s@InstanceFleetResizingSpecifications' {} a -> s {onDemandResizeSpecification = a} :: InstanceFleetResizingSpecifications)

-- | The resize specification for Spot Instances in the instance fleet, which
-- contains the resize timeout period.
instanceFleetResizingSpecifications_spotResizeSpecification :: Lens.Lens' InstanceFleetResizingSpecifications (Prelude.Maybe SpotResizingSpecification)
instanceFleetResizingSpecifications_spotResizeSpecification = Lens.lens (\InstanceFleetResizingSpecifications' {spotResizeSpecification} -> spotResizeSpecification) (\s@InstanceFleetResizingSpecifications' {} a -> s {spotResizeSpecification = a} :: InstanceFleetResizingSpecifications)

instance
  Data.FromJSON
    InstanceFleetResizingSpecifications
  where
  parseJSON =
    Data.withObject
      "InstanceFleetResizingSpecifications"
      ( \x ->
          InstanceFleetResizingSpecifications'
            Prelude.<$> (x Data..:? "OnDemandResizeSpecification")
            Prelude.<*> (x Data..:? "SpotResizeSpecification")
      )

instance
  Prelude.Hashable
    InstanceFleetResizingSpecifications
  where
  hashWithSalt
    _salt
    InstanceFleetResizingSpecifications' {..} =
      _salt
        `Prelude.hashWithSalt` onDemandResizeSpecification
        `Prelude.hashWithSalt` spotResizeSpecification

instance
  Prelude.NFData
    InstanceFleetResizingSpecifications
  where
  rnf InstanceFleetResizingSpecifications' {..} =
    Prelude.rnf onDemandResizeSpecification
      `Prelude.seq` Prelude.rnf spotResizeSpecification

instance
  Data.ToJSON
    InstanceFleetResizingSpecifications
  where
  toJSON InstanceFleetResizingSpecifications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OnDemandResizeSpecification" Data..=)
              Prelude.<$> onDemandResizeSpecification,
            ("SpotResizeSpecification" Data..=)
              Prelude.<$> spotResizeSpecification
          ]
      )
