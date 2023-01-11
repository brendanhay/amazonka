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
-- Module      : Amazonka.Outposts.Types.RackPhysicalProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.RackPhysicalProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.FiberOpticCableType
import Amazonka.Outposts.Types.MaximumSupportedWeightLbs
import Amazonka.Outposts.Types.OpticalStandard
import Amazonka.Outposts.Types.PowerConnector
import Amazonka.Outposts.Types.PowerDrawKva
import Amazonka.Outposts.Types.PowerFeedDrop
import Amazonka.Outposts.Types.PowerPhase
import Amazonka.Outposts.Types.UplinkCount
import Amazonka.Outposts.Types.UplinkGbps
import qualified Amazonka.Prelude as Prelude

-- | Information about the physical and logistical details for racks at
-- sites. For more information about hardware requirements for racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#checklist Network readiness checklist>
-- in the Amazon Web Services Outposts User Guide.
--
-- /See:/ 'newRackPhysicalProperties' smart constructor.
data RackPhysicalProperties = RackPhysicalProperties'
  { -- | The type of fiber used to attach the Outpost to the network.
    fiberOpticCableType :: Prelude.Maybe FiberOpticCableType,
    -- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
    -- 2000 lbs (907 kg).
    maximumSupportedWeightLbs :: Prelude.Maybe MaximumSupportedWeightLbs,
    -- | The type of optical standard used to attach the Outpost to the network.
    -- This field is dependent on uplink speed, fiber type, and distance to the
    -- upstream device. For more information about networking requirements for
    -- racks, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
    -- in the Amazon Web Services Outposts User Guide.
    opticalStandard :: Prelude.Maybe OpticalStandard,
    -- | The power connector for the hardware.
    powerConnector :: Prelude.Maybe PowerConnector,
    -- | The power draw available at the hardware placement position for the
    -- rack.
    powerDrawKva :: Prelude.Maybe PowerDrawKva,
    -- | The position of the power feed.
    powerFeedDrop :: Prelude.Maybe PowerFeedDrop,
    -- | The power option that you can provide for hardware.
    powerPhase :: Prelude.Maybe PowerPhase,
    -- | The number of uplinks each Outpost network device.
    uplinkCount :: Prelude.Maybe UplinkCount,
    -- | The uplink speed the rack supports for the connection to the Region.
    uplinkGbps :: Prelude.Maybe UplinkGbps
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RackPhysicalProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fiberOpticCableType', 'rackPhysicalProperties_fiberOpticCableType' - The type of fiber used to attach the Outpost to the network.
--
-- 'maximumSupportedWeightLbs', 'rackPhysicalProperties_maximumSupportedWeightLbs' - The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000 lbs (907 kg).
--
-- 'opticalStandard', 'rackPhysicalProperties_opticalStandard' - The type of optical standard used to attach the Outpost to the network.
-- This field is dependent on uplink speed, fiber type, and distance to the
-- upstream device. For more information about networking requirements for
-- racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
--
-- 'powerConnector', 'rackPhysicalProperties_powerConnector' - The power connector for the hardware.
--
-- 'powerDrawKva', 'rackPhysicalProperties_powerDrawKva' - The power draw available at the hardware placement position for the
-- rack.
--
-- 'powerFeedDrop', 'rackPhysicalProperties_powerFeedDrop' - The position of the power feed.
--
-- 'powerPhase', 'rackPhysicalProperties_powerPhase' - The power option that you can provide for hardware.
--
-- 'uplinkCount', 'rackPhysicalProperties_uplinkCount' - The number of uplinks each Outpost network device.
--
-- 'uplinkGbps', 'rackPhysicalProperties_uplinkGbps' - The uplink speed the rack supports for the connection to the Region.
newRackPhysicalProperties ::
  RackPhysicalProperties
newRackPhysicalProperties =
  RackPhysicalProperties'
    { fiberOpticCableType =
        Prelude.Nothing,
      maximumSupportedWeightLbs = Prelude.Nothing,
      opticalStandard = Prelude.Nothing,
      powerConnector = Prelude.Nothing,
      powerDrawKva = Prelude.Nothing,
      powerFeedDrop = Prelude.Nothing,
      powerPhase = Prelude.Nothing,
      uplinkCount = Prelude.Nothing,
      uplinkGbps = Prelude.Nothing
    }

-- | The type of fiber used to attach the Outpost to the network.
rackPhysicalProperties_fiberOpticCableType :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe FiberOpticCableType)
rackPhysicalProperties_fiberOpticCableType = Lens.lens (\RackPhysicalProperties' {fiberOpticCableType} -> fiberOpticCableType) (\s@RackPhysicalProperties' {} a -> s {fiberOpticCableType = a} :: RackPhysicalProperties)

-- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000 lbs (907 kg).
rackPhysicalProperties_maximumSupportedWeightLbs :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe MaximumSupportedWeightLbs)
rackPhysicalProperties_maximumSupportedWeightLbs = Lens.lens (\RackPhysicalProperties' {maximumSupportedWeightLbs} -> maximumSupportedWeightLbs) (\s@RackPhysicalProperties' {} a -> s {maximumSupportedWeightLbs = a} :: RackPhysicalProperties)

-- | The type of optical standard used to attach the Outpost to the network.
-- This field is dependent on uplink speed, fiber type, and distance to the
-- upstream device. For more information about networking requirements for
-- racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
rackPhysicalProperties_opticalStandard :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe OpticalStandard)
rackPhysicalProperties_opticalStandard = Lens.lens (\RackPhysicalProperties' {opticalStandard} -> opticalStandard) (\s@RackPhysicalProperties' {} a -> s {opticalStandard = a} :: RackPhysicalProperties)

-- | The power connector for the hardware.
rackPhysicalProperties_powerConnector :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerConnector)
rackPhysicalProperties_powerConnector = Lens.lens (\RackPhysicalProperties' {powerConnector} -> powerConnector) (\s@RackPhysicalProperties' {} a -> s {powerConnector = a} :: RackPhysicalProperties)

-- | The power draw available at the hardware placement position for the
-- rack.
rackPhysicalProperties_powerDrawKva :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerDrawKva)
rackPhysicalProperties_powerDrawKva = Lens.lens (\RackPhysicalProperties' {powerDrawKva} -> powerDrawKva) (\s@RackPhysicalProperties' {} a -> s {powerDrawKva = a} :: RackPhysicalProperties)

-- | The position of the power feed.
rackPhysicalProperties_powerFeedDrop :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerFeedDrop)
rackPhysicalProperties_powerFeedDrop = Lens.lens (\RackPhysicalProperties' {powerFeedDrop} -> powerFeedDrop) (\s@RackPhysicalProperties' {} a -> s {powerFeedDrop = a} :: RackPhysicalProperties)

-- | The power option that you can provide for hardware.
rackPhysicalProperties_powerPhase :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerPhase)
rackPhysicalProperties_powerPhase = Lens.lens (\RackPhysicalProperties' {powerPhase} -> powerPhase) (\s@RackPhysicalProperties' {} a -> s {powerPhase = a} :: RackPhysicalProperties)

-- | The number of uplinks each Outpost network device.
rackPhysicalProperties_uplinkCount :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe UplinkCount)
rackPhysicalProperties_uplinkCount = Lens.lens (\RackPhysicalProperties' {uplinkCount} -> uplinkCount) (\s@RackPhysicalProperties' {} a -> s {uplinkCount = a} :: RackPhysicalProperties)

-- | The uplink speed the rack supports for the connection to the Region.
rackPhysicalProperties_uplinkGbps :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe UplinkGbps)
rackPhysicalProperties_uplinkGbps = Lens.lens (\RackPhysicalProperties' {uplinkGbps} -> uplinkGbps) (\s@RackPhysicalProperties' {} a -> s {uplinkGbps = a} :: RackPhysicalProperties)

instance Data.FromJSON RackPhysicalProperties where
  parseJSON =
    Data.withObject
      "RackPhysicalProperties"
      ( \x ->
          RackPhysicalProperties'
            Prelude.<$> (x Data..:? "FiberOpticCableType")
            Prelude.<*> (x Data..:? "MaximumSupportedWeightLbs")
            Prelude.<*> (x Data..:? "OpticalStandard")
            Prelude.<*> (x Data..:? "PowerConnector")
            Prelude.<*> (x Data..:? "PowerDrawKva")
            Prelude.<*> (x Data..:? "PowerFeedDrop")
            Prelude.<*> (x Data..:? "PowerPhase")
            Prelude.<*> (x Data..:? "UplinkCount")
            Prelude.<*> (x Data..:? "UplinkGbps")
      )

instance Prelude.Hashable RackPhysicalProperties where
  hashWithSalt _salt RackPhysicalProperties' {..} =
    _salt `Prelude.hashWithSalt` fiberOpticCableType
      `Prelude.hashWithSalt` maximumSupportedWeightLbs
      `Prelude.hashWithSalt` opticalStandard
      `Prelude.hashWithSalt` powerConnector
      `Prelude.hashWithSalt` powerDrawKva
      `Prelude.hashWithSalt` powerFeedDrop
      `Prelude.hashWithSalt` powerPhase
      `Prelude.hashWithSalt` uplinkCount
      `Prelude.hashWithSalt` uplinkGbps

instance Prelude.NFData RackPhysicalProperties where
  rnf RackPhysicalProperties' {..} =
    Prelude.rnf fiberOpticCableType
      `Prelude.seq` Prelude.rnf maximumSupportedWeightLbs
      `Prelude.seq` Prelude.rnf opticalStandard
      `Prelude.seq` Prelude.rnf powerConnector
      `Prelude.seq` Prelude.rnf powerDrawKva
      `Prelude.seq` Prelude.rnf powerFeedDrop
      `Prelude.seq` Prelude.rnf powerPhase
      `Prelude.seq` Prelude.rnf uplinkCount
      `Prelude.seq` Prelude.rnf uplinkGbps

instance Data.ToJSON RackPhysicalProperties where
  toJSON RackPhysicalProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FiberOpticCableType" Data..=)
              Prelude.<$> fiberOpticCableType,
            ("MaximumSupportedWeightLbs" Data..=)
              Prelude.<$> maximumSupportedWeightLbs,
            ("OpticalStandard" Data..=)
              Prelude.<$> opticalStandard,
            ("PowerConnector" Data..=)
              Prelude.<$> powerConnector,
            ("PowerDrawKva" Data..=) Prelude.<$> powerDrawKva,
            ("PowerFeedDrop" Data..=) Prelude.<$> powerFeedDrop,
            ("PowerPhase" Data..=) Prelude.<$> powerPhase,
            ("UplinkCount" Data..=) Prelude.<$> uplinkCount,
            ("UplinkGbps" Data..=) Prelude.<$> uplinkGbps
          ]
      )
