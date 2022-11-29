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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.RackPhysicalProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The power option that you can provide for hardware.
    powerPhase :: Prelude.Maybe PowerPhase,
    -- | The power draw available at the hardware placement position for the
    -- rack.
    powerDrawKva :: Prelude.Maybe PowerDrawKva,
    -- | The type of fiber used to attach the Outpost to the network.
    fiberOpticCableType :: Prelude.Maybe FiberOpticCableType,
    -- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
    -- 2000 lbs (907 kg).
    maximumSupportedWeightLbs :: Prelude.Maybe MaximumSupportedWeightLbs,
    -- | The power connector for the hardware.
    powerConnector :: Prelude.Maybe PowerConnector,
    -- | The type of optical standard used to attach the Outpost to the network.
    -- This field is dependent on uplink speed, fiber type, and distance to the
    -- upstream device. For more information about networking requirements for
    -- racks, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
    -- in the Amazon Web Services Outposts User Guide.
    opticalStandard :: Prelude.Maybe OpticalStandard,
    -- | The position of the power feed.
    powerFeedDrop :: Prelude.Maybe PowerFeedDrop,
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
-- 'powerPhase', 'rackPhysicalProperties_powerPhase' - The power option that you can provide for hardware.
--
-- 'powerDrawKva', 'rackPhysicalProperties_powerDrawKva' - The power draw available at the hardware placement position for the
-- rack.
--
-- 'fiberOpticCableType', 'rackPhysicalProperties_fiberOpticCableType' - The type of fiber used to attach the Outpost to the network.
--
-- 'maximumSupportedWeightLbs', 'rackPhysicalProperties_maximumSupportedWeightLbs' - The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000 lbs (907 kg).
--
-- 'powerConnector', 'rackPhysicalProperties_powerConnector' - The power connector for the hardware.
--
-- 'opticalStandard', 'rackPhysicalProperties_opticalStandard' - The type of optical standard used to attach the Outpost to the network.
-- This field is dependent on uplink speed, fiber type, and distance to the
-- upstream device. For more information about networking requirements for
-- racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
--
-- 'powerFeedDrop', 'rackPhysicalProperties_powerFeedDrop' - The position of the power feed.
--
-- 'uplinkCount', 'rackPhysicalProperties_uplinkCount' - The number of uplinks each Outpost network device.
--
-- 'uplinkGbps', 'rackPhysicalProperties_uplinkGbps' - The uplink speed the rack supports for the connection to the Region.
newRackPhysicalProperties ::
  RackPhysicalProperties
newRackPhysicalProperties =
  RackPhysicalProperties'
    { powerPhase =
        Prelude.Nothing,
      powerDrawKva = Prelude.Nothing,
      fiberOpticCableType = Prelude.Nothing,
      maximumSupportedWeightLbs = Prelude.Nothing,
      powerConnector = Prelude.Nothing,
      opticalStandard = Prelude.Nothing,
      powerFeedDrop = Prelude.Nothing,
      uplinkCount = Prelude.Nothing,
      uplinkGbps = Prelude.Nothing
    }

-- | The power option that you can provide for hardware.
rackPhysicalProperties_powerPhase :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerPhase)
rackPhysicalProperties_powerPhase = Lens.lens (\RackPhysicalProperties' {powerPhase} -> powerPhase) (\s@RackPhysicalProperties' {} a -> s {powerPhase = a} :: RackPhysicalProperties)

-- | The power draw available at the hardware placement position for the
-- rack.
rackPhysicalProperties_powerDrawKva :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerDrawKva)
rackPhysicalProperties_powerDrawKva = Lens.lens (\RackPhysicalProperties' {powerDrawKva} -> powerDrawKva) (\s@RackPhysicalProperties' {} a -> s {powerDrawKva = a} :: RackPhysicalProperties)

-- | The type of fiber used to attach the Outpost to the network.
rackPhysicalProperties_fiberOpticCableType :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe FiberOpticCableType)
rackPhysicalProperties_fiberOpticCableType = Lens.lens (\RackPhysicalProperties' {fiberOpticCableType} -> fiberOpticCableType) (\s@RackPhysicalProperties' {} a -> s {fiberOpticCableType = a} :: RackPhysicalProperties)

-- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000 lbs (907 kg).
rackPhysicalProperties_maximumSupportedWeightLbs :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe MaximumSupportedWeightLbs)
rackPhysicalProperties_maximumSupportedWeightLbs = Lens.lens (\RackPhysicalProperties' {maximumSupportedWeightLbs} -> maximumSupportedWeightLbs) (\s@RackPhysicalProperties' {} a -> s {maximumSupportedWeightLbs = a} :: RackPhysicalProperties)

-- | The power connector for the hardware.
rackPhysicalProperties_powerConnector :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerConnector)
rackPhysicalProperties_powerConnector = Lens.lens (\RackPhysicalProperties' {powerConnector} -> powerConnector) (\s@RackPhysicalProperties' {} a -> s {powerConnector = a} :: RackPhysicalProperties)

-- | The type of optical standard used to attach the Outpost to the network.
-- This field is dependent on uplink speed, fiber type, and distance to the
-- upstream device. For more information about networking requirements for
-- racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
rackPhysicalProperties_opticalStandard :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe OpticalStandard)
rackPhysicalProperties_opticalStandard = Lens.lens (\RackPhysicalProperties' {opticalStandard} -> opticalStandard) (\s@RackPhysicalProperties' {} a -> s {opticalStandard = a} :: RackPhysicalProperties)

-- | The position of the power feed.
rackPhysicalProperties_powerFeedDrop :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe PowerFeedDrop)
rackPhysicalProperties_powerFeedDrop = Lens.lens (\RackPhysicalProperties' {powerFeedDrop} -> powerFeedDrop) (\s@RackPhysicalProperties' {} a -> s {powerFeedDrop = a} :: RackPhysicalProperties)

-- | The number of uplinks each Outpost network device.
rackPhysicalProperties_uplinkCount :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe UplinkCount)
rackPhysicalProperties_uplinkCount = Lens.lens (\RackPhysicalProperties' {uplinkCount} -> uplinkCount) (\s@RackPhysicalProperties' {} a -> s {uplinkCount = a} :: RackPhysicalProperties)

-- | The uplink speed the rack supports for the connection to the Region.
rackPhysicalProperties_uplinkGbps :: Lens.Lens' RackPhysicalProperties (Prelude.Maybe UplinkGbps)
rackPhysicalProperties_uplinkGbps = Lens.lens (\RackPhysicalProperties' {uplinkGbps} -> uplinkGbps) (\s@RackPhysicalProperties' {} a -> s {uplinkGbps = a} :: RackPhysicalProperties)

instance Core.FromJSON RackPhysicalProperties where
  parseJSON =
    Core.withObject
      "RackPhysicalProperties"
      ( \x ->
          RackPhysicalProperties'
            Prelude.<$> (x Core..:? "PowerPhase")
            Prelude.<*> (x Core..:? "PowerDrawKva")
            Prelude.<*> (x Core..:? "FiberOpticCableType")
            Prelude.<*> (x Core..:? "MaximumSupportedWeightLbs")
            Prelude.<*> (x Core..:? "PowerConnector")
            Prelude.<*> (x Core..:? "OpticalStandard")
            Prelude.<*> (x Core..:? "PowerFeedDrop")
            Prelude.<*> (x Core..:? "UplinkCount")
            Prelude.<*> (x Core..:? "UplinkGbps")
      )

instance Prelude.Hashable RackPhysicalProperties where
  hashWithSalt _salt RackPhysicalProperties' {..} =
    _salt `Prelude.hashWithSalt` powerPhase
      `Prelude.hashWithSalt` powerDrawKva
      `Prelude.hashWithSalt` fiberOpticCableType
      `Prelude.hashWithSalt` maximumSupportedWeightLbs
      `Prelude.hashWithSalt` powerConnector
      `Prelude.hashWithSalt` opticalStandard
      `Prelude.hashWithSalt` powerFeedDrop
      `Prelude.hashWithSalt` uplinkCount
      `Prelude.hashWithSalt` uplinkGbps

instance Prelude.NFData RackPhysicalProperties where
  rnf RackPhysicalProperties' {..} =
    Prelude.rnf powerPhase
      `Prelude.seq` Prelude.rnf powerDrawKva
      `Prelude.seq` Prelude.rnf fiberOpticCableType
      `Prelude.seq` Prelude.rnf maximumSupportedWeightLbs
      `Prelude.seq` Prelude.rnf powerConnector
      `Prelude.seq` Prelude.rnf opticalStandard
      `Prelude.seq` Prelude.rnf powerFeedDrop
      `Prelude.seq` Prelude.rnf uplinkCount
      `Prelude.seq` Prelude.rnf uplinkGbps

instance Core.ToJSON RackPhysicalProperties where
  toJSON RackPhysicalProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PowerPhase" Core..=) Prelude.<$> powerPhase,
            ("PowerDrawKva" Core..=) Prelude.<$> powerDrawKva,
            ("FiberOpticCableType" Core..=)
              Prelude.<$> fiberOpticCableType,
            ("MaximumSupportedWeightLbs" Core..=)
              Prelude.<$> maximumSupportedWeightLbs,
            ("PowerConnector" Core..=)
              Prelude.<$> powerConnector,
            ("OpticalStandard" Core..=)
              Prelude.<$> opticalStandard,
            ("PowerFeedDrop" Core..=) Prelude.<$> powerFeedDrop,
            ("UplinkCount" Core..=) Prelude.<$> uplinkCount,
            ("UplinkGbps" Core..=) Prelude.<$> uplinkGbps
          ]
      )
