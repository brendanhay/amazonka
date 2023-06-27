{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.UpdateSiteRackPhysicalProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the physical and logistical details for a rack at a site. For
-- more information about hardware requirements for racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#checklist Network readiness checklist>
-- in the Amazon Web Services Outposts User Guide.
--
-- To update a rack at a site with an order of @IN_PROGRESS@, you must wait
-- for the order to complete or cancel the order.
module Amazonka.Outposts.UpdateSiteRackPhysicalProperties
  ( -- * Creating a Request
    UpdateSiteRackPhysicalProperties (..),
    newUpdateSiteRackPhysicalProperties,

    -- * Request Lenses
    updateSiteRackPhysicalProperties_fiberOpticCableType,
    updateSiteRackPhysicalProperties_maximumSupportedWeightLbs,
    updateSiteRackPhysicalProperties_opticalStandard,
    updateSiteRackPhysicalProperties_powerConnector,
    updateSiteRackPhysicalProperties_powerDrawKva,
    updateSiteRackPhysicalProperties_powerFeedDrop,
    updateSiteRackPhysicalProperties_powerPhase,
    updateSiteRackPhysicalProperties_uplinkCount,
    updateSiteRackPhysicalProperties_uplinkGbps,
    updateSiteRackPhysicalProperties_siteId,

    -- * Destructuring the Response
    UpdateSiteRackPhysicalPropertiesResponse (..),
    newUpdateSiteRackPhysicalPropertiesResponse,

    -- * Response Lenses
    updateSiteRackPhysicalPropertiesResponse_site,
    updateSiteRackPhysicalPropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSiteRackPhysicalProperties' smart constructor.
data UpdateSiteRackPhysicalProperties = UpdateSiteRackPhysicalProperties'
  { -- | The type of fiber that you will use to attach the Outpost to your
    -- network.
    fiberOpticCableType :: Prelude.Maybe FiberOpticCableType,
    -- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
    -- 2000lbs.
    maximumSupportedWeightLbs :: Prelude.Maybe MaximumSupportedWeightLbs,
    -- | The type of optical standard that you will use to attach the Outpost to
    -- your network. This field is dependent on uplink speed, fiber type, and
    -- distance to the upstream device. For more information about networking
    -- requirements for racks, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
    -- in the Amazon Web Services Outposts User Guide.
    --
    -- -   @OPTIC_10GBASE_SR@: 10GBASE-SR
    --
    -- -   @OPTIC_10GBASE_IR@: 10GBASE-IR
    --
    -- -   @OPTIC_10GBASE_LR@: 10GBASE-LR
    --
    -- -   @OPTIC_40GBASE_SR@: 40GBASE-SR
    --
    -- -   @OPTIC_40GBASE_ESR@: 40GBASE-ESR
    --
    -- -   @OPTIC_40GBASE_IR4_LR4L@: 40GBASE-IR (LR4L)
    --
    -- -   @OPTIC_40GBASE_LR4@: 40GBASE-LR4
    --
    -- -   @OPTIC_100GBASE_SR4@: 100GBASE-SR4
    --
    -- -   @OPTIC_100GBASE_CWDM4@: 100GBASE-CWDM4
    --
    -- -   @OPTIC_100GBASE_LR4@: 100GBASE-LR4
    --
    -- -   @OPTIC_100G_PSM4_MSA@: 100G PSM4 MSA
    --
    -- -   @OPTIC_1000BASE_LX@: 1000Base-LX
    --
    -- -   @OPTIC_1000BASE_SX@ : 1000Base-SX
    opticalStandard :: Prelude.Maybe OpticalStandard,
    -- | The power connector that Amazon Web Services should plan to provide for
    -- connections to the hardware. Note the correlation between @PowerPhase@
    -- and @PowerConnector@.
    --
    -- -   Single-phase AC feed
    --
    --     -   __L6-30P__ – (common in US); 30A; single phase
    --
    --     -   __IEC309 (blue)__ – P+N+E, 6hr; 32 A; single phase
    --
    -- -   Three-phase AC feed
    --
    --     -   __AH530P7W (red)__ – 3P+N+E, 7hr; 30A; three phase
    --
    --     -   __AH532P6W (red)__ – 3P+N+E, 6hr; 32A; three phase
    powerConnector :: Prelude.Maybe PowerConnector,
    -- | The power draw, in kVA, available at the hardware placement position for
    -- the rack.
    powerDrawKva :: Prelude.Maybe PowerDrawKva,
    -- | Indicates whether the power feed comes above or below the rack.
    powerFeedDrop :: Prelude.Maybe PowerFeedDrop,
    -- | The power option that you can provide for hardware.
    --
    -- -   Single-phase AC feed: 200 V to 277 V, 50 Hz or 60 Hz
    --
    -- -   Three-phase AC feed: 346 V to 480 V, 50 Hz or 60 Hz
    powerPhase :: Prelude.Maybe PowerPhase,
    -- | Racks come with two Outpost network devices. Depending on the supported
    -- uplink speed at the site, the Outpost network devices provide a variable
    -- number of uplinks. Specify the number of uplinks for each Outpost
    -- network device that you intend to use to connect the rack to your
    -- network. Note the correlation between @UplinkGbps@ and @UplinkCount@.
    --
    -- -   1Gbps - Uplinks available: 1, 2, 4, 6, 8
    --
    -- -   10Gbps - Uplinks available: 1, 2, 4, 8, 12, 16
    --
    -- -   40 and 100 Gbps- Uplinks available: 1, 2, 4
    uplinkCount :: Prelude.Maybe UplinkCount,
    -- | The uplink speed the rack should support for the connection to the
    -- Region.
    uplinkGbps :: Prelude.Maybe UplinkGbps,
    -- | The ID or the Amazon Resource Name (ARN) of the site.
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteRackPhysicalProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fiberOpticCableType', 'updateSiteRackPhysicalProperties_fiberOpticCableType' - The type of fiber that you will use to attach the Outpost to your
-- network.
--
-- 'maximumSupportedWeightLbs', 'updateSiteRackPhysicalProperties_maximumSupportedWeightLbs' - The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000lbs.
--
-- 'opticalStandard', 'updateSiteRackPhysicalProperties_opticalStandard' - The type of optical standard that you will use to attach the Outpost to
-- your network. This field is dependent on uplink speed, fiber type, and
-- distance to the upstream device. For more information about networking
-- requirements for racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
--
-- -   @OPTIC_10GBASE_SR@: 10GBASE-SR
--
-- -   @OPTIC_10GBASE_IR@: 10GBASE-IR
--
-- -   @OPTIC_10GBASE_LR@: 10GBASE-LR
--
-- -   @OPTIC_40GBASE_SR@: 40GBASE-SR
--
-- -   @OPTIC_40GBASE_ESR@: 40GBASE-ESR
--
-- -   @OPTIC_40GBASE_IR4_LR4L@: 40GBASE-IR (LR4L)
--
-- -   @OPTIC_40GBASE_LR4@: 40GBASE-LR4
--
-- -   @OPTIC_100GBASE_SR4@: 100GBASE-SR4
--
-- -   @OPTIC_100GBASE_CWDM4@: 100GBASE-CWDM4
--
-- -   @OPTIC_100GBASE_LR4@: 100GBASE-LR4
--
-- -   @OPTIC_100G_PSM4_MSA@: 100G PSM4 MSA
--
-- -   @OPTIC_1000BASE_LX@: 1000Base-LX
--
-- -   @OPTIC_1000BASE_SX@ : 1000Base-SX
--
-- 'powerConnector', 'updateSiteRackPhysicalProperties_powerConnector' - The power connector that Amazon Web Services should plan to provide for
-- connections to the hardware. Note the correlation between @PowerPhase@
-- and @PowerConnector@.
--
-- -   Single-phase AC feed
--
--     -   __L6-30P__ – (common in US); 30A; single phase
--
--     -   __IEC309 (blue)__ – P+N+E, 6hr; 32 A; single phase
--
-- -   Three-phase AC feed
--
--     -   __AH530P7W (red)__ – 3P+N+E, 7hr; 30A; three phase
--
--     -   __AH532P6W (red)__ – 3P+N+E, 6hr; 32A; three phase
--
-- 'powerDrawKva', 'updateSiteRackPhysicalProperties_powerDrawKva' - The power draw, in kVA, available at the hardware placement position for
-- the rack.
--
-- 'powerFeedDrop', 'updateSiteRackPhysicalProperties_powerFeedDrop' - Indicates whether the power feed comes above or below the rack.
--
-- 'powerPhase', 'updateSiteRackPhysicalProperties_powerPhase' - The power option that you can provide for hardware.
--
-- -   Single-phase AC feed: 200 V to 277 V, 50 Hz or 60 Hz
--
-- -   Three-phase AC feed: 346 V to 480 V, 50 Hz or 60 Hz
--
-- 'uplinkCount', 'updateSiteRackPhysicalProperties_uplinkCount' - Racks come with two Outpost network devices. Depending on the supported
-- uplink speed at the site, the Outpost network devices provide a variable
-- number of uplinks. Specify the number of uplinks for each Outpost
-- network device that you intend to use to connect the rack to your
-- network. Note the correlation between @UplinkGbps@ and @UplinkCount@.
--
-- -   1Gbps - Uplinks available: 1, 2, 4, 6, 8
--
-- -   10Gbps - Uplinks available: 1, 2, 4, 8, 12, 16
--
-- -   40 and 100 Gbps- Uplinks available: 1, 2, 4
--
-- 'uplinkGbps', 'updateSiteRackPhysicalProperties_uplinkGbps' - The uplink speed the rack should support for the connection to the
-- Region.
--
-- 'siteId', 'updateSiteRackPhysicalProperties_siteId' - The ID or the Amazon Resource Name (ARN) of the site.
newUpdateSiteRackPhysicalProperties ::
  -- | 'siteId'
  Prelude.Text ->
  UpdateSiteRackPhysicalProperties
newUpdateSiteRackPhysicalProperties pSiteId_ =
  UpdateSiteRackPhysicalProperties'
    { fiberOpticCableType =
        Prelude.Nothing,
      maximumSupportedWeightLbs =
        Prelude.Nothing,
      opticalStandard = Prelude.Nothing,
      powerConnector = Prelude.Nothing,
      powerDrawKva = Prelude.Nothing,
      powerFeedDrop = Prelude.Nothing,
      powerPhase = Prelude.Nothing,
      uplinkCount = Prelude.Nothing,
      uplinkGbps = Prelude.Nothing,
      siteId = pSiteId_
    }

-- | The type of fiber that you will use to attach the Outpost to your
-- network.
updateSiteRackPhysicalProperties_fiberOpticCableType :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe FiberOpticCableType)
updateSiteRackPhysicalProperties_fiberOpticCableType = Lens.lens (\UpdateSiteRackPhysicalProperties' {fiberOpticCableType} -> fiberOpticCableType) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {fiberOpticCableType = a} :: UpdateSiteRackPhysicalProperties)

-- | The maximum rack weight that this site can support. @NO_LIMIT@ is over
-- 2000lbs.
updateSiteRackPhysicalProperties_maximumSupportedWeightLbs :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe MaximumSupportedWeightLbs)
updateSiteRackPhysicalProperties_maximumSupportedWeightLbs = Lens.lens (\UpdateSiteRackPhysicalProperties' {maximumSupportedWeightLbs} -> maximumSupportedWeightLbs) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {maximumSupportedWeightLbs = a} :: UpdateSiteRackPhysicalProperties)

-- | The type of optical standard that you will use to attach the Outpost to
-- your network. This field is dependent on uplink speed, fiber type, and
-- distance to the upstream device. For more information about networking
-- requirements for racks, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#facility-networking Network>
-- in the Amazon Web Services Outposts User Guide.
--
-- -   @OPTIC_10GBASE_SR@: 10GBASE-SR
--
-- -   @OPTIC_10GBASE_IR@: 10GBASE-IR
--
-- -   @OPTIC_10GBASE_LR@: 10GBASE-LR
--
-- -   @OPTIC_40GBASE_SR@: 40GBASE-SR
--
-- -   @OPTIC_40GBASE_ESR@: 40GBASE-ESR
--
-- -   @OPTIC_40GBASE_IR4_LR4L@: 40GBASE-IR (LR4L)
--
-- -   @OPTIC_40GBASE_LR4@: 40GBASE-LR4
--
-- -   @OPTIC_100GBASE_SR4@: 100GBASE-SR4
--
-- -   @OPTIC_100GBASE_CWDM4@: 100GBASE-CWDM4
--
-- -   @OPTIC_100GBASE_LR4@: 100GBASE-LR4
--
-- -   @OPTIC_100G_PSM4_MSA@: 100G PSM4 MSA
--
-- -   @OPTIC_1000BASE_LX@: 1000Base-LX
--
-- -   @OPTIC_1000BASE_SX@ : 1000Base-SX
updateSiteRackPhysicalProperties_opticalStandard :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe OpticalStandard)
updateSiteRackPhysicalProperties_opticalStandard = Lens.lens (\UpdateSiteRackPhysicalProperties' {opticalStandard} -> opticalStandard) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {opticalStandard = a} :: UpdateSiteRackPhysicalProperties)

-- | The power connector that Amazon Web Services should plan to provide for
-- connections to the hardware. Note the correlation between @PowerPhase@
-- and @PowerConnector@.
--
-- -   Single-phase AC feed
--
--     -   __L6-30P__ – (common in US); 30A; single phase
--
--     -   __IEC309 (blue)__ – P+N+E, 6hr; 32 A; single phase
--
-- -   Three-phase AC feed
--
--     -   __AH530P7W (red)__ – 3P+N+E, 7hr; 30A; three phase
--
--     -   __AH532P6W (red)__ – 3P+N+E, 6hr; 32A; three phase
updateSiteRackPhysicalProperties_powerConnector :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe PowerConnector)
updateSiteRackPhysicalProperties_powerConnector = Lens.lens (\UpdateSiteRackPhysicalProperties' {powerConnector} -> powerConnector) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {powerConnector = a} :: UpdateSiteRackPhysicalProperties)

-- | The power draw, in kVA, available at the hardware placement position for
-- the rack.
updateSiteRackPhysicalProperties_powerDrawKva :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe PowerDrawKva)
updateSiteRackPhysicalProperties_powerDrawKva = Lens.lens (\UpdateSiteRackPhysicalProperties' {powerDrawKva} -> powerDrawKva) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {powerDrawKva = a} :: UpdateSiteRackPhysicalProperties)

-- | Indicates whether the power feed comes above or below the rack.
updateSiteRackPhysicalProperties_powerFeedDrop :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe PowerFeedDrop)
updateSiteRackPhysicalProperties_powerFeedDrop = Lens.lens (\UpdateSiteRackPhysicalProperties' {powerFeedDrop} -> powerFeedDrop) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {powerFeedDrop = a} :: UpdateSiteRackPhysicalProperties)

-- | The power option that you can provide for hardware.
--
-- -   Single-phase AC feed: 200 V to 277 V, 50 Hz or 60 Hz
--
-- -   Three-phase AC feed: 346 V to 480 V, 50 Hz or 60 Hz
updateSiteRackPhysicalProperties_powerPhase :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe PowerPhase)
updateSiteRackPhysicalProperties_powerPhase = Lens.lens (\UpdateSiteRackPhysicalProperties' {powerPhase} -> powerPhase) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {powerPhase = a} :: UpdateSiteRackPhysicalProperties)

-- | Racks come with two Outpost network devices. Depending on the supported
-- uplink speed at the site, the Outpost network devices provide a variable
-- number of uplinks. Specify the number of uplinks for each Outpost
-- network device that you intend to use to connect the rack to your
-- network. Note the correlation between @UplinkGbps@ and @UplinkCount@.
--
-- -   1Gbps - Uplinks available: 1, 2, 4, 6, 8
--
-- -   10Gbps - Uplinks available: 1, 2, 4, 8, 12, 16
--
-- -   40 and 100 Gbps- Uplinks available: 1, 2, 4
updateSiteRackPhysicalProperties_uplinkCount :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe UplinkCount)
updateSiteRackPhysicalProperties_uplinkCount = Lens.lens (\UpdateSiteRackPhysicalProperties' {uplinkCount} -> uplinkCount) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {uplinkCount = a} :: UpdateSiteRackPhysicalProperties)

-- | The uplink speed the rack should support for the connection to the
-- Region.
updateSiteRackPhysicalProperties_uplinkGbps :: Lens.Lens' UpdateSiteRackPhysicalProperties (Prelude.Maybe UplinkGbps)
updateSiteRackPhysicalProperties_uplinkGbps = Lens.lens (\UpdateSiteRackPhysicalProperties' {uplinkGbps} -> uplinkGbps) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {uplinkGbps = a} :: UpdateSiteRackPhysicalProperties)

-- | The ID or the Amazon Resource Name (ARN) of the site.
updateSiteRackPhysicalProperties_siteId :: Lens.Lens' UpdateSiteRackPhysicalProperties Prelude.Text
updateSiteRackPhysicalProperties_siteId = Lens.lens (\UpdateSiteRackPhysicalProperties' {siteId} -> siteId) (\s@UpdateSiteRackPhysicalProperties' {} a -> s {siteId = a} :: UpdateSiteRackPhysicalProperties)

instance
  Core.AWSRequest
    UpdateSiteRackPhysicalProperties
  where
  type
    AWSResponse UpdateSiteRackPhysicalProperties =
      UpdateSiteRackPhysicalPropertiesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteRackPhysicalPropertiesResponse'
            Prelude.<$> (x Data..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSiteRackPhysicalProperties
  where
  hashWithSalt
    _salt
    UpdateSiteRackPhysicalProperties' {..} =
      _salt
        `Prelude.hashWithSalt` fiberOpticCableType
        `Prelude.hashWithSalt` maximumSupportedWeightLbs
        `Prelude.hashWithSalt` opticalStandard
        `Prelude.hashWithSalt` powerConnector
        `Prelude.hashWithSalt` powerDrawKva
        `Prelude.hashWithSalt` powerFeedDrop
        `Prelude.hashWithSalt` powerPhase
        `Prelude.hashWithSalt` uplinkCount
        `Prelude.hashWithSalt` uplinkGbps
        `Prelude.hashWithSalt` siteId

instance
  Prelude.NFData
    UpdateSiteRackPhysicalProperties
  where
  rnf UpdateSiteRackPhysicalProperties' {..} =
    Prelude.rnf fiberOpticCableType
      `Prelude.seq` Prelude.rnf maximumSupportedWeightLbs
      `Prelude.seq` Prelude.rnf opticalStandard
      `Prelude.seq` Prelude.rnf powerConnector
      `Prelude.seq` Prelude.rnf powerDrawKva
      `Prelude.seq` Prelude.rnf powerFeedDrop
      `Prelude.seq` Prelude.rnf powerPhase
      `Prelude.seq` Prelude.rnf uplinkCount
      `Prelude.seq` Prelude.rnf uplinkGbps
      `Prelude.seq` Prelude.rnf siteId

instance
  Data.ToHeaders
    UpdateSiteRackPhysicalProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSiteRackPhysicalProperties where
  toJSON UpdateSiteRackPhysicalProperties' {..} =
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

instance Data.ToPath UpdateSiteRackPhysicalProperties where
  toPath UpdateSiteRackPhysicalProperties' {..} =
    Prelude.mconcat
      [ "/sites/",
        Data.toBS siteId,
        "/rackPhysicalProperties"
      ]

instance
  Data.ToQuery
    UpdateSiteRackPhysicalProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSiteRackPhysicalPropertiesResponse' smart constructor.
data UpdateSiteRackPhysicalPropertiesResponse = UpdateSiteRackPhysicalPropertiesResponse'
  { site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteRackPhysicalPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'updateSiteRackPhysicalPropertiesResponse_site' - Undocumented member.
--
-- 'httpStatus', 'updateSiteRackPhysicalPropertiesResponse_httpStatus' - The response's http status code.
newUpdateSiteRackPhysicalPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSiteRackPhysicalPropertiesResponse
newUpdateSiteRackPhysicalPropertiesResponse
  pHttpStatus_ =
    UpdateSiteRackPhysicalPropertiesResponse'
      { site =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateSiteRackPhysicalPropertiesResponse_site :: Lens.Lens' UpdateSiteRackPhysicalPropertiesResponse (Prelude.Maybe Site)
updateSiteRackPhysicalPropertiesResponse_site = Lens.lens (\UpdateSiteRackPhysicalPropertiesResponse' {site} -> site) (\s@UpdateSiteRackPhysicalPropertiesResponse' {} a -> s {site = a} :: UpdateSiteRackPhysicalPropertiesResponse)

-- | The response's http status code.
updateSiteRackPhysicalPropertiesResponse_httpStatus :: Lens.Lens' UpdateSiteRackPhysicalPropertiesResponse Prelude.Int
updateSiteRackPhysicalPropertiesResponse_httpStatus = Lens.lens (\UpdateSiteRackPhysicalPropertiesResponse' {httpStatus} -> httpStatus) (\s@UpdateSiteRackPhysicalPropertiesResponse' {} a -> s {httpStatus = a} :: UpdateSiteRackPhysicalPropertiesResponse)

instance
  Prelude.NFData
    UpdateSiteRackPhysicalPropertiesResponse
  where
  rnf UpdateSiteRackPhysicalPropertiesResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
