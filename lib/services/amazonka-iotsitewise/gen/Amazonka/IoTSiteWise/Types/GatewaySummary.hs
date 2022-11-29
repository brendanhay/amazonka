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
-- Module      : Amazonka.IoTSiteWise.Types.GatewaySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.GatewaySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.GatewayCapabilitySummary
import Amazonka.IoTSiteWise.Types.GatewayPlatform
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a gateway.
--
-- /See:/ 'newGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { gatewayPlatform :: Prelude.Maybe GatewayPlatform,
    -- | A list of gateway capability summaries that each contain a namespace and
    -- status. Each gateway capability defines data sources for the gateway. To
    -- retrieve a capability configuration\'s definition, use
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
    gatewayCapabilitySummaries :: Prelude.Maybe [GatewayCapabilitySummary],
    -- | The ID of the gateway device.
    gatewayId :: Prelude.Text,
    -- | The name of the asset.
    gatewayName :: Prelude.Text,
    -- | The date the gateway was created, in Unix epoch time.
    creationDate :: Core.POSIX,
    -- | The date the gateway was last updated, in Unix epoch time.
    lastUpdateDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewaySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayPlatform', 'gatewaySummary_gatewayPlatform' - Undocumented member.
--
-- 'gatewayCapabilitySummaries', 'gatewaySummary_gatewayCapabilitySummaries' - A list of gateway capability summaries that each contain a namespace and
-- status. Each gateway capability defines data sources for the gateway. To
-- retrieve a capability configuration\'s definition, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
--
-- 'gatewayId', 'gatewaySummary_gatewayId' - The ID of the gateway device.
--
-- 'gatewayName', 'gatewaySummary_gatewayName' - The name of the asset.
--
-- 'creationDate', 'gatewaySummary_creationDate' - The date the gateway was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'gatewaySummary_lastUpdateDate' - The date the gateway was last updated, in Unix epoch time.
newGatewaySummary ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'gatewayName'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'lastUpdateDate'
  Prelude.UTCTime ->
  GatewaySummary
newGatewaySummary
  pGatewayId_
  pGatewayName_
  pCreationDate_
  pLastUpdateDate_ =
    GatewaySummary'
      { gatewayPlatform = Prelude.Nothing,
        gatewayCapabilitySummaries = Prelude.Nothing,
        gatewayId = pGatewayId_,
        gatewayName = pGatewayName_,
        creationDate = Core._Time Lens.# pCreationDate_,
        lastUpdateDate = Core._Time Lens.# pLastUpdateDate_
      }

-- | Undocumented member.
gatewaySummary_gatewayPlatform :: Lens.Lens' GatewaySummary (Prelude.Maybe GatewayPlatform)
gatewaySummary_gatewayPlatform = Lens.lens (\GatewaySummary' {gatewayPlatform} -> gatewayPlatform) (\s@GatewaySummary' {} a -> s {gatewayPlatform = a} :: GatewaySummary)

-- | A list of gateway capability summaries that each contain a namespace and
-- status. Each gateway capability defines data sources for the gateway. To
-- retrieve a capability configuration\'s definition, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
gatewaySummary_gatewayCapabilitySummaries :: Lens.Lens' GatewaySummary (Prelude.Maybe [GatewayCapabilitySummary])
gatewaySummary_gatewayCapabilitySummaries = Lens.lens (\GatewaySummary' {gatewayCapabilitySummaries} -> gatewayCapabilitySummaries) (\s@GatewaySummary' {} a -> s {gatewayCapabilitySummaries = a} :: GatewaySummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the gateway device.
gatewaySummary_gatewayId :: Lens.Lens' GatewaySummary Prelude.Text
gatewaySummary_gatewayId = Lens.lens (\GatewaySummary' {gatewayId} -> gatewayId) (\s@GatewaySummary' {} a -> s {gatewayId = a} :: GatewaySummary)

-- | The name of the asset.
gatewaySummary_gatewayName :: Lens.Lens' GatewaySummary Prelude.Text
gatewaySummary_gatewayName = Lens.lens (\GatewaySummary' {gatewayName} -> gatewayName) (\s@GatewaySummary' {} a -> s {gatewayName = a} :: GatewaySummary)

-- | The date the gateway was created, in Unix epoch time.
gatewaySummary_creationDate :: Lens.Lens' GatewaySummary Prelude.UTCTime
gatewaySummary_creationDate = Lens.lens (\GatewaySummary' {creationDate} -> creationDate) (\s@GatewaySummary' {} a -> s {creationDate = a} :: GatewaySummary) Prelude.. Core._Time

-- | The date the gateway was last updated, in Unix epoch time.
gatewaySummary_lastUpdateDate :: Lens.Lens' GatewaySummary Prelude.UTCTime
gatewaySummary_lastUpdateDate = Lens.lens (\GatewaySummary' {lastUpdateDate} -> lastUpdateDate) (\s@GatewaySummary' {} a -> s {lastUpdateDate = a} :: GatewaySummary) Prelude.. Core._Time

instance Core.FromJSON GatewaySummary where
  parseJSON =
    Core.withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            Prelude.<$> (x Core..:? "gatewayPlatform")
            Prelude.<*> ( x Core..:? "gatewayCapabilitySummaries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "gatewayId")
            Prelude.<*> (x Core..: "gatewayName")
            Prelude.<*> (x Core..: "creationDate")
            Prelude.<*> (x Core..: "lastUpdateDate")
      )

instance Prelude.Hashable GatewaySummary where
  hashWithSalt _salt GatewaySummary' {..} =
    _salt `Prelude.hashWithSalt` gatewayPlatform
      `Prelude.hashWithSalt` gatewayCapabilitySummaries
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` gatewayName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate

instance Prelude.NFData GatewaySummary where
  rnf GatewaySummary' {..} =
    Prelude.rnf gatewayPlatform
      `Prelude.seq` Prelude.rnf gatewayCapabilitySummaries
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf gatewayName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
