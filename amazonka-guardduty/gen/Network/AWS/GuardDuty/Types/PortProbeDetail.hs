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
-- Module      : Network.AWS.GuardDuty.Types.PortProbeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import qualified Network.AWS.Lens as Lens

-- | Contains information about the port probe details.
--
-- /See:/ 'newPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | The local port information of the connection.
    localPortDetails :: Core.Maybe LocalPortDetails,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Core.Maybe RemoteIpDetails,
    -- | The local IP information of the connection.
    localIpDetails :: Core.Maybe LocalIpDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PortProbeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localPortDetails', 'portProbeDetail_localPortDetails' - The local port information of the connection.
--
-- 'remoteIpDetails', 'portProbeDetail_remoteIpDetails' - The remote IP information of the connection.
--
-- 'localIpDetails', 'portProbeDetail_localIpDetails' - The local IP information of the connection.
newPortProbeDetail ::
  PortProbeDetail
newPortProbeDetail =
  PortProbeDetail'
    { localPortDetails = Core.Nothing,
      remoteIpDetails = Core.Nothing,
      localIpDetails = Core.Nothing
    }

-- | The local port information of the connection.
portProbeDetail_localPortDetails :: Lens.Lens' PortProbeDetail (Core.Maybe LocalPortDetails)
portProbeDetail_localPortDetails = Lens.lens (\PortProbeDetail' {localPortDetails} -> localPortDetails) (\s@PortProbeDetail' {} a -> s {localPortDetails = a} :: PortProbeDetail)

-- | The remote IP information of the connection.
portProbeDetail_remoteIpDetails :: Lens.Lens' PortProbeDetail (Core.Maybe RemoteIpDetails)
portProbeDetail_remoteIpDetails = Lens.lens (\PortProbeDetail' {remoteIpDetails} -> remoteIpDetails) (\s@PortProbeDetail' {} a -> s {remoteIpDetails = a} :: PortProbeDetail)

-- | The local IP information of the connection.
portProbeDetail_localIpDetails :: Lens.Lens' PortProbeDetail (Core.Maybe LocalIpDetails)
portProbeDetail_localIpDetails = Lens.lens (\PortProbeDetail' {localIpDetails} -> localIpDetails) (\s@PortProbeDetail' {} a -> s {localIpDetails = a} :: PortProbeDetail)

instance Core.FromJSON PortProbeDetail where
  parseJSON =
    Core.withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            Core.<$> (x Core..:? "localPortDetails")
            Core.<*> (x Core..:? "remoteIpDetails")
            Core.<*> (x Core..:? "localIpDetails")
      )

instance Core.Hashable PortProbeDetail

instance Core.NFData PortProbeDetail
