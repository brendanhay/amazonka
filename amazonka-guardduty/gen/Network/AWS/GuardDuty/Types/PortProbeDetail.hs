{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the port probe details.
--
-- /See:/ 'newPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | The local port information of the connection.
    localPortDetails :: Prelude.Maybe LocalPortDetails,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The local IP information of the connection.
    localIpDetails :: Prelude.Maybe LocalIpDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { localPortDetails =
        Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      localIpDetails = Prelude.Nothing
    }

-- | The local port information of the connection.
portProbeDetail_localPortDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe LocalPortDetails)
portProbeDetail_localPortDetails = Lens.lens (\PortProbeDetail' {localPortDetails} -> localPortDetails) (\s@PortProbeDetail' {} a -> s {localPortDetails = a} :: PortProbeDetail)

-- | The remote IP information of the connection.
portProbeDetail_remoteIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe RemoteIpDetails)
portProbeDetail_remoteIpDetails = Lens.lens (\PortProbeDetail' {remoteIpDetails} -> remoteIpDetails) (\s@PortProbeDetail' {} a -> s {remoteIpDetails = a} :: PortProbeDetail)

-- | The local IP information of the connection.
portProbeDetail_localIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe LocalIpDetails)
portProbeDetail_localIpDetails = Lens.lens (\PortProbeDetail' {localIpDetails} -> localIpDetails) (\s@PortProbeDetail' {} a -> s {localIpDetails = a} :: PortProbeDetail)

instance Prelude.FromJSON PortProbeDetail where
  parseJSON =
    Prelude.withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            Prelude.<$> (x Prelude..:? "localPortDetails")
            Prelude.<*> (x Prelude..:? "remoteIpDetails")
            Prelude.<*> (x Prelude..:? "localIpDetails")
      )

instance Prelude.Hashable PortProbeDetail

instance Prelude.NFData PortProbeDetail
