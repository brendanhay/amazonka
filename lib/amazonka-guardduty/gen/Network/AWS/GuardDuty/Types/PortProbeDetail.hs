{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeDetail
  ( PortProbeDetail (..),

    -- * Smart constructor
    mkPortProbeDetail,

    -- * Lenses
    ppdRemoteIPDetails,
    ppdLocalIPDetails,
    ppdLocalPortDetails,
  )
where

import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the port probe details.
--
-- /See:/ 'mkPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | The remote IP information of the connection.
    remoteIPDetails :: Lude.Maybe RemoteIPDetails,
    -- | The local IP information of the connection.
    localIPDetails :: Lude.Maybe LocalIPDetails,
    -- | The local port information of the connection.
    localPortDetails :: Lude.Maybe LocalPortDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortProbeDetail' with the minimum fields required to make a request.
--
-- * 'remoteIPDetails' - The remote IP information of the connection.
-- * 'localIPDetails' - The local IP information of the connection.
-- * 'localPortDetails' - The local port information of the connection.
mkPortProbeDetail ::
  PortProbeDetail
mkPortProbeDetail =
  PortProbeDetail'
    { remoteIPDetails = Lude.Nothing,
      localIPDetails = Lude.Nothing,
      localPortDetails = Lude.Nothing
    }

-- | The remote IP information of the connection.
--
-- /Note:/ Consider using 'remoteIPDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdRemoteIPDetails :: Lens.Lens' PortProbeDetail (Lude.Maybe RemoteIPDetails)
ppdRemoteIPDetails = Lens.lens (remoteIPDetails :: PortProbeDetail -> Lude.Maybe RemoteIPDetails) (\s a -> s {remoteIPDetails = a} :: PortProbeDetail)
{-# DEPRECATED ppdRemoteIPDetails "Use generic-lens or generic-optics with 'remoteIPDetails' instead." #-}

-- | The local IP information of the connection.
--
-- /Note:/ Consider using 'localIPDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLocalIPDetails :: Lens.Lens' PortProbeDetail (Lude.Maybe LocalIPDetails)
ppdLocalIPDetails = Lens.lens (localIPDetails :: PortProbeDetail -> Lude.Maybe LocalIPDetails) (\s a -> s {localIPDetails = a} :: PortProbeDetail)
{-# DEPRECATED ppdLocalIPDetails "Use generic-lens or generic-optics with 'localIPDetails' instead." #-}

-- | The local port information of the connection.
--
-- /Note:/ Consider using 'localPortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLocalPortDetails :: Lens.Lens' PortProbeDetail (Lude.Maybe LocalPortDetails)
ppdLocalPortDetails = Lens.lens (localPortDetails :: PortProbeDetail -> Lude.Maybe LocalPortDetails) (\s a -> s {localPortDetails = a} :: PortProbeDetail)
{-# DEPRECATED ppdLocalPortDetails "Use generic-lens or generic-optics with 'localPortDetails' instead." #-}

instance Lude.FromJSON PortProbeDetail where
  parseJSON =
    Lude.withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            Lude.<$> (x Lude..:? "remoteIpDetails")
            Lude.<*> (x Lude..:? "localIpDetails")
            Lude.<*> (x Lude..:? "localPortDetails")
      )
