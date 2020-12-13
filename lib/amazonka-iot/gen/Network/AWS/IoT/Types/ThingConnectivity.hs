{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingConnectivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingConnectivity
  ( ThingConnectivity (..),

    -- * Smart constructor
    mkThingConnectivity,

    -- * Lenses
    tcConnected,
    tcTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The connectivity status of the thing.
--
-- /See:/ 'mkThingConnectivity' smart constructor.
data ThingConnectivity = ThingConnectivity'
  { -- | True if the thing is connected to the AWS IoT service; false if it is not connected.
    connected :: Lude.Maybe Lude.Bool,
    -- | The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
    timestamp :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingConnectivity' with the minimum fields required to make a request.
--
-- * 'connected' - True if the thing is connected to the AWS IoT service; false if it is not connected.
-- * 'timestamp' - The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
mkThingConnectivity ::
  ThingConnectivity
mkThingConnectivity =
  ThingConnectivity'
    { connected = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | True if the thing is connected to the AWS IoT service; false if it is not connected.
--
-- /Note:/ Consider using 'connected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcConnected :: Lens.Lens' ThingConnectivity (Lude.Maybe Lude.Bool)
tcConnected = Lens.lens (connected :: ThingConnectivity -> Lude.Maybe Lude.Bool) (\s a -> s {connected = a} :: ThingConnectivity)
{-# DEPRECATED tcConnected "Use generic-lens or generic-optics with 'connected' instead." #-}

-- | The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTimestamp :: Lens.Lens' ThingConnectivity (Lude.Maybe Lude.Integer)
tcTimestamp = Lens.lens (timestamp :: ThingConnectivity -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: ThingConnectivity)
{-# DEPRECATED tcTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON ThingConnectivity where
  parseJSON =
    Lude.withObject
      "ThingConnectivity"
      ( \x ->
          ThingConnectivity'
            Lude.<$> (x Lude..:? "connected") Lude.<*> (x Lude..:? "timestamp")
      )
