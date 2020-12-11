-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceAccess
  ( InstanceAccess (..),

    -- * Smart constructor
    mkInstanceAccess,

    -- * Lenses
    iaInstanceId,
    iaIPAddress,
    iaOperatingSystem,
    iaCredentials,
    iaFleetId,
  )
where

import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information required to remotely connect to a fleet instance. Access is requested by calling 'GetInstanceAccess' .
--
-- /See:/ 'mkInstanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
  { instanceId ::
      Lude.Maybe Lude.Text,
    ipAddress :: Lude.Maybe Lude.Text,
    operatingSystem :: Lude.Maybe OperatingSystem,
    credentials :: Lude.Maybe InstanceCredentials,
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAccess' with the minimum fields required to make a request.
--
-- * 'credentials' - Credentials required to access the instance.
-- * 'fleetId' - A unique identifier for a fleet containing the instance being accessed.
-- * 'instanceId' - A unique identifier for an instance being accessed.
-- * 'ipAddress' - IP address that is assigned to the instance.
-- * 'operatingSystem' - Operating system that is running on the instance.
mkInstanceAccess ::
  InstanceAccess
mkInstanceAccess =
  InstanceAccess'
    { instanceId = Lude.Nothing,
      ipAddress = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      credentials = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | A unique identifier for an instance being accessed.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInstanceId :: Lens.Lens' InstanceAccess (Lude.Maybe Lude.Text)
iaInstanceId = Lens.lens (instanceId :: InstanceAccess -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceAccess)
{-# DEPRECATED iaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | IP address that is assigned to the instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaIPAddress :: Lens.Lens' InstanceAccess (Lude.Maybe Lude.Text)
iaIPAddress = Lens.lens (ipAddress :: InstanceAccess -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: InstanceAccess)
{-# DEPRECATED iaIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Operating system that is running on the instance.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaOperatingSystem :: Lens.Lens' InstanceAccess (Lude.Maybe OperatingSystem)
iaOperatingSystem = Lens.lens (operatingSystem :: InstanceAccess -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: InstanceAccess)
{-# DEPRECATED iaOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | Credentials required to access the instance.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaCredentials :: Lens.Lens' InstanceAccess (Lude.Maybe InstanceCredentials)
iaCredentials = Lens.lens (credentials :: InstanceAccess -> Lude.Maybe InstanceCredentials) (\s a -> s {credentials = a} :: InstanceAccess)
{-# DEPRECATED iaCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | A unique identifier for a fleet containing the instance being accessed.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaFleetId :: Lens.Lens' InstanceAccess (Lude.Maybe Lude.Text)
iaFleetId = Lens.lens (fleetId :: InstanceAccess -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: InstanceAccess)
{-# DEPRECATED iaFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.FromJSON InstanceAccess where
  parseJSON =
    Lude.withObject
      "InstanceAccess"
      ( \x ->
          InstanceAccess'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "Credentials")
            Lude.<*> (x Lude..:? "FleetId")
      )
