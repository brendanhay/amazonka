{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstanceOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstanceOption
  ( BrokerInstanceOption (..),

    -- * Smart constructor
    mkBrokerInstanceOption,

    -- * Lenses
    bioSupportedEngineVersions,
    bioAvailabilityZones,
    bioSupportedDeploymentModes,
    bioEngineType,
    bioHostInstanceType,
    bioStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerStorageType
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType
import qualified Network.AWS.Prelude as Lude

-- | Option for host instance type.
--
-- /See:/ 'mkBrokerInstanceOption' smart constructor.
data BrokerInstanceOption = BrokerInstanceOption'
  { supportedEngineVersions ::
      Lude.Maybe [Lude.Text],
    availabilityZones ::
      Lude.Maybe [AvailabilityZone],
    supportedDeploymentModes ::
      Lude.Maybe [DeploymentMode],
    engineType :: Lude.Maybe EngineType,
    hostInstanceType :: Lude.Maybe Lude.Text,
    storageType :: Lude.Maybe BrokerStorageType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BrokerInstanceOption' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The list of available az.
-- * 'engineType' - The type of broker engine.
-- * 'hostInstanceType' - The type of broker instance.
-- * 'storageType' - The broker's storage type.
-- * 'supportedDeploymentModes' - The list of supported deployment modes.
-- * 'supportedEngineVersions' - The list of supported engine versions.
mkBrokerInstanceOption ::
  BrokerInstanceOption
mkBrokerInstanceOption =
  BrokerInstanceOption'
    { supportedEngineVersions = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      supportedDeploymentModes = Lude.Nothing,
      engineType = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | The list of supported engine versions.
--
-- /Note:/ Consider using 'supportedEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioSupportedEngineVersions :: Lens.Lens' BrokerInstanceOption (Lude.Maybe [Lude.Text])
bioSupportedEngineVersions = Lens.lens (supportedEngineVersions :: BrokerInstanceOption -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedEngineVersions = a} :: BrokerInstanceOption)
{-# DEPRECATED bioSupportedEngineVersions "Use generic-lens or generic-optics with 'supportedEngineVersions' instead." #-}

-- | The list of available az.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioAvailabilityZones :: Lens.Lens' BrokerInstanceOption (Lude.Maybe [AvailabilityZone])
bioAvailabilityZones = Lens.lens (availabilityZones :: BrokerInstanceOption -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: BrokerInstanceOption)
{-# DEPRECATED bioAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The list of supported deployment modes.
--
-- /Note:/ Consider using 'supportedDeploymentModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioSupportedDeploymentModes :: Lens.Lens' BrokerInstanceOption (Lude.Maybe [DeploymentMode])
bioSupportedDeploymentModes = Lens.lens (supportedDeploymentModes :: BrokerInstanceOption -> Lude.Maybe [DeploymentMode]) (\s a -> s {supportedDeploymentModes = a} :: BrokerInstanceOption)
{-# DEPRECATED bioSupportedDeploymentModes "Use generic-lens or generic-optics with 'supportedDeploymentModes' instead." #-}

-- | The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioEngineType :: Lens.Lens' BrokerInstanceOption (Lude.Maybe EngineType)
bioEngineType = Lens.lens (engineType :: BrokerInstanceOption -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: BrokerInstanceOption)
{-# DEPRECATED bioEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The type of broker instance.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioHostInstanceType :: Lens.Lens' BrokerInstanceOption (Lude.Maybe Lude.Text)
bioHostInstanceType = Lens.lens (hostInstanceType :: BrokerInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: BrokerInstanceOption)
{-# DEPRECATED bioHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioStorageType :: Lens.Lens' BrokerInstanceOption (Lude.Maybe BrokerStorageType)
bioStorageType = Lens.lens (storageType :: BrokerInstanceOption -> Lude.Maybe BrokerStorageType) (\s a -> s {storageType = a} :: BrokerInstanceOption)
{-# DEPRECATED bioStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromJSON BrokerInstanceOption where
  parseJSON =
    Lude.withObject
      "BrokerInstanceOption"
      ( \x ->
          BrokerInstanceOption'
            Lude.<$> (x Lude..:? "supportedEngineVersions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "availabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "supportedDeploymentModes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "engineType")
            Lude.<*> (x Lude..:? "hostInstanceType")
            Lude.<*> (x Lude..:? "storageType")
      )
