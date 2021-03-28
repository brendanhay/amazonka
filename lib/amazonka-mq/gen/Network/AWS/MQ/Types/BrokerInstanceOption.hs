{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstanceOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerInstanceOption
  ( BrokerInstanceOption (..)
  -- * Smart constructor
  , mkBrokerInstanceOption
  -- * Lenses
  , bioAvailabilityZones
  , bioEngineType
  , bioHostInstanceType
  , bioStorageType
  , bioSupportedDeploymentModes
  , bioSupportedEngineVersions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.AvailabilityZone as Types
import qualified Network.AWS.MQ.Types.BrokerStorageType as Types
import qualified Network.AWS.MQ.Types.DeploymentMode as Types
import qualified Network.AWS.MQ.Types.EngineType as Types
import qualified Network.AWS.Prelude as Core

-- | Option for host instance type.
--
-- /See:/ 'mkBrokerInstanceOption' smart constructor.
data BrokerInstanceOption = BrokerInstanceOption'
  { availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ The list of available az.
  , engineType :: Core.Maybe Types.EngineType
    -- ^ The type of broker engine.
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ The type of broker instance.
  , storageType :: Core.Maybe Types.BrokerStorageType
    -- ^ The broker's storage type.
  , supportedDeploymentModes :: Core.Maybe [Types.DeploymentMode]
    -- ^ The list of supported deployment modes.
  , supportedEngineVersions :: Core.Maybe [Core.Text]
    -- ^ The list of supported engine versions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BrokerInstanceOption' value with any optional fields omitted.
mkBrokerInstanceOption
    :: BrokerInstanceOption
mkBrokerInstanceOption
  = BrokerInstanceOption'{availabilityZones = Core.Nothing,
                          engineType = Core.Nothing, hostInstanceType = Core.Nothing,
                          storageType = Core.Nothing,
                          supportedDeploymentModes = Core.Nothing,
                          supportedEngineVersions = Core.Nothing}

-- | The list of available az.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioAvailabilityZones :: Lens.Lens' BrokerInstanceOption (Core.Maybe [Types.AvailabilityZone])
bioAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE bioAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioEngineType :: Lens.Lens' BrokerInstanceOption (Core.Maybe Types.EngineType)
bioEngineType = Lens.field @"engineType"
{-# INLINEABLE bioEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | The type of broker instance.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioHostInstanceType :: Lens.Lens' BrokerInstanceOption (Core.Maybe Core.Text)
bioHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE bioHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioStorageType :: Lens.Lens' BrokerInstanceOption (Core.Maybe Types.BrokerStorageType)
bioStorageType = Lens.field @"storageType"
{-# INLINEABLE bioStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | The list of supported deployment modes.
--
-- /Note:/ Consider using 'supportedDeploymentModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioSupportedDeploymentModes :: Lens.Lens' BrokerInstanceOption (Core.Maybe [Types.DeploymentMode])
bioSupportedDeploymentModes = Lens.field @"supportedDeploymentModes"
{-# INLINEABLE bioSupportedDeploymentModes #-}
{-# DEPRECATED supportedDeploymentModes "Use generic-lens or generic-optics with 'supportedDeploymentModes' instead"  #-}

-- | The list of supported engine versions.
--
-- /Note:/ Consider using 'supportedEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bioSupportedEngineVersions :: Lens.Lens' BrokerInstanceOption (Core.Maybe [Core.Text])
bioSupportedEngineVersions = Lens.field @"supportedEngineVersions"
{-# INLINEABLE bioSupportedEngineVersions #-}
{-# DEPRECATED supportedEngineVersions "Use generic-lens or generic-optics with 'supportedEngineVersions' instead"  #-}

instance Core.FromJSON BrokerInstanceOption where
        parseJSON
          = Core.withObject "BrokerInstanceOption" Core.$
              \ x ->
                BrokerInstanceOption' Core.<$>
                  (x Core..:? "availabilityZones") Core.<*> x Core..:? "engineType"
                    Core.<*> x Core..:? "hostInstanceType"
                    Core.<*> x Core..:? "storageType"
                    Core.<*> x Core..:? "supportedDeploymentModes"
                    Core.<*> x Core..:? "supportedEngineVersions"
