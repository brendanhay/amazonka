{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
  ( ServiceSoftwareOptions (..)
  -- * Smart constructor
  , mkServiceSoftwareOptions
  -- * Lenses
  , ssoAutomatedUpdateDate
  , ssoCancellable
  , ssoCurrentVersion
  , ssoDescription
  , ssoNewVersion
  , ssoOptionalDeployment
  , ssoUpdateAvailable
  , ssoUpdateStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.DeploymentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current options of an Elasticsearch domain service software options.
--
-- /See:/ 'mkServiceSoftwareOptions' smart constructor.
data ServiceSoftwareOptions = ServiceSoftwareOptions'
  { automatedUpdateDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp, in Epoch time, until which you can manually request a service software update. After this date, we automatically update your service software.
  , cancellable :: Core.Maybe Core.Bool
    -- ^ @True@ if you are able to cancel your service software version update. @False@ if you are not able to cancel your service software version. 
  , currentVersion :: Core.Maybe Core.Text
    -- ^ The current service software version that is present on the domain.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the @UpdateStatus@ .
  , newVersion :: Core.Maybe Core.Text
    -- ^ The new service software version if one is available.
  , optionalDeployment :: Core.Maybe Core.Bool
    -- ^ @True@ if a service software is never automatically updated. @False@ if a service software is automatically updated after @AutomatedUpdateDate@ . 
  , updateAvailable :: Core.Maybe Core.Bool
    -- ^ @True@ if you are able to update you service software version. @False@ if you are not able to update your service software version. 
  , updateStatus :: Core.Maybe Types.DeploymentStatus
    -- ^ The status of your service software update. This field can take the following values: @ELIGIBLE@ , @PENDING_UPDATE@ , @IN_PROGRESS@ , @COMPLETED@ , and @NOT_ELIGIBLE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServiceSoftwareOptions' value with any optional fields omitted.
mkServiceSoftwareOptions
    :: ServiceSoftwareOptions
mkServiceSoftwareOptions
  = ServiceSoftwareOptions'{automatedUpdateDate = Core.Nothing,
                            cancellable = Core.Nothing, currentVersion = Core.Nothing,
                            description = Core.Nothing, newVersion = Core.Nothing,
                            optionalDeployment = Core.Nothing, updateAvailable = Core.Nothing,
                            updateStatus = Core.Nothing}

-- | Timestamp, in Epoch time, until which you can manually request a service software update. After this date, we automatically update your service software.
--
-- /Note:/ Consider using 'automatedUpdateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAutomatedUpdateDate :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.NominalDiffTime)
ssoAutomatedUpdateDate = Lens.field @"automatedUpdateDate"
{-# INLINEABLE ssoAutomatedUpdateDate #-}
{-# DEPRECATED automatedUpdateDate "Use generic-lens or generic-optics with 'automatedUpdateDate' instead"  #-}

-- | @True@ if you are able to cancel your service software version update. @False@ if you are not able to cancel your service software version. 
--
-- /Note:/ Consider using 'cancellable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCancellable :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
ssoCancellable = Lens.field @"cancellable"
{-# INLINEABLE ssoCancellable #-}
{-# DEPRECATED cancellable "Use generic-lens or generic-optics with 'cancellable' instead"  #-}

-- | The current service software version that is present on the domain.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCurrentVersion :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
ssoCurrentVersion = Lens.field @"currentVersion"
{-# INLINEABLE ssoCurrentVersion #-}
{-# DEPRECATED currentVersion "Use generic-lens or generic-optics with 'currentVersion' instead"  #-}

-- | The description of the @UpdateStatus@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoDescription :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
ssoDescription = Lens.field @"description"
{-# INLINEABLE ssoDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The new service software version if one is available.
--
-- /Note:/ Consider using 'newVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoNewVersion :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
ssoNewVersion = Lens.field @"newVersion"
{-# INLINEABLE ssoNewVersion #-}
{-# DEPRECATED newVersion "Use generic-lens or generic-optics with 'newVersion' instead"  #-}

-- | @True@ if a service software is never automatically updated. @False@ if a service software is automatically updated after @AutomatedUpdateDate@ . 
--
-- /Note:/ Consider using 'optionalDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOptionalDeployment :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
ssoOptionalDeployment = Lens.field @"optionalDeployment"
{-# INLINEABLE ssoOptionalDeployment #-}
{-# DEPRECATED optionalDeployment "Use generic-lens or generic-optics with 'optionalDeployment' instead"  #-}

-- | @True@ if you are able to update you service software version. @False@ if you are not able to update your service software version. 
--
-- /Note:/ Consider using 'updateAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoUpdateAvailable :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
ssoUpdateAvailable = Lens.field @"updateAvailable"
{-# INLINEABLE ssoUpdateAvailable #-}
{-# DEPRECATED updateAvailable "Use generic-lens or generic-optics with 'updateAvailable' instead"  #-}

-- | The status of your service software update. This field can take the following values: @ELIGIBLE@ , @PENDING_UPDATE@ , @IN_PROGRESS@ , @COMPLETED@ , and @NOT_ELIGIBLE@ .
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoUpdateStatus :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Types.DeploymentStatus)
ssoUpdateStatus = Lens.field @"updateStatus"
{-# INLINEABLE ssoUpdateStatus #-}
{-# DEPRECATED updateStatus "Use generic-lens or generic-optics with 'updateStatus' instead"  #-}

instance Core.FromJSON ServiceSoftwareOptions where
        parseJSON
          = Core.withObject "ServiceSoftwareOptions" Core.$
              \ x ->
                ServiceSoftwareOptions' Core.<$>
                  (x Core..:? "AutomatedUpdateDate") Core.<*>
                    x Core..:? "Cancellable"
                    Core.<*> x Core..:? "CurrentVersion"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "NewVersion"
                    Core.<*> x Core..:? "OptionalDeployment"
                    Core.<*> x Core..:? "UpdateAvailable"
                    Core.<*> x Core..:? "UpdateStatus"
