{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EnabledServicePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.EnabledServicePrincipal
  ( EnabledServicePrincipal (..)
  -- * Smart constructor
  , mkEnabledServicePrincipal
  -- * Lenses
  , espDateEnabled
  , espServicePrincipal
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.ServicePrincipal as Types
import qualified Network.AWS.Prelude as Core

-- | A structure that contains details of a service principal that represents an AWS service that is enabled to integrate with AWS Organizations.
--
-- /See:/ 'mkEnabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { dateEnabled :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the service principal was enabled for integration with AWS Organizations.
  , servicePrincipal :: Core.Maybe Types.ServicePrincipal
    -- ^ The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EnabledServicePrincipal' value with any optional fields omitted.
mkEnabledServicePrincipal
    :: EnabledServicePrincipal
mkEnabledServicePrincipal
  = EnabledServicePrincipal'{dateEnabled = Core.Nothing,
                             servicePrincipal = Core.Nothing}

-- | The date that the service principal was enabled for integration with AWS Organizations.
--
-- /Note:/ Consider using 'dateEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espDateEnabled :: Lens.Lens' EnabledServicePrincipal (Core.Maybe Core.NominalDiffTime)
espDateEnabled = Lens.field @"dateEnabled"
{-# INLINEABLE espDateEnabled #-}
{-# DEPRECATED dateEnabled "Use generic-lens or generic-optics with 'dateEnabled' instead"  #-}

-- | The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espServicePrincipal :: Lens.Lens' EnabledServicePrincipal (Core.Maybe Types.ServicePrincipal)
espServicePrincipal = Lens.field @"servicePrincipal"
{-# INLINEABLE espServicePrincipal #-}
{-# DEPRECATED servicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead"  #-}

instance Core.FromJSON EnabledServicePrincipal where
        parseJSON
          = Core.withObject "EnabledServicePrincipal" Core.$
              \ x ->
                EnabledServicePrincipal' Core.<$>
                  (x Core..:? "DateEnabled") Core.<*> x Core..:? "ServicePrincipal"
