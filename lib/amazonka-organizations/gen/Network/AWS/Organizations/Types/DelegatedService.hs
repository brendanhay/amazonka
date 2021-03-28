{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.DelegatedService
  ( DelegatedService (..)
  -- * Smart constructor
  , mkDelegatedService
  -- * Lenses
  , dsDelegationEnabledDate
  , dsServicePrincipal
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.ServicePrincipal as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about the AWS service for which the account is a delegated administrator.
--
-- /See:/ 'mkDelegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { delegationEnabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the account became a delegated administrator for this service. 
  , servicePrincipal :: Core.Maybe Types.ServicePrincipal
    -- ^ The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DelegatedService' value with any optional fields omitted.
mkDelegatedService
    :: DelegatedService
mkDelegatedService
  = DelegatedService'{delegationEnabledDate = Core.Nothing,
                      servicePrincipal = Core.Nothing}

-- | The date that the account became a delegated administrator for this service. 
--
-- /Note:/ Consider using 'delegationEnabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDelegationEnabledDate :: Lens.Lens' DelegatedService (Core.Maybe Core.NominalDiffTime)
dsDelegationEnabledDate = Lens.field @"delegationEnabledDate"
{-# INLINEABLE dsDelegationEnabledDate #-}
{-# DEPRECATED delegationEnabledDate "Use generic-lens or generic-optics with 'delegationEnabledDate' instead"  #-}

-- | The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServicePrincipal :: Lens.Lens' DelegatedService (Core.Maybe Types.ServicePrincipal)
dsServicePrincipal = Lens.field @"servicePrincipal"
{-# INLINEABLE dsServicePrincipal #-}
{-# DEPRECATED servicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead"  #-}

instance Core.FromJSON DelegatedService where
        parseJSON
          = Core.withObject "DelegatedService" Core.$
              \ x ->
                DelegatedService' Core.<$>
                  (x Core..:? "DelegationEnabledDate") Core.<*>
                    x Core..:? "ServicePrincipal"
