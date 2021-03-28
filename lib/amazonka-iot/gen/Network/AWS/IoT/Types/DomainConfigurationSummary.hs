{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DomainConfigurationSummary
  ( DomainConfigurationSummary (..)
  -- * Smart constructor
  , mkDomainConfigurationSummary
  -- * Lenses
  , dcsDomainConfigurationArn
  , dcsDomainConfigurationName
  , dcsServiceType
  ) where

import qualified Network.AWS.IoT.Types.DomainConfigurationArn as Types
import qualified Network.AWS.IoT.Types.DomainConfigurationName as Types
import qualified Network.AWS.IoT.Types.ServiceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of a domain configuration. A domain configuration specifies custom IoT-specific information about a domain. A domain configuration can be associated with an AWS-managed domain (for example, dbc123defghijk.iot.us-west-2.amazonaws.com), a customer managed domain, or a default endpoint.
--
--
--     * Data
--
--
--     * Jobs
--
--
--     * CredentialProvider
--
--
--
-- /See:/ 'mkDomainConfigurationSummary' smart constructor.
data DomainConfigurationSummary = DomainConfigurationSummary'
  { domainConfigurationArn :: Core.Maybe Types.DomainConfigurationArn
    -- ^ The ARN of the domain configuration.
  , domainConfigurationName :: Core.Maybe Types.DomainConfigurationName
    -- ^ The name of the domain configuration. This value must be unique to a region.
  , serviceType :: Core.Maybe Types.ServiceType
    -- ^ The type of service delivered by the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainConfigurationSummary' value with any optional fields omitted.
mkDomainConfigurationSummary
    :: DomainConfigurationSummary
mkDomainConfigurationSummary
  = DomainConfigurationSummary'{domainConfigurationArn =
                                  Core.Nothing,
                                domainConfigurationName = Core.Nothing, serviceType = Core.Nothing}

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDomainConfigurationArn :: Lens.Lens' DomainConfigurationSummary (Core.Maybe Types.DomainConfigurationArn)
dcsDomainConfigurationArn = Lens.field @"domainConfigurationArn"
{-# INLINEABLE dcsDomainConfigurationArn #-}
{-# DEPRECATED domainConfigurationArn "Use generic-lens or generic-optics with 'domainConfigurationArn' instead"  #-}

-- | The name of the domain configuration. This value must be unique to a region.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDomainConfigurationName :: Lens.Lens' DomainConfigurationSummary (Core.Maybe Types.DomainConfigurationName)
dcsDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# INLINEABLE dcsDomainConfigurationName #-}
{-# DEPRECATED domainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead"  #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsServiceType :: Lens.Lens' DomainConfigurationSummary (Core.Maybe Types.ServiceType)
dcsServiceType = Lens.field @"serviceType"
{-# INLINEABLE dcsServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

instance Core.FromJSON DomainConfigurationSummary where
        parseJSON
          = Core.withObject "DomainConfigurationSummary" Core.$
              \ x ->
                DomainConfigurationSummary' Core.<$>
                  (x Core..:? "domainConfigurationArn") Core.<*>
                    x Core..:? "domainConfigurationName"
                    Core.<*> x Core..:? "serviceType"
