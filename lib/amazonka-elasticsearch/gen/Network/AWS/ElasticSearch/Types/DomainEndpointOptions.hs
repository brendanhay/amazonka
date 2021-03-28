{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DomainEndpointOptions
  ( DomainEndpointOptions (..)
  -- * Smart constructor
  , mkDomainEndpointOptions
  -- * Lenses
  , deoCustomEndpoint
  , deoCustomEndpointCertificateArn
  , deoCustomEndpointEnabled
  , deoEnforceHTTPS
  , deoTLSSecurityPolicy
  ) where

import qualified Network.AWS.ElasticSearch.Types.ARN as Types
import qualified Network.AWS.ElasticSearch.Types.DomainNameFqdn as Types
import qualified Network.AWS.ElasticSearch.Types.TLSSecurityPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /See:/ 'mkDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { customEndpoint :: Core.Maybe Types.DomainNameFqdn
    -- ^ Specify the fully qualified domain for your custom endpoint.
  , customEndpointCertificateArn :: Core.Maybe Types.ARN
    -- ^ Specify ACM certificate ARN for your custom endpoint.
  , customEndpointEnabled :: Core.Maybe Core.Bool
    -- ^ Specify if custom endpoint should be enabled for the Elasticsearch domain.
  , enforceHTTPS :: Core.Maybe Core.Bool
    -- ^ Specify if only HTTPS endpoint should be enabled for the Elasticsearch domain.
  , tLSSecurityPolicy :: Core.Maybe Types.TLSSecurityPolicy
    -- ^ Specify the TLS security policy that needs to be applied to the HTTPS endpoint of Elasticsearch domain. 
--
-- It can be one of the following values: 
--     * __Policy-Min-TLS-1-0-2019-07: __ TLS security policy which supports TLSv1.0 and higher.
--
--     * __Policy-Min-TLS-1-2-2019-07: __ TLS security policy which supports only TLSv1.2
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainEndpointOptions' value with any optional fields omitted.
mkDomainEndpointOptions
    :: DomainEndpointOptions
mkDomainEndpointOptions
  = DomainEndpointOptions'{customEndpoint = Core.Nothing,
                           customEndpointCertificateArn = Core.Nothing,
                           customEndpointEnabled = Core.Nothing, enforceHTTPS = Core.Nothing,
                           tLSSecurityPolicy = Core.Nothing}

-- | Specify the fully qualified domain for your custom endpoint.
--
-- /Note:/ Consider using 'customEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpoint :: Lens.Lens' DomainEndpointOptions (Core.Maybe Types.DomainNameFqdn)
deoCustomEndpoint = Lens.field @"customEndpoint"
{-# INLINEABLE deoCustomEndpoint #-}
{-# DEPRECATED customEndpoint "Use generic-lens or generic-optics with 'customEndpoint' instead"  #-}

-- | Specify ACM certificate ARN for your custom endpoint.
--
-- /Note:/ Consider using 'customEndpointCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpointCertificateArn :: Lens.Lens' DomainEndpointOptions (Core.Maybe Types.ARN)
deoCustomEndpointCertificateArn = Lens.field @"customEndpointCertificateArn"
{-# INLINEABLE deoCustomEndpointCertificateArn #-}
{-# DEPRECATED customEndpointCertificateArn "Use generic-lens or generic-optics with 'customEndpointCertificateArn' instead"  #-}

-- | Specify if custom endpoint should be enabled for the Elasticsearch domain.
--
-- /Note:/ Consider using 'customEndpointEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Bool)
deoCustomEndpointEnabled = Lens.field @"customEndpointEnabled"
{-# INLINEABLE deoCustomEndpointEnabled #-}
{-# DEPRECATED customEndpointEnabled "Use generic-lens or generic-optics with 'customEndpointEnabled' instead"  #-}

-- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch domain.
--
-- /Note:/ Consider using 'enforceHTTPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoEnforceHTTPS :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Bool)
deoEnforceHTTPS = Lens.field @"enforceHTTPS"
{-# INLINEABLE deoEnforceHTTPS #-}
{-# DEPRECATED enforceHTTPS "Use generic-lens or generic-optics with 'enforceHTTPS' instead"  #-}

-- | Specify the TLS security policy that needs to be applied to the HTTPS endpoint of Elasticsearch domain. 
--
-- It can be one of the following values: 
--     * __Policy-Min-TLS-1-0-2019-07: __ TLS security policy which supports TLSv1.0 and higher.
--
--     * __Policy-Min-TLS-1-2-2019-07: __ TLS security policy which supports only TLSv1.2
--
--
--
-- /Note:/ Consider using 'tLSSecurityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoTLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Core.Maybe Types.TLSSecurityPolicy)
deoTLSSecurityPolicy = Lens.field @"tLSSecurityPolicy"
{-# INLINEABLE deoTLSSecurityPolicy #-}
{-# DEPRECATED tLSSecurityPolicy "Use generic-lens or generic-optics with 'tLSSecurityPolicy' instead"  #-}

instance Core.FromJSON DomainEndpointOptions where
        toJSON DomainEndpointOptions{..}
          = Core.object
              (Core.catMaybes
                 [("CustomEndpoint" Core..=) Core.<$> customEndpoint,
                  ("CustomEndpointCertificateArn" Core..=) Core.<$>
                    customEndpointCertificateArn,
                  ("CustomEndpointEnabled" Core..=) Core.<$> customEndpointEnabled,
                  ("EnforceHTTPS" Core..=) Core.<$> enforceHTTPS,
                  ("TLSSecurityPolicy" Core..=) Core.<$> tLSSecurityPolicy])

instance Core.FromJSON DomainEndpointOptions where
        parseJSON
          = Core.withObject "DomainEndpointOptions" Core.$
              \ x ->
                DomainEndpointOptions' Core.<$>
                  (x Core..:? "CustomEndpoint") Core.<*>
                    x Core..:? "CustomEndpointCertificateArn"
                    Core.<*> x Core..:? "CustomEndpointEnabled"
                    Core.<*> x Core..:? "EnforceHTTPS"
                    Core.<*> x Core..:? "TLSSecurityPolicy"
