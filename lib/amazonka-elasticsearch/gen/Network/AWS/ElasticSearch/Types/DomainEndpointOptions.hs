{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptions
  ( DomainEndpointOptions (..),

    -- * Smart constructor
    mkDomainEndpointOptions,

    -- * Lenses
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,
    deoCustomEndpointEnabled,
    deoCustomEndpoint,
    deoCustomEndpointCertificateARN,
  )
where

import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /See:/ 'mkDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { enforceHTTPS ::
      Lude.Maybe Lude.Bool,
    tlsSecurityPolicy ::
      Lude.Maybe TLSSecurityPolicy,
    customEndpointEnabled :: Lude.Maybe Lude.Bool,
    customEndpoint :: Lude.Maybe Lude.Text,
    customEndpointCertificateARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainEndpointOptions' with the minimum fields required to make a request.
--
-- * 'customEndpoint' - Specify the fully qualified domain for your custom endpoint.
-- * 'customEndpointCertificateARN' - Specify ACM certificate ARN for your custom endpoint.
-- * 'customEndpointEnabled' - Specify if custom endpoint should be enabled for the Elasticsearch domain.
-- * 'enforceHTTPS' - Specify if only HTTPS endpoint should be enabled for the Elasticsearch domain.
-- * 'tlsSecurityPolicy' - Specify the TLS security policy that needs to be applied to the HTTPS endpoint of Elasticsearch domain.
--
-- It can be one of the following values:
--     * __Policy-Min-TLS-1-0-2019-07: __ TLS security policy which supports TLSv1.0 and higher.
--
--     * __Policy-Min-TLS-1-2-2019-07: __ TLS security policy which supports only TLSv1.2
mkDomainEndpointOptions ::
  DomainEndpointOptions
mkDomainEndpointOptions =
  DomainEndpointOptions'
    { enforceHTTPS = Lude.Nothing,
      tlsSecurityPolicy = Lude.Nothing,
      customEndpointEnabled = Lude.Nothing,
      customEndpoint = Lude.Nothing,
      customEndpointCertificateARN = Lude.Nothing
    }

-- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch domain.
--
-- /Note:/ Consider using 'enforceHTTPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoEnforceHTTPS :: Lens.Lens' DomainEndpointOptions (Lude.Maybe Lude.Bool)
deoEnforceHTTPS = Lens.lens (enforceHTTPS :: DomainEndpointOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enforceHTTPS = a} :: DomainEndpointOptions)
{-# DEPRECATED deoEnforceHTTPS "Use generic-lens or generic-optics with 'enforceHTTPS' instead." #-}

-- | Specify the TLS security policy that needs to be applied to the HTTPS endpoint of Elasticsearch domain.
--
-- It can be one of the following values:
--     * __Policy-Min-TLS-1-0-2019-07: __ TLS security policy which supports TLSv1.0 and higher.
--
--     * __Policy-Min-TLS-1-2-2019-07: __ TLS security policy which supports only TLSv1.2
--
--
--
-- /Note:/ Consider using 'tlsSecurityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoTLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Lude.Maybe TLSSecurityPolicy)
deoTLSSecurityPolicy = Lens.lens (tlsSecurityPolicy :: DomainEndpointOptions -> Lude.Maybe TLSSecurityPolicy) (\s a -> s {tlsSecurityPolicy = a} :: DomainEndpointOptions)
{-# DEPRECATED deoTLSSecurityPolicy "Use generic-lens or generic-optics with 'tlsSecurityPolicy' instead." #-}

-- | Specify if custom endpoint should be enabled for the Elasticsearch domain.
--
-- /Note:/ Consider using 'customEndpointEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Lude.Maybe Lude.Bool)
deoCustomEndpointEnabled = Lens.lens (customEndpointEnabled :: DomainEndpointOptions -> Lude.Maybe Lude.Bool) (\s a -> s {customEndpointEnabled = a} :: DomainEndpointOptions)
{-# DEPRECATED deoCustomEndpointEnabled "Use generic-lens or generic-optics with 'customEndpointEnabled' instead." #-}

-- | Specify the fully qualified domain for your custom endpoint.
--
-- /Note:/ Consider using 'customEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpoint :: Lens.Lens' DomainEndpointOptions (Lude.Maybe Lude.Text)
deoCustomEndpoint = Lens.lens (customEndpoint :: DomainEndpointOptions -> Lude.Maybe Lude.Text) (\s a -> s {customEndpoint = a} :: DomainEndpointOptions)
{-# DEPRECATED deoCustomEndpoint "Use generic-lens or generic-optics with 'customEndpoint' instead." #-}

-- | Specify ACM certificate ARN for your custom endpoint.
--
-- /Note:/ Consider using 'customEndpointCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoCustomEndpointCertificateARN :: Lens.Lens' DomainEndpointOptions (Lude.Maybe Lude.Text)
deoCustomEndpointCertificateARN = Lens.lens (customEndpointCertificateARN :: DomainEndpointOptions -> Lude.Maybe Lude.Text) (\s a -> s {customEndpointCertificateARN = a} :: DomainEndpointOptions)
{-# DEPRECATED deoCustomEndpointCertificateARN "Use generic-lens or generic-optics with 'customEndpointCertificateARN' instead." #-}

instance Lude.FromJSON DomainEndpointOptions where
  parseJSON =
    Lude.withObject
      "DomainEndpointOptions"
      ( \x ->
          DomainEndpointOptions'
            Lude.<$> (x Lude..:? "EnforceHTTPS")
            Lude.<*> (x Lude..:? "TLSSecurityPolicy")
            Lude.<*> (x Lude..:? "CustomEndpointEnabled")
            Lude.<*> (x Lude..:? "CustomEndpoint")
            Lude.<*> (x Lude..:? "CustomEndpointCertificateArn")
      )

instance Lude.ToJSON DomainEndpointOptions where
  toJSON DomainEndpointOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EnforceHTTPS" Lude..=) Lude.<$> enforceHTTPS,
            ("TLSSecurityPolicy" Lude..=) Lude.<$> tlsSecurityPolicy,
            ("CustomEndpointEnabled" Lude..=) Lude.<$> customEndpointEnabled,
            ("CustomEndpoint" Lude..=) Lude.<$> customEndpoint,
            ("CustomEndpointCertificateArn" Lude..=)
              Lude.<$> customEndpointCertificateARN
          ]
      )
