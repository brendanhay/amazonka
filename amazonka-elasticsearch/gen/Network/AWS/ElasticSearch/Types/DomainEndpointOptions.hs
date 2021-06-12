{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
import qualified Network.AWS.Lens as Lens

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /See:/ 'newDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | Specify ACM certificate ARN for your custom endpoint.
    customEndpointCertificateArn :: Core.Maybe Core.Text,
    -- | Specify the fully qualified domain for your custom endpoint.
    customEndpoint :: Core.Maybe Core.Text,
    -- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch
    -- domain.
    enforceHTTPS :: Core.Maybe Core.Bool,
    -- | Specify the TLS security policy that needs to be applied to the HTTPS
    -- endpoint of Elasticsearch domain.
    -- It can be one of the following values:
    --
    -- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
    --     TLSv1.0 and higher.
    -- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
    --     only TLSv1.2
    tLSSecurityPolicy :: Core.Maybe TLSSecurityPolicy,
    -- | Specify if custom endpoint should be enabled for the Elasticsearch
    -- domain.
    customEndpointEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEndpointCertificateArn', 'domainEndpointOptions_customEndpointCertificateArn' - Specify ACM certificate ARN for your custom endpoint.
--
-- 'customEndpoint', 'domainEndpointOptions_customEndpoint' - Specify the fully qualified domain for your custom endpoint.
--
-- 'enforceHTTPS', 'domainEndpointOptions_enforceHTTPS' - Specify if only HTTPS endpoint should be enabled for the Elasticsearch
-- domain.
--
-- 'tLSSecurityPolicy', 'domainEndpointOptions_tLSSecurityPolicy' - Specify the TLS security policy that needs to be applied to the HTTPS
-- endpoint of Elasticsearch domain.
-- It can be one of the following values:
--
-- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
--     TLSv1.0 and higher.
-- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
--     only TLSv1.2
--
-- 'customEndpointEnabled', 'domainEndpointOptions_customEndpointEnabled' - Specify if custom endpoint should be enabled for the Elasticsearch
-- domain.
newDomainEndpointOptions ::
  DomainEndpointOptions
newDomainEndpointOptions =
  DomainEndpointOptions'
    { customEndpointCertificateArn =
        Core.Nothing,
      customEndpoint = Core.Nothing,
      enforceHTTPS = Core.Nothing,
      tLSSecurityPolicy = Core.Nothing,
      customEndpointEnabled = Core.Nothing
    }

-- | Specify ACM certificate ARN for your custom endpoint.
domainEndpointOptions_customEndpointCertificateArn :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Text)
domainEndpointOptions_customEndpointCertificateArn = Lens.lens (\DomainEndpointOptions' {customEndpointCertificateArn} -> customEndpointCertificateArn) (\s@DomainEndpointOptions' {} a -> s {customEndpointCertificateArn = a} :: DomainEndpointOptions)

-- | Specify the fully qualified domain for your custom endpoint.
domainEndpointOptions_customEndpoint :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Text)
domainEndpointOptions_customEndpoint = Lens.lens (\DomainEndpointOptions' {customEndpoint} -> customEndpoint) (\s@DomainEndpointOptions' {} a -> s {customEndpoint = a} :: DomainEndpointOptions)

-- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch
-- domain.
domainEndpointOptions_enforceHTTPS :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Bool)
domainEndpointOptions_enforceHTTPS = Lens.lens (\DomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@DomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: DomainEndpointOptions)

-- | Specify the TLS security policy that needs to be applied to the HTTPS
-- endpoint of Elasticsearch domain.
-- It can be one of the following values:
--
-- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
--     TLSv1.0 and higher.
-- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
--     only TLSv1.2
domainEndpointOptions_tLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Core.Maybe TLSSecurityPolicy)
domainEndpointOptions_tLSSecurityPolicy = Lens.lens (\DomainEndpointOptions' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@DomainEndpointOptions' {} a -> s {tLSSecurityPolicy = a} :: DomainEndpointOptions)

-- | Specify if custom endpoint should be enabled for the Elasticsearch
-- domain.
domainEndpointOptions_customEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Bool)
domainEndpointOptions_customEndpointEnabled = Lens.lens (\DomainEndpointOptions' {customEndpointEnabled} -> customEndpointEnabled) (\s@DomainEndpointOptions' {} a -> s {customEndpointEnabled = a} :: DomainEndpointOptions)

instance Core.FromJSON DomainEndpointOptions where
  parseJSON =
    Core.withObject
      "DomainEndpointOptions"
      ( \x ->
          DomainEndpointOptions'
            Core.<$> (x Core..:? "CustomEndpointCertificateArn")
            Core.<*> (x Core..:? "CustomEndpoint")
            Core.<*> (x Core..:? "EnforceHTTPS")
            Core.<*> (x Core..:? "TLSSecurityPolicy")
            Core.<*> (x Core..:? "CustomEndpointEnabled")
      )

instance Core.Hashable DomainEndpointOptions

instance Core.NFData DomainEndpointOptions

instance Core.ToJSON DomainEndpointOptions where
  toJSON DomainEndpointOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomEndpointCertificateArn" Core..=)
              Core.<$> customEndpointCertificateArn,
            ("CustomEndpoint" Core..=) Core.<$> customEndpoint,
            ("EnforceHTTPS" Core..=) Core.<$> enforceHTTPS,
            ("TLSSecurityPolicy" Core..=)
              Core.<$> tLSSecurityPolicy,
            ("CustomEndpointEnabled" Core..=)
              Core.<$> customEndpointEnabled
          ]
      )
