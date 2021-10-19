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
import qualified Network.AWS.Prelude as Prelude

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /See:/ 'newDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch
    -- domain.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool,
    -- | Specify the TLS security policy that needs to be applied to the HTTPS
    -- endpoint of Elasticsearch domain.
    -- It can be one of the following values:
    --
    -- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
    --     TLSv1.0 and higher.
    -- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
    --     only TLSv1.2
    tLSSecurityPolicy :: Prelude.Maybe TLSSecurityPolicy,
    -- | Specify if custom endpoint should be enabled for the Elasticsearch
    -- domain.
    customEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specify the fully qualified domain for your custom endpoint.
    customEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Specify ACM certificate ARN for your custom endpoint.
    customEndpointCertificateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'customEndpoint', 'domainEndpointOptions_customEndpoint' - Specify the fully qualified domain for your custom endpoint.
--
-- 'customEndpointCertificateArn', 'domainEndpointOptions_customEndpointCertificateArn' - Specify ACM certificate ARN for your custom endpoint.
newDomainEndpointOptions ::
  DomainEndpointOptions
newDomainEndpointOptions =
  DomainEndpointOptions'
    { enforceHTTPS =
        Prelude.Nothing,
      tLSSecurityPolicy = Prelude.Nothing,
      customEndpointEnabled = Prelude.Nothing,
      customEndpoint = Prelude.Nothing,
      customEndpointCertificateArn = Prelude.Nothing
    }

-- | Specify if only HTTPS endpoint should be enabled for the Elasticsearch
-- domain.
domainEndpointOptions_enforceHTTPS :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_enforceHTTPS = Lens.lens (\DomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@DomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: DomainEndpointOptions)

-- | Specify the TLS security policy that needs to be applied to the HTTPS
-- endpoint of Elasticsearch domain.
-- It can be one of the following values:
--
-- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
--     TLSv1.0 and higher.
-- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
--     only TLSv1.2
domainEndpointOptions_tLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe TLSSecurityPolicy)
domainEndpointOptions_tLSSecurityPolicy = Lens.lens (\DomainEndpointOptions' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@DomainEndpointOptions' {} a -> s {tLSSecurityPolicy = a} :: DomainEndpointOptions)

-- | Specify if custom endpoint should be enabled for the Elasticsearch
-- domain.
domainEndpointOptions_customEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_customEndpointEnabled = Lens.lens (\DomainEndpointOptions' {customEndpointEnabled} -> customEndpointEnabled) (\s@DomainEndpointOptions' {} a -> s {customEndpointEnabled = a} :: DomainEndpointOptions)

-- | Specify the fully qualified domain for your custom endpoint.
domainEndpointOptions_customEndpoint :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpoint = Lens.lens (\DomainEndpointOptions' {customEndpoint} -> customEndpoint) (\s@DomainEndpointOptions' {} a -> s {customEndpoint = a} :: DomainEndpointOptions)

-- | Specify ACM certificate ARN for your custom endpoint.
domainEndpointOptions_customEndpointCertificateArn :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpointCertificateArn = Lens.lens (\DomainEndpointOptions' {customEndpointCertificateArn} -> customEndpointCertificateArn) (\s@DomainEndpointOptions' {} a -> s {customEndpointCertificateArn = a} :: DomainEndpointOptions)

instance Core.FromJSON DomainEndpointOptions where
  parseJSON =
    Core.withObject
      "DomainEndpointOptions"
      ( \x ->
          DomainEndpointOptions'
            Prelude.<$> (x Core..:? "EnforceHTTPS")
            Prelude.<*> (x Core..:? "TLSSecurityPolicy")
            Prelude.<*> (x Core..:? "CustomEndpointEnabled")
            Prelude.<*> (x Core..:? "CustomEndpoint")
            Prelude.<*> (x Core..:? "CustomEndpointCertificateArn")
      )

instance Prelude.Hashable DomainEndpointOptions

instance Prelude.NFData DomainEndpointOptions

instance Core.ToJSON DomainEndpointOptions where
  toJSON DomainEndpointOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnforceHTTPS" Core..=) Prelude.<$> enforceHTTPS,
            ("TLSSecurityPolicy" Core..=)
              Prelude.<$> tLSSecurityPolicy,
            ("CustomEndpointEnabled" Core..=)
              Prelude.<$> customEndpointEnabled,
            ("CustomEndpoint" Core..=)
              Prelude.<$> customEndpoint,
            ("CustomEndpointCertificateArn" Core..=)
              Prelude.<$> customEndpointCertificateArn
          ]
      )
