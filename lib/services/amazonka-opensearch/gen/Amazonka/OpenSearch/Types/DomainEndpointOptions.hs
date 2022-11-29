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
-- Module      : Amazonka.OpenSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainEndpointOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.TLSSecurityPolicy
import qualified Amazonka.Prelude as Prelude

-- | Options to configure a custom endpoint for an OpenSearch Service domain.
--
-- /See:/ 'newDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | The ARN for your security certificate, managed in Amazon Web Services
    -- Certificate Manager (ACM).
    customEndpointCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | Specify the TLS security policy to apply to the HTTPS endpoint of the
    -- domain.
    --
    -- Can be one of the following values:
    --
    -- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
    --     TLS version 1.0 and higher.
    --
    -- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
    --     only TLS version 1.2
    tLSSecurityPolicy :: Prelude.Maybe TLSSecurityPolicy,
    -- | Whether to enable a custom endpoint for the domain.
    customEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | True to require that all traffic to the domain arrive over HTTPS.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool,
    -- | The fully qualified URL for the custom endpoint.
    customEndpoint :: Prelude.Maybe Prelude.Text
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
-- 'customEndpointCertificateArn', 'domainEndpointOptions_customEndpointCertificateArn' - The ARN for your security certificate, managed in Amazon Web Services
-- Certificate Manager (ACM).
--
-- 'tLSSecurityPolicy', 'domainEndpointOptions_tLSSecurityPolicy' - Specify the TLS security policy to apply to the HTTPS endpoint of the
-- domain.
--
-- Can be one of the following values:
--
-- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
--     TLS version 1.0 and higher.
--
-- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
--     only TLS version 1.2
--
-- 'customEndpointEnabled', 'domainEndpointOptions_customEndpointEnabled' - Whether to enable a custom endpoint for the domain.
--
-- 'enforceHTTPS', 'domainEndpointOptions_enforceHTTPS' - True to require that all traffic to the domain arrive over HTTPS.
--
-- 'customEndpoint', 'domainEndpointOptions_customEndpoint' - The fully qualified URL for the custom endpoint.
newDomainEndpointOptions ::
  DomainEndpointOptions
newDomainEndpointOptions =
  DomainEndpointOptions'
    { customEndpointCertificateArn =
        Prelude.Nothing,
      tLSSecurityPolicy = Prelude.Nothing,
      customEndpointEnabled = Prelude.Nothing,
      enforceHTTPS = Prelude.Nothing,
      customEndpoint = Prelude.Nothing
    }

-- | The ARN for your security certificate, managed in Amazon Web Services
-- Certificate Manager (ACM).
domainEndpointOptions_customEndpointCertificateArn :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpointCertificateArn = Lens.lens (\DomainEndpointOptions' {customEndpointCertificateArn} -> customEndpointCertificateArn) (\s@DomainEndpointOptions' {} a -> s {customEndpointCertificateArn = a} :: DomainEndpointOptions)

-- | Specify the TLS security policy to apply to the HTTPS endpoint of the
-- domain.
--
-- Can be one of the following values:
--
-- -   __Policy-Min-TLS-1-0-2019-07:__ TLS security policy which supports
--     TLS version 1.0 and higher.
--
-- -   __Policy-Min-TLS-1-2-2019-07:__ TLS security policy which supports
--     only TLS version 1.2
domainEndpointOptions_tLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe TLSSecurityPolicy)
domainEndpointOptions_tLSSecurityPolicy = Lens.lens (\DomainEndpointOptions' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@DomainEndpointOptions' {} a -> s {tLSSecurityPolicy = a} :: DomainEndpointOptions)

-- | Whether to enable a custom endpoint for the domain.
domainEndpointOptions_customEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_customEndpointEnabled = Lens.lens (\DomainEndpointOptions' {customEndpointEnabled} -> customEndpointEnabled) (\s@DomainEndpointOptions' {} a -> s {customEndpointEnabled = a} :: DomainEndpointOptions)

-- | True to require that all traffic to the domain arrive over HTTPS.
domainEndpointOptions_enforceHTTPS :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_enforceHTTPS = Lens.lens (\DomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@DomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: DomainEndpointOptions)

-- | The fully qualified URL for the custom endpoint.
domainEndpointOptions_customEndpoint :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpoint = Lens.lens (\DomainEndpointOptions' {customEndpoint} -> customEndpoint) (\s@DomainEndpointOptions' {} a -> s {customEndpoint = a} :: DomainEndpointOptions)

instance Core.FromJSON DomainEndpointOptions where
  parseJSON =
    Core.withObject
      "DomainEndpointOptions"
      ( \x ->
          DomainEndpointOptions'
            Prelude.<$> (x Core..:? "CustomEndpointCertificateArn")
            Prelude.<*> (x Core..:? "TLSSecurityPolicy")
            Prelude.<*> (x Core..:? "CustomEndpointEnabled")
            Prelude.<*> (x Core..:? "EnforceHTTPS")
            Prelude.<*> (x Core..:? "CustomEndpoint")
      )

instance Prelude.Hashable DomainEndpointOptions where
  hashWithSalt _salt DomainEndpointOptions' {..} =
    _salt
      `Prelude.hashWithSalt` customEndpointCertificateArn
      `Prelude.hashWithSalt` tLSSecurityPolicy
      `Prelude.hashWithSalt` customEndpointEnabled
      `Prelude.hashWithSalt` enforceHTTPS
      `Prelude.hashWithSalt` customEndpoint

instance Prelude.NFData DomainEndpointOptions where
  rnf DomainEndpointOptions' {..} =
    Prelude.rnf customEndpointCertificateArn
      `Prelude.seq` Prelude.rnf tLSSecurityPolicy
      `Prelude.seq` Prelude.rnf customEndpointEnabled
      `Prelude.seq` Prelude.rnf enforceHTTPS
      `Prelude.seq` Prelude.rnf customEndpoint

instance Core.ToJSON DomainEndpointOptions where
  toJSON DomainEndpointOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomEndpointCertificateArn" Core..=)
              Prelude.<$> customEndpointCertificateArn,
            ("TLSSecurityPolicy" Core..=)
              Prelude.<$> tLSSecurityPolicy,
            ("CustomEndpointEnabled" Core..=)
              Prelude.<$> customEndpointEnabled,
            ("EnforceHTTPS" Core..=) Prelude.<$> enforceHTTPS,
            ("CustomEndpoint" Core..=)
              Prelude.<$> customEndpoint
          ]
      )
