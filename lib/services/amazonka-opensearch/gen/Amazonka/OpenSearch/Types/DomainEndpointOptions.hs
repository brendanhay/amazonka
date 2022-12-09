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
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.TLSSecurityPolicy
import qualified Amazonka.Prelude as Prelude

-- | Options to configure a custom endpoint for an OpenSearch Service domain.
--
-- /See:/ 'newDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | The fully qualified URL for the custom endpoint.
    customEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN for your security certificate, managed in Amazon Web Services
    -- Certificate Manager (ACM).
    customEndpointCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable a custom endpoint for the domain.
    customEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | True to require that all traffic to the domain arrive over HTTPS.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool,
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
    tLSSecurityPolicy :: Prelude.Maybe TLSSecurityPolicy
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
-- 'customEndpoint', 'domainEndpointOptions_customEndpoint' - The fully qualified URL for the custom endpoint.
--
-- 'customEndpointCertificateArn', 'domainEndpointOptions_customEndpointCertificateArn' - The ARN for your security certificate, managed in Amazon Web Services
-- Certificate Manager (ACM).
--
-- 'customEndpointEnabled', 'domainEndpointOptions_customEndpointEnabled' - Whether to enable a custom endpoint for the domain.
--
-- 'enforceHTTPS', 'domainEndpointOptions_enforceHTTPS' - True to require that all traffic to the domain arrive over HTTPS.
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
newDomainEndpointOptions ::
  DomainEndpointOptions
newDomainEndpointOptions =
  DomainEndpointOptions'
    { customEndpoint =
        Prelude.Nothing,
      customEndpointCertificateArn = Prelude.Nothing,
      customEndpointEnabled = Prelude.Nothing,
      enforceHTTPS = Prelude.Nothing,
      tLSSecurityPolicy = Prelude.Nothing
    }

-- | The fully qualified URL for the custom endpoint.
domainEndpointOptions_customEndpoint :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpoint = Lens.lens (\DomainEndpointOptions' {customEndpoint} -> customEndpoint) (\s@DomainEndpointOptions' {} a -> s {customEndpoint = a} :: DomainEndpointOptions)

-- | The ARN for your security certificate, managed in Amazon Web Services
-- Certificate Manager (ACM).
domainEndpointOptions_customEndpointCertificateArn :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Text)
domainEndpointOptions_customEndpointCertificateArn = Lens.lens (\DomainEndpointOptions' {customEndpointCertificateArn} -> customEndpointCertificateArn) (\s@DomainEndpointOptions' {} a -> s {customEndpointCertificateArn = a} :: DomainEndpointOptions)

-- | Whether to enable a custom endpoint for the domain.
domainEndpointOptions_customEndpointEnabled :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_customEndpointEnabled = Lens.lens (\DomainEndpointOptions' {customEndpointEnabled} -> customEndpointEnabled) (\s@DomainEndpointOptions' {} a -> s {customEndpointEnabled = a} :: DomainEndpointOptions)

-- | True to require that all traffic to the domain arrive over HTTPS.
domainEndpointOptions_enforceHTTPS :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_enforceHTTPS = Lens.lens (\DomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@DomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: DomainEndpointOptions)

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

instance Data.FromJSON DomainEndpointOptions where
  parseJSON =
    Data.withObject
      "DomainEndpointOptions"
      ( \x ->
          DomainEndpointOptions'
            Prelude.<$> (x Data..:? "CustomEndpoint")
            Prelude.<*> (x Data..:? "CustomEndpointCertificateArn")
            Prelude.<*> (x Data..:? "CustomEndpointEnabled")
            Prelude.<*> (x Data..:? "EnforceHTTPS")
            Prelude.<*> (x Data..:? "TLSSecurityPolicy")
      )

instance Prelude.Hashable DomainEndpointOptions where
  hashWithSalt _salt DomainEndpointOptions' {..} =
    _salt `Prelude.hashWithSalt` customEndpoint
      `Prelude.hashWithSalt` customEndpointCertificateArn
      `Prelude.hashWithSalt` customEndpointEnabled
      `Prelude.hashWithSalt` enforceHTTPS
      `Prelude.hashWithSalt` tLSSecurityPolicy

instance Prelude.NFData DomainEndpointOptions where
  rnf DomainEndpointOptions' {..} =
    Prelude.rnf customEndpoint
      `Prelude.seq` Prelude.rnf customEndpointCertificateArn
      `Prelude.seq` Prelude.rnf customEndpointEnabled
      `Prelude.seq` Prelude.rnf enforceHTTPS
      `Prelude.seq` Prelude.rnf tLSSecurityPolicy

instance Data.ToJSON DomainEndpointOptions where
  toJSON DomainEndpointOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomEndpoint" Data..=)
              Prelude.<$> customEndpoint,
            ("CustomEndpointCertificateArn" Data..=)
              Prelude.<$> customEndpointCertificateArn,
            ("CustomEndpointEnabled" Data..=)
              Prelude.<$> customEndpointEnabled,
            ("EnforceHTTPS" Data..=) Prelude.<$> enforceHTTPS,
            ("TLSSecurityPolicy" Data..=)
              Prelude.<$> tLSSecurityPolicy
          ]
      )
