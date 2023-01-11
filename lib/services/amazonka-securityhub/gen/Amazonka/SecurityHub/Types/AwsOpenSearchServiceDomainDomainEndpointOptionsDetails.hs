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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDomainEndpointOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about additional options for the domain endpoint.
--
-- /See:/ 'newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainDomainEndpointOptionsDetails = AwsOpenSearchServiceDomainDomainEndpointOptionsDetails'
  { -- | The fully qualified URL for the custom endpoint.
    customEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the security certificate. The certificate is managed in ACM.
    customEndpointCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable a custom endpoint for the domain.
    customEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether to require that all traffic to the domain arrive over HTTPS.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool,
    -- | The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
    -- domain.
    tLSSecurityPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEndpoint', 'awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint' - The fully qualified URL for the custom endpoint.
--
-- 'customEndpointCertificateArn', 'awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn' - The ARN for the security certificate. The certificate is managed in ACM.
--
-- 'customEndpointEnabled', 'awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled' - Whether to enable a custom endpoint for the domain.
--
-- 'enforceHTTPS', 'awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS' - Whether to require that all traffic to the domain arrive over HTTPS.
--
-- 'tLSSecurityPolicy', 'awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy' - The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
-- domain.
newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails ::
  AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails =
  AwsOpenSearchServiceDomainDomainEndpointOptionsDetails'
    { customEndpoint =
        Prelude.Nothing,
      customEndpointCertificateArn =
        Prelude.Nothing,
      customEndpointEnabled =
        Prelude.Nothing,
      enforceHTTPS =
        Prelude.Nothing,
      tLSSecurityPolicy =
        Prelude.Nothing
    }

-- | The fully qualified URL for the custom endpoint.
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint :: Lens.Lens' AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint = Lens.lens (\AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {customEndpoint} -> customEndpoint) (\s@AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {} a -> s {customEndpoint = a} :: AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)

-- | The ARN for the security certificate. The certificate is managed in ACM.
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn :: Lens.Lens' AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn = Lens.lens (\AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {customEndpointCertificateArn} -> customEndpointCertificateArn) (\s@AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {} a -> s {customEndpointCertificateArn = a} :: AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)

-- | Whether to enable a custom endpoint for the domain.
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled :: Lens.Lens' AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled = Lens.lens (\AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {customEndpointEnabled} -> customEndpointEnabled) (\s@AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {} a -> s {customEndpointEnabled = a} :: AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)

-- | Whether to require that all traffic to the domain arrive over HTTPS.
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS :: Lens.Lens' AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS = Lens.lens (\AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {enforceHTTPS} -> enforceHTTPS) (\s@AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {} a -> s {enforceHTTPS = a} :: AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)

-- | The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
-- domain.
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy :: Lens.Lens' AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy = Lens.lens (\AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {} a -> s {tLSSecurityPolicy = a} :: AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainDomainEndpointOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainDomainEndpointOptionsDetails'
            Prelude.<$> (x Data..:? "CustomEndpoint")
              Prelude.<*> (x Data..:? "CustomEndpointCertificateArn")
              Prelude.<*> (x Data..:? "CustomEndpointEnabled")
              Prelude.<*> (x Data..:? "EnforceHTTPS")
              Prelude.<*> (x Data..:? "TLSSecurityPolicy")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` customEndpoint
        `Prelude.hashWithSalt` customEndpointCertificateArn
        `Prelude.hashWithSalt` customEndpointEnabled
        `Prelude.hashWithSalt` enforceHTTPS
        `Prelude.hashWithSalt` tLSSecurityPolicy

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {..} =
      Prelude.rnf customEndpoint
        `Prelude.seq` Prelude.rnf customEndpointCertificateArn
        `Prelude.seq` Prelude.rnf customEndpointEnabled
        `Prelude.seq` Prelude.rnf enforceHTTPS
        `Prelude.seq` Prelude.rnf tLSSecurityPolicy

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails' {..} =
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
