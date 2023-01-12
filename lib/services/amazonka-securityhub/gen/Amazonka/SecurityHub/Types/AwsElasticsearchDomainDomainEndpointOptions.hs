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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainDomainEndpointOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainDomainEndpointOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- /See:/ 'newAwsElasticsearchDomainDomainEndpointOptions' smart constructor.
data AwsElasticsearchDomainDomainEndpointOptions = AwsElasticsearchDomainDomainEndpointOptions'
  { -- | Whether to require that all traffic to the domain arrive over HTTPS.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool,
    -- | The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
    -- domain.
    --
    -- Valid values:
    --
    -- -   @Policy-Min-TLS-1-0-2019-07@, which supports TLSv1.0 and higher
    --
    -- -   @Policy-Min-TLS-1-2-2019-07@, which only supports TLSv1.2
    tLSSecurityPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainDomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enforceHTTPS', 'awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS' - Whether to require that all traffic to the domain arrive over HTTPS.
--
-- 'tLSSecurityPolicy', 'awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy' - The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
-- domain.
--
-- Valid values:
--
-- -   @Policy-Min-TLS-1-0-2019-07@, which supports TLSv1.0 and higher
--
-- -   @Policy-Min-TLS-1-2-2019-07@, which only supports TLSv1.2
newAwsElasticsearchDomainDomainEndpointOptions ::
  AwsElasticsearchDomainDomainEndpointOptions
newAwsElasticsearchDomainDomainEndpointOptions =
  AwsElasticsearchDomainDomainEndpointOptions'
    { enforceHTTPS =
        Prelude.Nothing,
      tLSSecurityPolicy =
        Prelude.Nothing
    }

-- | Whether to require that all traffic to the domain arrive over HTTPS.
awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS :: Lens.Lens' AwsElasticsearchDomainDomainEndpointOptions (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS = Lens.lens (\AwsElasticsearchDomainDomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@AwsElasticsearchDomainDomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: AwsElasticsearchDomainDomainEndpointOptions)

-- | The TLS security policy to apply to the HTTPS endpoint of the OpenSearch
-- domain.
--
-- Valid values:
--
-- -   @Policy-Min-TLS-1-0-2019-07@, which supports TLSv1.0 and higher
--
-- -   @Policy-Min-TLS-1-2-2019-07@, which only supports TLSv1.2
awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy :: Lens.Lens' AwsElasticsearchDomainDomainEndpointOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy = Lens.lens (\AwsElasticsearchDomainDomainEndpointOptions' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@AwsElasticsearchDomainDomainEndpointOptions' {} a -> s {tLSSecurityPolicy = a} :: AwsElasticsearchDomainDomainEndpointOptions)

instance
  Data.FromJSON
    AwsElasticsearchDomainDomainEndpointOptions
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainDomainEndpointOptions"
      ( \x ->
          AwsElasticsearchDomainDomainEndpointOptions'
            Prelude.<$> (x Data..:? "EnforceHTTPS")
              Prelude.<*> (x Data..:? "TLSSecurityPolicy")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainDomainEndpointOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainDomainEndpointOptions' {..} =
      _salt `Prelude.hashWithSalt` enforceHTTPS
        `Prelude.hashWithSalt` tLSSecurityPolicy

instance
  Prelude.NFData
    AwsElasticsearchDomainDomainEndpointOptions
  where
  rnf AwsElasticsearchDomainDomainEndpointOptions' {..} =
    Prelude.rnf enforceHTTPS
      `Prelude.seq` Prelude.rnf tLSSecurityPolicy

instance
  Data.ToJSON
    AwsElasticsearchDomainDomainEndpointOptions
  where
  toJSON
    AwsElasticsearchDomainDomainEndpointOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("EnforceHTTPS" Data..=) Prelude.<$> enforceHTTPS,
              ("TLSSecurityPolicy" Data..=)
                Prelude.<$> tLSSecurityPolicy
            ]
        )
