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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the configuration for the URL endpoint type.
--
-- /See:/ 'newUrlEndpointSummary' smart constructor.
data UrlEndpointSummary = UrlEndpointSummary'
  { -- | The health check URL of the URL endpoint type. If the URL is a public
    -- endpoint, the @HealthUrl@ must also be a public endpoint. If the URL is
    -- a private endpoint inside a virtual private cloud (VPC), the health URL
    -- must also be a private endpoint, and the host must be the same as the
    -- URL.
    healthUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to route traffic to. The URL must be an
    -- <https://datatracker.ietf.org/doc/html/rfc3986 rfc3986-formatted URL>.
    -- If the host is a domain name, the name must be resolvable over the
    -- public internet. If the scheme is @https@, the top level domain of the
    -- host must be listed in the
    -- <https://www.iana.org/domains/root/db IANA root zone database>.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UrlEndpointSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthUrl', 'urlEndpointSummary_healthUrl' - The health check URL of the URL endpoint type. If the URL is a public
-- endpoint, the @HealthUrl@ must also be a public endpoint. If the URL is
-- a private endpoint inside a virtual private cloud (VPC), the health URL
-- must also be a private endpoint, and the host must be the same as the
-- URL.
--
-- 'url', 'urlEndpointSummary_url' - The URL to route traffic to. The URL must be an
-- <https://datatracker.ietf.org/doc/html/rfc3986 rfc3986-formatted URL>.
-- If the host is a domain name, the name must be resolvable over the
-- public internet. If the scheme is @https@, the top level domain of the
-- host must be listed in the
-- <https://www.iana.org/domains/root/db IANA root zone database>.
newUrlEndpointSummary ::
  UrlEndpointSummary
newUrlEndpointSummary =
  UrlEndpointSummary'
    { healthUrl = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The health check URL of the URL endpoint type. If the URL is a public
-- endpoint, the @HealthUrl@ must also be a public endpoint. If the URL is
-- a private endpoint inside a virtual private cloud (VPC), the health URL
-- must also be a private endpoint, and the host must be the same as the
-- URL.
urlEndpointSummary_healthUrl :: Lens.Lens' UrlEndpointSummary (Prelude.Maybe Prelude.Text)
urlEndpointSummary_healthUrl = Lens.lens (\UrlEndpointSummary' {healthUrl} -> healthUrl) (\s@UrlEndpointSummary' {} a -> s {healthUrl = a} :: UrlEndpointSummary)

-- | The URL to route traffic to. The URL must be an
-- <https://datatracker.ietf.org/doc/html/rfc3986 rfc3986-formatted URL>.
-- If the host is a domain name, the name must be resolvable over the
-- public internet. If the scheme is @https@, the top level domain of the
-- host must be listed in the
-- <https://www.iana.org/domains/root/db IANA root zone database>.
urlEndpointSummary_url :: Lens.Lens' UrlEndpointSummary (Prelude.Maybe Prelude.Text)
urlEndpointSummary_url = Lens.lens (\UrlEndpointSummary' {url} -> url) (\s@UrlEndpointSummary' {} a -> s {url = a} :: UrlEndpointSummary)

instance Data.FromJSON UrlEndpointSummary where
  parseJSON =
    Data.withObject
      "UrlEndpointSummary"
      ( \x ->
          UrlEndpointSummary'
            Prelude.<$> (x Data..:? "HealthUrl")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable UrlEndpointSummary where
  hashWithSalt _salt UrlEndpointSummary' {..} =
    _salt `Prelude.hashWithSalt` healthUrl
      `Prelude.hashWithSalt` url

instance Prelude.NFData UrlEndpointSummary where
  rnf UrlEndpointSummary' {..} =
    Prelude.rnf healthUrl `Prelude.seq` Prelude.rnf url
