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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the URL endpoint type.
--
-- /See:/ 'newUrlEndpointInput' smart constructor.
data UrlEndpointInput = UrlEndpointInput'
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
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UrlEndpointInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthUrl', 'urlEndpointInput_healthUrl' - The health check URL of the URL endpoint type. If the URL is a public
-- endpoint, the @HealthUrl@ must also be a public endpoint. If the URL is
-- a private endpoint inside a virtual private cloud (VPC), the health URL
-- must also be a private endpoint, and the host must be the same as the
-- URL.
--
-- 'url', 'urlEndpointInput_url' - The URL to route traffic to. The URL must be an
-- <https://datatracker.ietf.org/doc/html/rfc3986 rfc3986-formatted URL>.
-- If the host is a domain name, the name must be resolvable over the
-- public internet. If the scheme is @https@, the top level domain of the
-- host must be listed in the
-- <https://www.iana.org/domains/root/db IANA root zone database>.
newUrlEndpointInput ::
  -- | 'url'
  Prelude.Text ->
  UrlEndpointInput
newUrlEndpointInput pUrl_ =
  UrlEndpointInput'
    { healthUrl = Prelude.Nothing,
      url = pUrl_
    }

-- | The health check URL of the URL endpoint type. If the URL is a public
-- endpoint, the @HealthUrl@ must also be a public endpoint. If the URL is
-- a private endpoint inside a virtual private cloud (VPC), the health URL
-- must also be a private endpoint, and the host must be the same as the
-- URL.
urlEndpointInput_healthUrl :: Lens.Lens' UrlEndpointInput (Prelude.Maybe Prelude.Text)
urlEndpointInput_healthUrl = Lens.lens (\UrlEndpointInput' {healthUrl} -> healthUrl) (\s@UrlEndpointInput' {} a -> s {healthUrl = a} :: UrlEndpointInput)

-- | The URL to route traffic to. The URL must be an
-- <https://datatracker.ietf.org/doc/html/rfc3986 rfc3986-formatted URL>.
-- If the host is a domain name, the name must be resolvable over the
-- public internet. If the scheme is @https@, the top level domain of the
-- host must be listed in the
-- <https://www.iana.org/domains/root/db IANA root zone database>.
urlEndpointInput_url :: Lens.Lens' UrlEndpointInput Prelude.Text
urlEndpointInput_url = Lens.lens (\UrlEndpointInput' {url} -> url) (\s@UrlEndpointInput' {} a -> s {url = a} :: UrlEndpointInput)

instance Data.FromJSON UrlEndpointInput where
  parseJSON =
    Data.withObject
      "UrlEndpointInput"
      ( \x ->
          UrlEndpointInput'
            Prelude.<$> (x Data..:? "HealthUrl")
            Prelude.<*> (x Data..: "Url")
      )

instance Prelude.Hashable UrlEndpointInput where
  hashWithSalt _salt UrlEndpointInput' {..} =
    _salt `Prelude.hashWithSalt` healthUrl
      `Prelude.hashWithSalt` url

instance Prelude.NFData UrlEndpointInput where
  rnf UrlEndpointInput' {..} =
    Prelude.rnf healthUrl `Prelude.seq` Prelude.rnf url

instance Data.ToJSON UrlEndpointInput where
  toJSON UrlEndpointInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HealthUrl" Data..=) Prelude.<$> healthUrl,
            Prelude.Just ("Url" Data..= url)
          ]
      )
