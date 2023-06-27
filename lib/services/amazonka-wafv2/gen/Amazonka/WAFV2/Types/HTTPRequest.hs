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
-- Module      : Amazonka.WAFV2.Types.HTTPRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.HTTPRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.HTTPHeader

-- | Part of the response from GetSampledRequests. This is a complex type
-- that appears as @Request@ in the response syntax. @HTTPRequest@ contains
-- information about one of the web requests.
--
-- /See:/ 'newHTTPRequest' smart constructor.
data HTTPRequest = HTTPRequest'
  { -- | The IP address that the request originated from. If the web ACL is
    -- associated with a CloudFront distribution, this is the value of one of
    -- the following fields in CloudFront access logs:
    --
    -- -   @c-ip@, if the viewer did not use an HTTP proxy or a load balancer
    --     to send the request
    --
    -- -   @x-forwarded-for@, if the viewer did use an HTTP proxy or a load
    --     balancer to send the request
    clientIP :: Prelude.Maybe Prelude.Text,
    -- | The two-letter country code for the country that the request originated
    -- from. For a current list of country codes, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2>.
    country :: Prelude.Maybe Prelude.Text,
    -- | The HTTP version specified in the sampled web request, for example,
    -- @HTTP\/1.1@.
    hTTPVersion :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains the name and value for each header in the
    -- sampled web request.
    headers :: Prelude.Maybe [HTTPHeader],
    -- | The HTTP method specified in the sampled web request.
    method :: Prelude.Maybe Prelude.Text,
    -- | The URI path of the request, which identifies the resource, for example,
    -- @\/images\/daily-ad.jpg@.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HTTPRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIP', 'hTTPRequest_clientIP' - The IP address that the request originated from. If the web ACL is
-- associated with a CloudFront distribution, this is the value of one of
-- the following fields in CloudFront access logs:
--
-- -   @c-ip@, if the viewer did not use an HTTP proxy or a load balancer
--     to send the request
--
-- -   @x-forwarded-for@, if the viewer did use an HTTP proxy or a load
--     balancer to send the request
--
-- 'country', 'hTTPRequest_country' - The two-letter country code for the country that the request originated
-- from. For a current list of country codes, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2>.
--
-- 'hTTPVersion', 'hTTPRequest_hTTPVersion' - The HTTP version specified in the sampled web request, for example,
-- @HTTP\/1.1@.
--
-- 'headers', 'hTTPRequest_headers' - A complex type that contains the name and value for each header in the
-- sampled web request.
--
-- 'method', 'hTTPRequest_method' - The HTTP method specified in the sampled web request.
--
-- 'uri', 'hTTPRequest_uri' - The URI path of the request, which identifies the resource, for example,
-- @\/images\/daily-ad.jpg@.
newHTTPRequest ::
  HTTPRequest
newHTTPRequest =
  HTTPRequest'
    { clientIP = Prelude.Nothing,
      country = Prelude.Nothing,
      hTTPVersion = Prelude.Nothing,
      headers = Prelude.Nothing,
      method = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | The IP address that the request originated from. If the web ACL is
-- associated with a CloudFront distribution, this is the value of one of
-- the following fields in CloudFront access logs:
--
-- -   @c-ip@, if the viewer did not use an HTTP proxy or a load balancer
--     to send the request
--
-- -   @x-forwarded-for@, if the viewer did use an HTTP proxy or a load
--     balancer to send the request
hTTPRequest_clientIP :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_clientIP = Lens.lens (\HTTPRequest' {clientIP} -> clientIP) (\s@HTTPRequest' {} a -> s {clientIP = a} :: HTTPRequest)

-- | The two-letter country code for the country that the request originated
-- from. For a current list of country codes, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2>.
hTTPRequest_country :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_country = Lens.lens (\HTTPRequest' {country} -> country) (\s@HTTPRequest' {} a -> s {country = a} :: HTTPRequest)

-- | The HTTP version specified in the sampled web request, for example,
-- @HTTP\/1.1@.
hTTPRequest_hTTPVersion :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_hTTPVersion = Lens.lens (\HTTPRequest' {hTTPVersion} -> hTTPVersion) (\s@HTTPRequest' {} a -> s {hTTPVersion = a} :: HTTPRequest)

-- | A complex type that contains the name and value for each header in the
-- sampled web request.
hTTPRequest_headers :: Lens.Lens' HTTPRequest (Prelude.Maybe [HTTPHeader])
hTTPRequest_headers = Lens.lens (\HTTPRequest' {headers} -> headers) (\s@HTTPRequest' {} a -> s {headers = a} :: HTTPRequest) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP method specified in the sampled web request.
hTTPRequest_method :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_method = Lens.lens (\HTTPRequest' {method} -> method) (\s@HTTPRequest' {} a -> s {method = a} :: HTTPRequest)

-- | The URI path of the request, which identifies the resource, for example,
-- @\/images\/daily-ad.jpg@.
hTTPRequest_uri :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_uri = Lens.lens (\HTTPRequest' {uri} -> uri) (\s@HTTPRequest' {} a -> s {uri = a} :: HTTPRequest)

instance Data.FromJSON HTTPRequest where
  parseJSON =
    Data.withObject
      "HTTPRequest"
      ( \x ->
          HTTPRequest'
            Prelude.<$> (x Data..:? "ClientIP")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "HTTPVersion")
            Prelude.<*> (x Data..:? "Headers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Method")
            Prelude.<*> (x Data..:? "URI")
      )

instance Prelude.Hashable HTTPRequest where
  hashWithSalt _salt HTTPRequest' {..} =
    _salt
      `Prelude.hashWithSalt` clientIP
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` hTTPVersion
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` uri

instance Prelude.NFData HTTPRequest where
  rnf HTTPRequest' {..} =
    Prelude.rnf clientIP
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf hTTPVersion
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf uri
