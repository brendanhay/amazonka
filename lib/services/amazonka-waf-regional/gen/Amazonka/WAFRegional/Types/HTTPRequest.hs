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
-- Module      : Amazonka.WAFRegional.Types.HTTPRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFRegional.Types.HTTPRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFRegional.Types.HTTPHeader

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The response from a GetSampledRequests request includes an @HTTPRequest@
-- complex type that appears as @Request@ in the response syntax.
-- @HTTPRequest@ contains information about one of the web requests that
-- were returned by @GetSampledRequests@.
--
-- /See:/ 'newHTTPRequest' smart constructor.
data HTTPRequest = HTTPRequest'
  { -- | The IP address that the request originated from. If the @WebACL@ is
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
    -- | A complex type that contains two values for each header in the sampled
    -- web request: the name of the header and the value of the header.
    headers :: Prelude.Maybe [HTTPHeader],
    -- | The HTTP method specified in the sampled web request. CloudFront
    -- supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
    -- @PATCH@, @POST@, and @PUT@.
    method :: Prelude.Maybe Prelude.Text,
    -- | The part of a web request that identifies the resource, for example,
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
-- 'clientIP', 'hTTPRequest_clientIP' - The IP address that the request originated from. If the @WebACL@ is
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
-- 'headers', 'hTTPRequest_headers' - A complex type that contains two values for each header in the sampled
-- web request: the name of the header and the value of the header.
--
-- 'method', 'hTTPRequest_method' - The HTTP method specified in the sampled web request. CloudFront
-- supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
-- @PATCH@, @POST@, and @PUT@.
--
-- 'uri', 'hTTPRequest_uri' - The part of a web request that identifies the resource, for example,
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

-- | The IP address that the request originated from. If the @WebACL@ is
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

-- | A complex type that contains two values for each header in the sampled
-- web request: the name of the header and the value of the header.
hTTPRequest_headers :: Lens.Lens' HTTPRequest (Prelude.Maybe [HTTPHeader])
hTTPRequest_headers = Lens.lens (\HTTPRequest' {headers} -> headers) (\s@HTTPRequest' {} a -> s {headers = a} :: HTTPRequest) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP method specified in the sampled web request. CloudFront
-- supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
-- @PATCH@, @POST@, and @PUT@.
hTTPRequest_method :: Lens.Lens' HTTPRequest (Prelude.Maybe Prelude.Text)
hTTPRequest_method = Lens.lens (\HTTPRequest' {method} -> method) (\s@HTTPRequest' {} a -> s {method = a} :: HTTPRequest)

-- | The part of a web request that identifies the resource, for example,
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
    _salt `Prelude.hashWithSalt` clientIP
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
