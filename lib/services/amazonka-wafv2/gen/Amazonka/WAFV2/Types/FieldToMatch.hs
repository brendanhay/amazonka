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
-- Module      : Amazonka.WAFV2.Types.FieldToMatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.FieldToMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AllQueryArguments
import Amazonka.WAFV2.Types.Body
import Amazonka.WAFV2.Types.Cookies
import Amazonka.WAFV2.Types.Headers
import Amazonka.WAFV2.Types.JsonBody
import Amazonka.WAFV2.Types.Method
import Amazonka.WAFV2.Types.QueryString
import Amazonka.WAFV2.Types.SingleHeader
import Amazonka.WAFV2.Types.SingleQueryArgument
import Amazonka.WAFV2.Types.UriPath

-- | The part of the web request that you want WAF to inspect. Include the
-- single @FieldToMatch@ type that you want to inspect, with additional
-- specifications as needed, according to the type. You specify a single
-- request component in @FieldToMatch@ for each rule statement that
-- requires it. To inspect more than one component of the web request,
-- create a separate rule statement for each component.
--
-- Example JSON for a @QueryString@ field to match:
--
-- @ \"FieldToMatch\": { \"QueryString\": {} }@
--
-- Example JSON for a @Method@ field to match specification:
--
-- @ \"FieldToMatch\": { \"Method\": { \"Name\": \"DELETE\" } }@
--
-- /See:/ 'newFieldToMatch' smart constructor.
data FieldToMatch = FieldToMatch'
  { -- | Inspect the request URI path. This is the part of the web request that
    -- identifies a resource, for example, @\/images\/daily-ad.jpg@.
    uriPath :: Prelude.Maybe UriPath,
    -- | Inspect the HTTP method. The method indicates the type of operation that
    -- the request is asking the origin to perform.
    method :: Prelude.Maybe Method,
    -- | Inspect the request headers. You must configure scope and pattern
    -- matching filters in the @Headers@ object, to define the set of headers
    -- to and the parts of the headers that WAF inspects.
    --
    -- Only the first 8 KB (8192 bytes) of a request\'s headers and only the
    -- first 200 headers are forwarded to WAF for inspection by the underlying
    -- host service. You must configure how to handle any oversize header
    -- content in the @Headers@ object. WAF applies the pattern matching
    -- filters to the headers that it receives from the underlying host
    -- service.
    headers :: Prelude.Maybe Headers,
    -- | Inspect the request body as JSON. The request body immediately follows
    -- the request headers. This is the part of a request that contains any
    -- additional data that you want to send to your web server as the HTTP
    -- request body, such as data from a form.
    --
    -- Only the first 8 KB (8192 bytes) of the request body are forwarded to
    -- WAF for inspection by the underlying host service. For information about
    -- how to handle oversized request bodies, see the @JsonBody@ object
    -- configuration.
    jsonBody :: Prelude.Maybe JsonBody,
    -- | Inspect all query arguments.
    allQueryArguments :: Prelude.Maybe AllQueryArguments,
    -- | Inspect the request body as plain text. The request body immediately
    -- follows the request headers. This is the part of a request that contains
    -- any additional data that you want to send to your web server as the HTTP
    -- request body, such as data from a form.
    --
    -- Only the first 8 KB (8192 bytes) of the request body are forwarded to
    -- WAF for inspection by the underlying host service. For information about
    -- how to handle oversized request bodies, see the @Body@ object
    -- configuration.
    body :: Prelude.Maybe Body,
    -- | Inspect a single header. Provide the name of the header to inspect, for
    -- example, @User-Agent@ or @Referer@. This setting isn\'t case sensitive.
    --
    -- Example JSON: @\"SingleHeader\": { \"Name\": \"haystack\" }@
    --
    -- Alternately, you can filter and inspect all headers with the @Headers@
    -- @FieldToMatch@ setting.
    singleHeader :: Prelude.Maybe SingleHeader,
    -- | Inspect the request cookies. You must configure scope and pattern
    -- matching filters in the @Cookies@ object, to define the set of cookies
    -- and the parts of the cookies that WAF inspects.
    --
    -- Only the first 8 KB (8192 bytes) of a request\'s cookies and only the
    -- first 200 cookies are forwarded to WAF for inspection by the underlying
    -- host service. You must configure how to handle any oversize cookie
    -- content in the @Cookies@ object. WAF applies the pattern matching
    -- filters to the cookies that it receives from the underlying host
    -- service.
    cookies :: Prelude.Maybe Cookies,
    -- | Inspect the query string. This is the part of a URL that appears after a
    -- @?@ character, if any.
    queryString :: Prelude.Maybe QueryString,
    -- | Inspect a single query argument. Provide the name of the query argument
    -- to inspect, such as /UserName/ or /SalesRegion/. The name can be up to
    -- 30 characters long and isn\'t case sensitive.
    --
    -- Example JSON: @\"SingleQueryArgument\": { \"Name\": \"myArgument\" }@
    singleQueryArgument :: Prelude.Maybe SingleQueryArgument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldToMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uriPath', 'fieldToMatch_uriPath' - Inspect the request URI path. This is the part of the web request that
-- identifies a resource, for example, @\/images\/daily-ad.jpg@.
--
-- 'method', 'fieldToMatch_method' - Inspect the HTTP method. The method indicates the type of operation that
-- the request is asking the origin to perform.
--
-- 'headers', 'fieldToMatch_headers' - Inspect the request headers. You must configure scope and pattern
-- matching filters in the @Headers@ object, to define the set of headers
-- to and the parts of the headers that WAF inspects.
--
-- Only the first 8 KB (8192 bytes) of a request\'s headers and only the
-- first 200 headers are forwarded to WAF for inspection by the underlying
-- host service. You must configure how to handle any oversize header
-- content in the @Headers@ object. WAF applies the pattern matching
-- filters to the headers that it receives from the underlying host
-- service.
--
-- 'jsonBody', 'fieldToMatch_jsonBody' - Inspect the request body as JSON. The request body immediately follows
-- the request headers. This is the part of a request that contains any
-- additional data that you want to send to your web server as the HTTP
-- request body, such as data from a form.
--
-- Only the first 8 KB (8192 bytes) of the request body are forwarded to
-- WAF for inspection by the underlying host service. For information about
-- how to handle oversized request bodies, see the @JsonBody@ object
-- configuration.
--
-- 'allQueryArguments', 'fieldToMatch_allQueryArguments' - Inspect all query arguments.
--
-- 'body', 'fieldToMatch_body' - Inspect the request body as plain text. The request body immediately
-- follows the request headers. This is the part of a request that contains
-- any additional data that you want to send to your web server as the HTTP
-- request body, such as data from a form.
--
-- Only the first 8 KB (8192 bytes) of the request body are forwarded to
-- WAF for inspection by the underlying host service. For information about
-- how to handle oversized request bodies, see the @Body@ object
-- configuration.
--
-- 'singleHeader', 'fieldToMatch_singleHeader' - Inspect a single header. Provide the name of the header to inspect, for
-- example, @User-Agent@ or @Referer@. This setting isn\'t case sensitive.
--
-- Example JSON: @\"SingleHeader\": { \"Name\": \"haystack\" }@
--
-- Alternately, you can filter and inspect all headers with the @Headers@
-- @FieldToMatch@ setting.
--
-- 'cookies', 'fieldToMatch_cookies' - Inspect the request cookies. You must configure scope and pattern
-- matching filters in the @Cookies@ object, to define the set of cookies
-- and the parts of the cookies that WAF inspects.
--
-- Only the first 8 KB (8192 bytes) of a request\'s cookies and only the
-- first 200 cookies are forwarded to WAF for inspection by the underlying
-- host service. You must configure how to handle any oversize cookie
-- content in the @Cookies@ object. WAF applies the pattern matching
-- filters to the cookies that it receives from the underlying host
-- service.
--
-- 'queryString', 'fieldToMatch_queryString' - Inspect the query string. This is the part of a URL that appears after a
-- @?@ character, if any.
--
-- 'singleQueryArgument', 'fieldToMatch_singleQueryArgument' - Inspect a single query argument. Provide the name of the query argument
-- to inspect, such as /UserName/ or /SalesRegion/. The name can be up to
-- 30 characters long and isn\'t case sensitive.
--
-- Example JSON: @\"SingleQueryArgument\": { \"Name\": \"myArgument\" }@
newFieldToMatch ::
  FieldToMatch
newFieldToMatch =
  FieldToMatch'
    { uriPath = Prelude.Nothing,
      method = Prelude.Nothing,
      headers = Prelude.Nothing,
      jsonBody = Prelude.Nothing,
      allQueryArguments = Prelude.Nothing,
      body = Prelude.Nothing,
      singleHeader = Prelude.Nothing,
      cookies = Prelude.Nothing,
      queryString = Prelude.Nothing,
      singleQueryArgument = Prelude.Nothing
    }

-- | Inspect the request URI path. This is the part of the web request that
-- identifies a resource, for example, @\/images\/daily-ad.jpg@.
fieldToMatch_uriPath :: Lens.Lens' FieldToMatch (Prelude.Maybe UriPath)
fieldToMatch_uriPath = Lens.lens (\FieldToMatch' {uriPath} -> uriPath) (\s@FieldToMatch' {} a -> s {uriPath = a} :: FieldToMatch)

-- | Inspect the HTTP method. The method indicates the type of operation that
-- the request is asking the origin to perform.
fieldToMatch_method :: Lens.Lens' FieldToMatch (Prelude.Maybe Method)
fieldToMatch_method = Lens.lens (\FieldToMatch' {method} -> method) (\s@FieldToMatch' {} a -> s {method = a} :: FieldToMatch)

-- | Inspect the request headers. You must configure scope and pattern
-- matching filters in the @Headers@ object, to define the set of headers
-- to and the parts of the headers that WAF inspects.
--
-- Only the first 8 KB (8192 bytes) of a request\'s headers and only the
-- first 200 headers are forwarded to WAF for inspection by the underlying
-- host service. You must configure how to handle any oversize header
-- content in the @Headers@ object. WAF applies the pattern matching
-- filters to the headers that it receives from the underlying host
-- service.
fieldToMatch_headers :: Lens.Lens' FieldToMatch (Prelude.Maybe Headers)
fieldToMatch_headers = Lens.lens (\FieldToMatch' {headers} -> headers) (\s@FieldToMatch' {} a -> s {headers = a} :: FieldToMatch)

-- | Inspect the request body as JSON. The request body immediately follows
-- the request headers. This is the part of a request that contains any
-- additional data that you want to send to your web server as the HTTP
-- request body, such as data from a form.
--
-- Only the first 8 KB (8192 bytes) of the request body are forwarded to
-- WAF for inspection by the underlying host service. For information about
-- how to handle oversized request bodies, see the @JsonBody@ object
-- configuration.
fieldToMatch_jsonBody :: Lens.Lens' FieldToMatch (Prelude.Maybe JsonBody)
fieldToMatch_jsonBody = Lens.lens (\FieldToMatch' {jsonBody} -> jsonBody) (\s@FieldToMatch' {} a -> s {jsonBody = a} :: FieldToMatch)

-- | Inspect all query arguments.
fieldToMatch_allQueryArguments :: Lens.Lens' FieldToMatch (Prelude.Maybe AllQueryArguments)
fieldToMatch_allQueryArguments = Lens.lens (\FieldToMatch' {allQueryArguments} -> allQueryArguments) (\s@FieldToMatch' {} a -> s {allQueryArguments = a} :: FieldToMatch)

-- | Inspect the request body as plain text. The request body immediately
-- follows the request headers. This is the part of a request that contains
-- any additional data that you want to send to your web server as the HTTP
-- request body, such as data from a form.
--
-- Only the first 8 KB (8192 bytes) of the request body are forwarded to
-- WAF for inspection by the underlying host service. For information about
-- how to handle oversized request bodies, see the @Body@ object
-- configuration.
fieldToMatch_body :: Lens.Lens' FieldToMatch (Prelude.Maybe Body)
fieldToMatch_body = Lens.lens (\FieldToMatch' {body} -> body) (\s@FieldToMatch' {} a -> s {body = a} :: FieldToMatch)

-- | Inspect a single header. Provide the name of the header to inspect, for
-- example, @User-Agent@ or @Referer@. This setting isn\'t case sensitive.
--
-- Example JSON: @\"SingleHeader\": { \"Name\": \"haystack\" }@
--
-- Alternately, you can filter and inspect all headers with the @Headers@
-- @FieldToMatch@ setting.
fieldToMatch_singleHeader :: Lens.Lens' FieldToMatch (Prelude.Maybe SingleHeader)
fieldToMatch_singleHeader = Lens.lens (\FieldToMatch' {singleHeader} -> singleHeader) (\s@FieldToMatch' {} a -> s {singleHeader = a} :: FieldToMatch)

-- | Inspect the request cookies. You must configure scope and pattern
-- matching filters in the @Cookies@ object, to define the set of cookies
-- and the parts of the cookies that WAF inspects.
--
-- Only the first 8 KB (8192 bytes) of a request\'s cookies and only the
-- first 200 cookies are forwarded to WAF for inspection by the underlying
-- host service. You must configure how to handle any oversize cookie
-- content in the @Cookies@ object. WAF applies the pattern matching
-- filters to the cookies that it receives from the underlying host
-- service.
fieldToMatch_cookies :: Lens.Lens' FieldToMatch (Prelude.Maybe Cookies)
fieldToMatch_cookies = Lens.lens (\FieldToMatch' {cookies} -> cookies) (\s@FieldToMatch' {} a -> s {cookies = a} :: FieldToMatch)

-- | Inspect the query string. This is the part of a URL that appears after a
-- @?@ character, if any.
fieldToMatch_queryString :: Lens.Lens' FieldToMatch (Prelude.Maybe QueryString)
fieldToMatch_queryString = Lens.lens (\FieldToMatch' {queryString} -> queryString) (\s@FieldToMatch' {} a -> s {queryString = a} :: FieldToMatch)

-- | Inspect a single query argument. Provide the name of the query argument
-- to inspect, such as /UserName/ or /SalesRegion/. The name can be up to
-- 30 characters long and isn\'t case sensitive.
--
-- Example JSON: @\"SingleQueryArgument\": { \"Name\": \"myArgument\" }@
fieldToMatch_singleQueryArgument :: Lens.Lens' FieldToMatch (Prelude.Maybe SingleQueryArgument)
fieldToMatch_singleQueryArgument = Lens.lens (\FieldToMatch' {singleQueryArgument} -> singleQueryArgument) (\s@FieldToMatch' {} a -> s {singleQueryArgument = a} :: FieldToMatch)

instance Core.FromJSON FieldToMatch where
  parseJSON =
    Core.withObject
      "FieldToMatch"
      ( \x ->
          FieldToMatch'
            Prelude.<$> (x Core..:? "UriPath")
            Prelude.<*> (x Core..:? "Method")
            Prelude.<*> (x Core..:? "Headers")
            Prelude.<*> (x Core..:? "JsonBody")
            Prelude.<*> (x Core..:? "AllQueryArguments")
            Prelude.<*> (x Core..:? "Body")
            Prelude.<*> (x Core..:? "SingleHeader")
            Prelude.<*> (x Core..:? "Cookies")
            Prelude.<*> (x Core..:? "QueryString")
            Prelude.<*> (x Core..:? "SingleQueryArgument")
      )

instance Prelude.Hashable FieldToMatch where
  hashWithSalt _salt FieldToMatch' {..} =
    _salt `Prelude.hashWithSalt` uriPath
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` jsonBody
      `Prelude.hashWithSalt` allQueryArguments
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` singleHeader
      `Prelude.hashWithSalt` cookies
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` singleQueryArgument

instance Prelude.NFData FieldToMatch where
  rnf FieldToMatch' {..} =
    Prelude.rnf uriPath
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf jsonBody
      `Prelude.seq` Prelude.rnf allQueryArguments
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf singleHeader
      `Prelude.seq` Prelude.rnf cookies
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf singleQueryArgument

instance Core.ToJSON FieldToMatch where
  toJSON FieldToMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UriPath" Core..=) Prelude.<$> uriPath,
            ("Method" Core..=) Prelude.<$> method,
            ("Headers" Core..=) Prelude.<$> headers,
            ("JsonBody" Core..=) Prelude.<$> jsonBody,
            ("AllQueryArguments" Core..=)
              Prelude.<$> allQueryArguments,
            ("Body" Core..=) Prelude.<$> body,
            ("SingleHeader" Core..=) Prelude.<$> singleHeader,
            ("Cookies" Core..=) Prelude.<$> cookies,
            ("QueryString" Core..=) Prelude.<$> queryString,
            ("SingleQueryArgument" Core..=)
              Prelude.<$> singleQueryArgument
          ]
      )
