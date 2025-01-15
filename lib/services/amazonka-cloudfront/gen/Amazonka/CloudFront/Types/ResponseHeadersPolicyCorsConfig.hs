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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyCorsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyCorsConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowHeaders
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethods
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowOrigins
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlExposeHeaders
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration for a set of HTTP response headers that are used for
-- cross-origin resource sharing (CORS). CloudFront adds these headers to
-- HTTP responses that it sends for CORS requests that match a cache
-- behavior associated with this response headers policy.
--
-- For more information about CORS, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS Cross-Origin Resource Sharing (CORS)>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyCorsConfig' smart constructor.
data ResponseHeadersPolicyCorsConfig = ResponseHeadersPolicyCorsConfig'
  { -- | A list of HTTP headers that CloudFront includes as values for the
    -- @Access-Control-Expose-Headers@ HTTP response header.
    --
    -- For more information about the @Access-Control-Expose-Headers@ HTTP
    -- response header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers Access-Control-Expose-Headers>
    -- in the MDN Web Docs.
    accessControlExposeHeaders :: Prelude.Maybe ResponseHeadersPolicyAccessControlExposeHeaders,
    -- | A number that CloudFront uses as the value for the
    -- @Access-Control-Max-Age@ HTTP response header.
    --
    -- For more information about the @Access-Control-Max-Age@ HTTP response
    -- header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Max-Age Access-Control-Max-Age>
    -- in the MDN Web Docs.
    accessControlMaxAgeSec :: Prelude.Maybe Prelude.Int,
    -- | A list of origins (domain names) that CloudFront can use as the value
    -- for the @Access-Control-Allow-Origin@ HTTP response header.
    --
    -- For more information about the @Access-Control-Allow-Origin@ HTTP
    -- response header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin Access-Control-Allow-Origin>
    -- in the MDN Web Docs.
    accessControlAllowOrigins :: ResponseHeadersPolicyAccessControlAllowOrigins,
    -- | A list of HTTP header names that CloudFront includes as values for the
    -- @Access-Control-Allow-Headers@ HTTP response header.
    --
    -- For more information about the @Access-Control-Allow-Headers@ HTTP
    -- response header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers Access-Control-Allow-Headers>
    -- in the MDN Web Docs.
    accessControlAllowHeaders :: ResponseHeadersPolicyAccessControlAllowHeaders,
    -- | A list of HTTP methods that CloudFront includes as values for the
    -- @Access-Control-Allow-Methods@ HTTP response header.
    --
    -- For more information about the @Access-Control-Allow-Methods@ HTTP
    -- response header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods Access-Control-Allow-Methods>
    -- in the MDN Web Docs.
    accessControlAllowMethods :: ResponseHeadersPolicyAccessControlAllowMethods,
    -- | A Boolean that CloudFront uses as the value for the
    -- @Access-Control-Allow-Credentials@ HTTP response header.
    --
    -- For more information about the @Access-Control-Allow-Credentials@ HTTP
    -- response header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials Access-Control-Allow-Credentials>
    -- in the MDN Web Docs.
    accessControlAllowCredentials :: Prelude.Bool,
    -- | A Boolean that determines whether CloudFront overrides HTTP response
    -- headers received from the origin with the ones specified in this
    -- response headers policy.
    originOverride :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyCorsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlExposeHeaders', 'responseHeadersPolicyCorsConfig_accessControlExposeHeaders' - A list of HTTP headers that CloudFront includes as values for the
-- @Access-Control-Expose-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Expose-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers Access-Control-Expose-Headers>
-- in the MDN Web Docs.
--
-- 'accessControlMaxAgeSec', 'responseHeadersPolicyCorsConfig_accessControlMaxAgeSec' - A number that CloudFront uses as the value for the
-- @Access-Control-Max-Age@ HTTP response header.
--
-- For more information about the @Access-Control-Max-Age@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Max-Age Access-Control-Max-Age>
-- in the MDN Web Docs.
--
-- 'accessControlAllowOrigins', 'responseHeadersPolicyCorsConfig_accessControlAllowOrigins' - A list of origins (domain names) that CloudFront can use as the value
-- for the @Access-Control-Allow-Origin@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Origin@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin Access-Control-Allow-Origin>
-- in the MDN Web Docs.
--
-- 'accessControlAllowHeaders', 'responseHeadersPolicyCorsConfig_accessControlAllowHeaders' - A list of HTTP header names that CloudFront includes as values for the
-- @Access-Control-Allow-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers Access-Control-Allow-Headers>
-- in the MDN Web Docs.
--
-- 'accessControlAllowMethods', 'responseHeadersPolicyCorsConfig_accessControlAllowMethods' - A list of HTTP methods that CloudFront includes as values for the
-- @Access-Control-Allow-Methods@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Methods@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods Access-Control-Allow-Methods>
-- in the MDN Web Docs.
--
-- 'accessControlAllowCredentials', 'responseHeadersPolicyCorsConfig_accessControlAllowCredentials' - A Boolean that CloudFront uses as the value for the
-- @Access-Control-Allow-Credentials@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Credentials@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials Access-Control-Allow-Credentials>
-- in the MDN Web Docs.
--
-- 'originOverride', 'responseHeadersPolicyCorsConfig_originOverride' - A Boolean that determines whether CloudFront overrides HTTP response
-- headers received from the origin with the ones specified in this
-- response headers policy.
newResponseHeadersPolicyCorsConfig ::
  -- | 'accessControlAllowOrigins'
  ResponseHeadersPolicyAccessControlAllowOrigins ->
  -- | 'accessControlAllowHeaders'
  ResponseHeadersPolicyAccessControlAllowHeaders ->
  -- | 'accessControlAllowMethods'
  ResponseHeadersPolicyAccessControlAllowMethods ->
  -- | 'accessControlAllowCredentials'
  Prelude.Bool ->
  -- | 'originOverride'
  Prelude.Bool ->
  ResponseHeadersPolicyCorsConfig
newResponseHeadersPolicyCorsConfig
  pAccessControlAllowOrigins_
  pAccessControlAllowHeaders_
  pAccessControlAllowMethods_
  pAccessControlAllowCredentials_
  pOriginOverride_ =
    ResponseHeadersPolicyCorsConfig'
      { accessControlExposeHeaders =
          Prelude.Nothing,
        accessControlMaxAgeSec = Prelude.Nothing,
        accessControlAllowOrigins =
          pAccessControlAllowOrigins_,
        accessControlAllowHeaders =
          pAccessControlAllowHeaders_,
        accessControlAllowMethods =
          pAccessControlAllowMethods_,
        accessControlAllowCredentials =
          pAccessControlAllowCredentials_,
        originOverride = pOriginOverride_
      }

-- | A list of HTTP headers that CloudFront includes as values for the
-- @Access-Control-Expose-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Expose-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers Access-Control-Expose-Headers>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlExposeHeaders :: Lens.Lens' ResponseHeadersPolicyCorsConfig (Prelude.Maybe ResponseHeadersPolicyAccessControlExposeHeaders)
responseHeadersPolicyCorsConfig_accessControlExposeHeaders = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlExposeHeaders} -> accessControlExposeHeaders) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlExposeHeaders = a} :: ResponseHeadersPolicyCorsConfig)

-- | A number that CloudFront uses as the value for the
-- @Access-Control-Max-Age@ HTTP response header.
--
-- For more information about the @Access-Control-Max-Age@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Max-Age Access-Control-Max-Age>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlMaxAgeSec :: Lens.Lens' ResponseHeadersPolicyCorsConfig (Prelude.Maybe Prelude.Int)
responseHeadersPolicyCorsConfig_accessControlMaxAgeSec = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlMaxAgeSec} -> accessControlMaxAgeSec) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlMaxAgeSec = a} :: ResponseHeadersPolicyCorsConfig)

-- | A list of origins (domain names) that CloudFront can use as the value
-- for the @Access-Control-Allow-Origin@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Origin@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin Access-Control-Allow-Origin>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlAllowOrigins :: Lens.Lens' ResponseHeadersPolicyCorsConfig ResponseHeadersPolicyAccessControlAllowOrigins
responseHeadersPolicyCorsConfig_accessControlAllowOrigins = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlAllowOrigins} -> accessControlAllowOrigins) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlAllowOrigins = a} :: ResponseHeadersPolicyCorsConfig)

-- | A list of HTTP header names that CloudFront includes as values for the
-- @Access-Control-Allow-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers Access-Control-Allow-Headers>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlAllowHeaders :: Lens.Lens' ResponseHeadersPolicyCorsConfig ResponseHeadersPolicyAccessControlAllowHeaders
responseHeadersPolicyCorsConfig_accessControlAllowHeaders = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlAllowHeaders} -> accessControlAllowHeaders) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlAllowHeaders = a} :: ResponseHeadersPolicyCorsConfig)

-- | A list of HTTP methods that CloudFront includes as values for the
-- @Access-Control-Allow-Methods@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Methods@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods Access-Control-Allow-Methods>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlAllowMethods :: Lens.Lens' ResponseHeadersPolicyCorsConfig ResponseHeadersPolicyAccessControlAllowMethods
responseHeadersPolicyCorsConfig_accessControlAllowMethods = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlAllowMethods} -> accessControlAllowMethods) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlAllowMethods = a} :: ResponseHeadersPolicyCorsConfig)

-- | A Boolean that CloudFront uses as the value for the
-- @Access-Control-Allow-Credentials@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Credentials@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials Access-Control-Allow-Credentials>
-- in the MDN Web Docs.
responseHeadersPolicyCorsConfig_accessControlAllowCredentials :: Lens.Lens' ResponseHeadersPolicyCorsConfig Prelude.Bool
responseHeadersPolicyCorsConfig_accessControlAllowCredentials = Lens.lens (\ResponseHeadersPolicyCorsConfig' {accessControlAllowCredentials} -> accessControlAllowCredentials) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {accessControlAllowCredentials = a} :: ResponseHeadersPolicyCorsConfig)

-- | A Boolean that determines whether CloudFront overrides HTTP response
-- headers received from the origin with the ones specified in this
-- response headers policy.
responseHeadersPolicyCorsConfig_originOverride :: Lens.Lens' ResponseHeadersPolicyCorsConfig Prelude.Bool
responseHeadersPolicyCorsConfig_originOverride = Lens.lens (\ResponseHeadersPolicyCorsConfig' {originOverride} -> originOverride) (\s@ResponseHeadersPolicyCorsConfig' {} a -> s {originOverride = a} :: ResponseHeadersPolicyCorsConfig)

instance Data.FromXML ResponseHeadersPolicyCorsConfig where
  parseXML x =
    ResponseHeadersPolicyCorsConfig'
      Prelude.<$> (x Data..@? "AccessControlExposeHeaders")
      Prelude.<*> (x Data..@? "AccessControlMaxAgeSec")
      Prelude.<*> (x Data..@ "AccessControlAllowOrigins")
      Prelude.<*> (x Data..@ "AccessControlAllowHeaders")
      Prelude.<*> (x Data..@ "AccessControlAllowMethods")
      Prelude.<*> (x Data..@ "AccessControlAllowCredentials")
      Prelude.<*> (x Data..@ "OriginOverride")

instance
  Prelude.Hashable
    ResponseHeadersPolicyCorsConfig
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyCorsConfig' {..} =
      _salt
        `Prelude.hashWithSalt` accessControlExposeHeaders
        `Prelude.hashWithSalt` accessControlMaxAgeSec
        `Prelude.hashWithSalt` accessControlAllowOrigins
        `Prelude.hashWithSalt` accessControlAllowHeaders
        `Prelude.hashWithSalt` accessControlAllowMethods
        `Prelude.hashWithSalt` accessControlAllowCredentials
        `Prelude.hashWithSalt` originOverride

instance
  Prelude.NFData
    ResponseHeadersPolicyCorsConfig
  where
  rnf ResponseHeadersPolicyCorsConfig' {..} =
    Prelude.rnf accessControlExposeHeaders `Prelude.seq`
      Prelude.rnf accessControlMaxAgeSec `Prelude.seq`
        Prelude.rnf accessControlAllowOrigins `Prelude.seq`
          Prelude.rnf accessControlAllowHeaders `Prelude.seq`
            Prelude.rnf accessControlAllowMethods `Prelude.seq`
              Prelude.rnf accessControlAllowCredentials `Prelude.seq`
                Prelude.rnf originOverride

instance Data.ToXML ResponseHeadersPolicyCorsConfig where
  toXML ResponseHeadersPolicyCorsConfig' {..} =
    Prelude.mconcat
      [ "AccessControlExposeHeaders"
          Data.@= accessControlExposeHeaders,
        "AccessControlMaxAgeSec"
          Data.@= accessControlMaxAgeSec,
        "AccessControlAllowOrigins"
          Data.@= accessControlAllowOrigins,
        "AccessControlAllowHeaders"
          Data.@= accessControlAllowHeaders,
        "AccessControlAllowMethods"
          Data.@= accessControlAllowMethods,
        "AccessControlAllowCredentials"
          Data.@= accessControlAllowCredentials,
        "OriginOverride" Data.@= originOverride
      ]
