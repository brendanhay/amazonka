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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicySecurityHeadersConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicySecurityHeadersConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyContentSecurityPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyContentTypeOptions
import Amazonka.CloudFront.Types.ResponseHeadersPolicyFrameOptions
import Amazonka.CloudFront.Types.ResponseHeadersPolicyReferrerPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyStrictTransportSecurity
import Amazonka.CloudFront.Types.ResponseHeadersPolicyXSSProtection
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration for a set of security-related HTTP response headers.
-- CloudFront adds these headers to HTTP responses that it sends for
-- requests that match a cache behavior associated with this response
-- headers policy.
--
-- /See:/ 'newResponseHeadersPolicySecurityHeadersConfig' smart constructor.
data ResponseHeadersPolicySecurityHeadersConfig = ResponseHeadersPolicySecurityHeadersConfig'
  { -- | Determines whether CloudFront includes the @X-Content-Type-Options@ HTTP
    -- response header with its value set to @nosniff@.
    --
    -- For more information about the @X-Content-Type-Options@ HTTP response
    -- header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options X-Content-Type-Options>
    -- in the MDN Web Docs.
    contentTypeOptions :: Prelude.Maybe ResponseHeadersPolicyContentTypeOptions,
    -- | Determines whether CloudFront includes the @X-XSS-Protection@ HTTP
    -- response header and the header’s value.
    --
    -- For more information about the @X-XSS-Protection@ HTTP response header,
    -- see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
    -- in the MDN Web Docs.
    xSSProtection :: Prelude.Maybe ResponseHeadersPolicyXSSProtection,
    -- | Determines whether CloudFront includes the @X-Frame-Options@ HTTP
    -- response header and the header’s value.
    --
    -- For more information about the @X-Frame-Options@ HTTP response header,
    -- see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
    -- in the MDN Web Docs.
    frameOptions :: Prelude.Maybe ResponseHeadersPolicyFrameOptions,
    -- | The policy directives and their values that CloudFront includes as
    -- values for the @Content-Security-Policy@ HTTP response header.
    --
    -- For more information about the @Content-Security-Policy@ HTTP response
    -- header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy Content-Security-Policy>
    -- in the MDN Web Docs.
    contentSecurityPolicy :: Prelude.Maybe ResponseHeadersPolicyContentSecurityPolicy,
    -- | Determines whether CloudFront includes the @Referrer-Policy@ HTTP
    -- response header and the header’s value.
    --
    -- For more information about the @Referrer-Policy@ HTTP response header,
    -- see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
    -- in the MDN Web Docs.
    referrerPolicy :: Prelude.Maybe ResponseHeadersPolicyReferrerPolicy,
    -- | Determines whether CloudFront includes the @Strict-Transport-Security@
    -- HTTP response header and the header’s value.
    --
    -- For more information about the @Strict-Transport-Security@ HTTP response
    -- header, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security Strict-Transport-Security>
    -- in the MDN Web Docs.
    strictTransportSecurity :: Prelude.Maybe ResponseHeadersPolicyStrictTransportSecurity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicySecurityHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentTypeOptions', 'responseHeadersPolicySecurityHeadersConfig_contentTypeOptions' - Determines whether CloudFront includes the @X-Content-Type-Options@ HTTP
-- response header with its value set to @nosniff@.
--
-- For more information about the @X-Content-Type-Options@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options X-Content-Type-Options>
-- in the MDN Web Docs.
--
-- 'xSSProtection', 'responseHeadersPolicySecurityHeadersConfig_xSSProtection' - Determines whether CloudFront includes the @X-XSS-Protection@ HTTP
-- response header and the header’s value.
--
-- For more information about the @X-XSS-Protection@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
--
-- 'frameOptions', 'responseHeadersPolicySecurityHeadersConfig_frameOptions' - Determines whether CloudFront includes the @X-Frame-Options@ HTTP
-- response header and the header’s value.
--
-- For more information about the @X-Frame-Options@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
-- in the MDN Web Docs.
--
-- 'contentSecurityPolicy', 'responseHeadersPolicySecurityHeadersConfig_contentSecurityPolicy' - The policy directives and their values that CloudFront includes as
-- values for the @Content-Security-Policy@ HTTP response header.
--
-- For more information about the @Content-Security-Policy@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy Content-Security-Policy>
-- in the MDN Web Docs.
--
-- 'referrerPolicy', 'responseHeadersPolicySecurityHeadersConfig_referrerPolicy' - Determines whether CloudFront includes the @Referrer-Policy@ HTTP
-- response header and the header’s value.
--
-- For more information about the @Referrer-Policy@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
-- in the MDN Web Docs.
--
-- 'strictTransportSecurity', 'responseHeadersPolicySecurityHeadersConfig_strictTransportSecurity' - Determines whether CloudFront includes the @Strict-Transport-Security@
-- HTTP response header and the header’s value.
--
-- For more information about the @Strict-Transport-Security@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security Strict-Transport-Security>
-- in the MDN Web Docs.
newResponseHeadersPolicySecurityHeadersConfig ::
  ResponseHeadersPolicySecurityHeadersConfig
newResponseHeadersPolicySecurityHeadersConfig =
  ResponseHeadersPolicySecurityHeadersConfig'
    { contentTypeOptions =
        Prelude.Nothing,
      xSSProtection = Prelude.Nothing,
      frameOptions = Prelude.Nothing,
      contentSecurityPolicy =
        Prelude.Nothing,
      referrerPolicy =
        Prelude.Nothing,
      strictTransportSecurity =
        Prelude.Nothing
    }

-- | Determines whether CloudFront includes the @X-Content-Type-Options@ HTTP
-- response header with its value set to @nosniff@.
--
-- For more information about the @X-Content-Type-Options@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options X-Content-Type-Options>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_contentTypeOptions :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyContentTypeOptions)
responseHeadersPolicySecurityHeadersConfig_contentTypeOptions = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {contentTypeOptions} -> contentTypeOptions) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {contentTypeOptions = a} :: ResponseHeadersPolicySecurityHeadersConfig)

-- | Determines whether CloudFront includes the @X-XSS-Protection@ HTTP
-- response header and the header’s value.
--
-- For more information about the @X-XSS-Protection@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_xSSProtection :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyXSSProtection)
responseHeadersPolicySecurityHeadersConfig_xSSProtection = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {xSSProtection} -> xSSProtection) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {xSSProtection = a} :: ResponseHeadersPolicySecurityHeadersConfig)

-- | Determines whether CloudFront includes the @X-Frame-Options@ HTTP
-- response header and the header’s value.
--
-- For more information about the @X-Frame-Options@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_frameOptions :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyFrameOptions)
responseHeadersPolicySecurityHeadersConfig_frameOptions = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {frameOptions} -> frameOptions) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {frameOptions = a} :: ResponseHeadersPolicySecurityHeadersConfig)

-- | The policy directives and their values that CloudFront includes as
-- values for the @Content-Security-Policy@ HTTP response header.
--
-- For more information about the @Content-Security-Policy@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy Content-Security-Policy>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_contentSecurityPolicy :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyContentSecurityPolicy)
responseHeadersPolicySecurityHeadersConfig_contentSecurityPolicy = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {contentSecurityPolicy} -> contentSecurityPolicy) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {contentSecurityPolicy = a} :: ResponseHeadersPolicySecurityHeadersConfig)

-- | Determines whether CloudFront includes the @Referrer-Policy@ HTTP
-- response header and the header’s value.
--
-- For more information about the @Referrer-Policy@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_referrerPolicy :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyReferrerPolicy)
responseHeadersPolicySecurityHeadersConfig_referrerPolicy = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {referrerPolicy} -> referrerPolicy) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {referrerPolicy = a} :: ResponseHeadersPolicySecurityHeadersConfig)

-- | Determines whether CloudFront includes the @Strict-Transport-Security@
-- HTTP response header and the header’s value.
--
-- For more information about the @Strict-Transport-Security@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security Strict-Transport-Security>
-- in the MDN Web Docs.
responseHeadersPolicySecurityHeadersConfig_strictTransportSecurity :: Lens.Lens' ResponseHeadersPolicySecurityHeadersConfig (Prelude.Maybe ResponseHeadersPolicyStrictTransportSecurity)
responseHeadersPolicySecurityHeadersConfig_strictTransportSecurity = Lens.lens (\ResponseHeadersPolicySecurityHeadersConfig' {strictTransportSecurity} -> strictTransportSecurity) (\s@ResponseHeadersPolicySecurityHeadersConfig' {} a -> s {strictTransportSecurity = a} :: ResponseHeadersPolicySecurityHeadersConfig)

instance
  Data.FromXML
    ResponseHeadersPolicySecurityHeadersConfig
  where
  parseXML x =
    ResponseHeadersPolicySecurityHeadersConfig'
      Prelude.<$> (x Data..@? "ContentTypeOptions")
        Prelude.<*> (x Data..@? "XSSProtection")
        Prelude.<*> (x Data..@? "FrameOptions")
        Prelude.<*> (x Data..@? "ContentSecurityPolicy")
        Prelude.<*> (x Data..@? "ReferrerPolicy")
        Prelude.<*> (x Data..@? "StrictTransportSecurity")

instance
  Prelude.Hashable
    ResponseHeadersPolicySecurityHeadersConfig
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicySecurityHeadersConfig' {..} =
      _salt `Prelude.hashWithSalt` contentTypeOptions
        `Prelude.hashWithSalt` xSSProtection
        `Prelude.hashWithSalt` frameOptions
        `Prelude.hashWithSalt` contentSecurityPolicy
        `Prelude.hashWithSalt` referrerPolicy
        `Prelude.hashWithSalt` strictTransportSecurity

instance
  Prelude.NFData
    ResponseHeadersPolicySecurityHeadersConfig
  where
  rnf ResponseHeadersPolicySecurityHeadersConfig' {..} =
    Prelude.rnf contentTypeOptions
      `Prelude.seq` Prelude.rnf xSSProtection
      `Prelude.seq` Prelude.rnf frameOptions
      `Prelude.seq` Prelude.rnf contentSecurityPolicy
      `Prelude.seq` Prelude.rnf referrerPolicy
      `Prelude.seq` Prelude.rnf strictTransportSecurity

instance
  Data.ToXML
    ResponseHeadersPolicySecurityHeadersConfig
  where
  toXML ResponseHeadersPolicySecurityHeadersConfig' {..} =
    Prelude.mconcat
      [ "ContentTypeOptions" Data.@= contentTypeOptions,
        "XSSProtection" Data.@= xSSProtection,
        "FrameOptions" Data.@= frameOptions,
        "ContentSecurityPolicy"
          Data.@= contentSecurityPolicy,
        "ReferrerPolicy" Data.@= referrerPolicy,
        "StrictTransportSecurity"
          Data.@= strictTransportSecurity
      ]
