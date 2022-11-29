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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyStrictTransportSecurity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyStrictTransportSecurity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Determines whether CloudFront includes the @Strict-Transport-Security@
-- HTTP response header and the header’s value.
--
-- For more information about the @Strict-Transport-Security@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security Strict-Transport-Security>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyStrictTransportSecurity' smart constructor.
data ResponseHeadersPolicyStrictTransportSecurity = ResponseHeadersPolicyStrictTransportSecurity'
  { -- | A Boolean that determines whether CloudFront includes the
    -- @includeSubDomains@ directive in the @Strict-Transport-Security@ HTTP
    -- response header.
    includeSubdomains :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean that determines whether CloudFront includes the @preload@
    -- directive in the @Strict-Transport-Security@ HTTP response header.
    preload :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean that determines whether CloudFront overrides the
    -- @Strict-Transport-Security@ HTTP response header received from the
    -- origin with the one specified in this response headers policy.
    override :: Prelude.Bool,
    -- | A number that CloudFront uses as the value for the @max-age@ directive
    -- in the @Strict-Transport-Security@ HTTP response header.
    accessControlMaxAgeSec :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyStrictTransportSecurity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeSubdomains', 'responseHeadersPolicyStrictTransportSecurity_includeSubdomains' - A Boolean that determines whether CloudFront includes the
-- @includeSubDomains@ directive in the @Strict-Transport-Security@ HTTP
-- response header.
--
-- 'preload', 'responseHeadersPolicyStrictTransportSecurity_preload' - A Boolean that determines whether CloudFront includes the @preload@
-- directive in the @Strict-Transport-Security@ HTTP response header.
--
-- 'override', 'responseHeadersPolicyStrictTransportSecurity_override' - A Boolean that determines whether CloudFront overrides the
-- @Strict-Transport-Security@ HTTP response header received from the
-- origin with the one specified in this response headers policy.
--
-- 'accessControlMaxAgeSec', 'responseHeadersPolicyStrictTransportSecurity_accessControlMaxAgeSec' - A number that CloudFront uses as the value for the @max-age@ directive
-- in the @Strict-Transport-Security@ HTTP response header.
newResponseHeadersPolicyStrictTransportSecurity ::
  -- | 'override'
  Prelude.Bool ->
  -- | 'accessControlMaxAgeSec'
  Prelude.Int ->
  ResponseHeadersPolicyStrictTransportSecurity
newResponseHeadersPolicyStrictTransportSecurity
  pOverride_
  pAccessControlMaxAgeSec_ =
    ResponseHeadersPolicyStrictTransportSecurity'
      { includeSubdomains =
          Prelude.Nothing,
        preload = Prelude.Nothing,
        override = pOverride_,
        accessControlMaxAgeSec =
          pAccessControlMaxAgeSec_
      }

-- | A Boolean that determines whether CloudFront includes the
-- @includeSubDomains@ directive in the @Strict-Transport-Security@ HTTP
-- response header.
responseHeadersPolicyStrictTransportSecurity_includeSubdomains :: Lens.Lens' ResponseHeadersPolicyStrictTransportSecurity (Prelude.Maybe Prelude.Bool)
responseHeadersPolicyStrictTransportSecurity_includeSubdomains = Lens.lens (\ResponseHeadersPolicyStrictTransportSecurity' {includeSubdomains} -> includeSubdomains) (\s@ResponseHeadersPolicyStrictTransportSecurity' {} a -> s {includeSubdomains = a} :: ResponseHeadersPolicyStrictTransportSecurity)

-- | A Boolean that determines whether CloudFront includes the @preload@
-- directive in the @Strict-Transport-Security@ HTTP response header.
responseHeadersPolicyStrictTransportSecurity_preload :: Lens.Lens' ResponseHeadersPolicyStrictTransportSecurity (Prelude.Maybe Prelude.Bool)
responseHeadersPolicyStrictTransportSecurity_preload = Lens.lens (\ResponseHeadersPolicyStrictTransportSecurity' {preload} -> preload) (\s@ResponseHeadersPolicyStrictTransportSecurity' {} a -> s {preload = a} :: ResponseHeadersPolicyStrictTransportSecurity)

-- | A Boolean that determines whether CloudFront overrides the
-- @Strict-Transport-Security@ HTTP response header received from the
-- origin with the one specified in this response headers policy.
responseHeadersPolicyStrictTransportSecurity_override :: Lens.Lens' ResponseHeadersPolicyStrictTransportSecurity Prelude.Bool
responseHeadersPolicyStrictTransportSecurity_override = Lens.lens (\ResponseHeadersPolicyStrictTransportSecurity' {override} -> override) (\s@ResponseHeadersPolicyStrictTransportSecurity' {} a -> s {override = a} :: ResponseHeadersPolicyStrictTransportSecurity)

-- | A number that CloudFront uses as the value for the @max-age@ directive
-- in the @Strict-Transport-Security@ HTTP response header.
responseHeadersPolicyStrictTransportSecurity_accessControlMaxAgeSec :: Lens.Lens' ResponseHeadersPolicyStrictTransportSecurity Prelude.Int
responseHeadersPolicyStrictTransportSecurity_accessControlMaxAgeSec = Lens.lens (\ResponseHeadersPolicyStrictTransportSecurity' {accessControlMaxAgeSec} -> accessControlMaxAgeSec) (\s@ResponseHeadersPolicyStrictTransportSecurity' {} a -> s {accessControlMaxAgeSec = a} :: ResponseHeadersPolicyStrictTransportSecurity)

instance
  Core.FromXML
    ResponseHeadersPolicyStrictTransportSecurity
  where
  parseXML x =
    ResponseHeadersPolicyStrictTransportSecurity'
      Prelude.<$> (x Core..@? "IncludeSubdomains")
        Prelude.<*> (x Core..@? "Preload")
        Prelude.<*> (x Core..@ "Override")
        Prelude.<*> (x Core..@ "AccessControlMaxAgeSec")

instance
  Prelude.Hashable
    ResponseHeadersPolicyStrictTransportSecurity
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyStrictTransportSecurity' {..} =
      _salt `Prelude.hashWithSalt` includeSubdomains
        `Prelude.hashWithSalt` preload
        `Prelude.hashWithSalt` override
        `Prelude.hashWithSalt` accessControlMaxAgeSec

instance
  Prelude.NFData
    ResponseHeadersPolicyStrictTransportSecurity
  where
  rnf ResponseHeadersPolicyStrictTransportSecurity' {..} =
    Prelude.rnf includeSubdomains
      `Prelude.seq` Prelude.rnf preload
      `Prelude.seq` Prelude.rnf override
      `Prelude.seq` Prelude.rnf accessControlMaxAgeSec

instance
  Core.ToXML
    ResponseHeadersPolicyStrictTransportSecurity
  where
  toXML
    ResponseHeadersPolicyStrictTransportSecurity' {..} =
      Prelude.mconcat
        [ "IncludeSubdomains" Core.@= includeSubdomains,
          "Preload" Core.@= preload,
          "Override" Core.@= override,
          "AccessControlMaxAgeSec"
            Core.@= accessControlMaxAgeSec
        ]
