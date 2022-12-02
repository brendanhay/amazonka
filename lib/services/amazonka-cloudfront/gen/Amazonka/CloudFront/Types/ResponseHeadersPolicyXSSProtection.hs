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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyXSSProtection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyXSSProtection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines whether CloudFront includes the @X-XSS-Protection@ HTTP
-- response header and the header’s value.
--
-- For more information about the @X-XSS-Protection@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyXSSProtection' smart constructor.
data ResponseHeadersPolicyXSSProtection = ResponseHeadersPolicyXSSProtection'
  { -- | A Boolean that determines whether CloudFront includes the @mode=block@
    -- directive in the @X-XSS-Protection@ header.
    --
    -- For more information about this directive, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
    -- in the MDN Web Docs.
    modeBlock :: Prelude.Maybe Prelude.Bool,
    -- | A reporting URI, which CloudFront uses as the value of the @report@
    -- directive in the @X-XSS-Protection@ header.
    --
    -- You cannot specify a @ReportUri@ when @ModeBlock@ is @true@.
    --
    -- For more information about using a reporting URL, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
    -- in the MDN Web Docs.
    reportUri :: Prelude.Maybe Prelude.Text,
    -- | A Boolean that determines whether CloudFront overrides the
    -- @X-XSS-Protection@ HTTP response header received from the origin with
    -- the one specified in this response headers policy.
    override :: Prelude.Bool,
    -- | A Boolean that determines the value of the @X-XSS-Protection@ HTTP
    -- response header. When this setting is @true@, the value of the
    -- @X-XSS-Protection@ header is @1@. When this setting is @false@, the
    -- value of the @X-XSS-Protection@ header is @0@.
    --
    -- For more information about these settings, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
    -- in the MDN Web Docs.
    protection :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyXSSProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modeBlock', 'responseHeadersPolicyXSSProtection_modeBlock' - A Boolean that determines whether CloudFront includes the @mode=block@
-- directive in the @X-XSS-Protection@ header.
--
-- For more information about this directive, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
--
-- 'reportUri', 'responseHeadersPolicyXSSProtection_reportUri' - A reporting URI, which CloudFront uses as the value of the @report@
-- directive in the @X-XSS-Protection@ header.
--
-- You cannot specify a @ReportUri@ when @ModeBlock@ is @true@.
--
-- For more information about using a reporting URL, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
--
-- 'override', 'responseHeadersPolicyXSSProtection_override' - A Boolean that determines whether CloudFront overrides the
-- @X-XSS-Protection@ HTTP response header received from the origin with
-- the one specified in this response headers policy.
--
-- 'protection', 'responseHeadersPolicyXSSProtection_protection' - A Boolean that determines the value of the @X-XSS-Protection@ HTTP
-- response header. When this setting is @true@, the value of the
-- @X-XSS-Protection@ header is @1@. When this setting is @false@, the
-- value of the @X-XSS-Protection@ header is @0@.
--
-- For more information about these settings, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
newResponseHeadersPolicyXSSProtection ::
  -- | 'override'
  Prelude.Bool ->
  -- | 'protection'
  Prelude.Bool ->
  ResponseHeadersPolicyXSSProtection
newResponseHeadersPolicyXSSProtection
  pOverride_
  pProtection_ =
    ResponseHeadersPolicyXSSProtection'
      { modeBlock =
          Prelude.Nothing,
        reportUri = Prelude.Nothing,
        override = pOverride_,
        protection = pProtection_
      }

-- | A Boolean that determines whether CloudFront includes the @mode=block@
-- directive in the @X-XSS-Protection@ header.
--
-- For more information about this directive, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
responseHeadersPolicyXSSProtection_modeBlock :: Lens.Lens' ResponseHeadersPolicyXSSProtection (Prelude.Maybe Prelude.Bool)
responseHeadersPolicyXSSProtection_modeBlock = Lens.lens (\ResponseHeadersPolicyXSSProtection' {modeBlock} -> modeBlock) (\s@ResponseHeadersPolicyXSSProtection' {} a -> s {modeBlock = a} :: ResponseHeadersPolicyXSSProtection)

-- | A reporting URI, which CloudFront uses as the value of the @report@
-- directive in the @X-XSS-Protection@ header.
--
-- You cannot specify a @ReportUri@ when @ModeBlock@ is @true@.
--
-- For more information about using a reporting URL, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
responseHeadersPolicyXSSProtection_reportUri :: Lens.Lens' ResponseHeadersPolicyXSSProtection (Prelude.Maybe Prelude.Text)
responseHeadersPolicyXSSProtection_reportUri = Lens.lens (\ResponseHeadersPolicyXSSProtection' {reportUri} -> reportUri) (\s@ResponseHeadersPolicyXSSProtection' {} a -> s {reportUri = a} :: ResponseHeadersPolicyXSSProtection)

-- | A Boolean that determines whether CloudFront overrides the
-- @X-XSS-Protection@ HTTP response header received from the origin with
-- the one specified in this response headers policy.
responseHeadersPolicyXSSProtection_override :: Lens.Lens' ResponseHeadersPolicyXSSProtection Prelude.Bool
responseHeadersPolicyXSSProtection_override = Lens.lens (\ResponseHeadersPolicyXSSProtection' {override} -> override) (\s@ResponseHeadersPolicyXSSProtection' {} a -> s {override = a} :: ResponseHeadersPolicyXSSProtection)

-- | A Boolean that determines the value of the @X-XSS-Protection@ HTTP
-- response header. When this setting is @true@, the value of the
-- @X-XSS-Protection@ header is @1@. When this setting is @false@, the
-- value of the @X-XSS-Protection@ header is @0@.
--
-- For more information about these settings, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection X-XSS-Protection>
-- in the MDN Web Docs.
responseHeadersPolicyXSSProtection_protection :: Lens.Lens' ResponseHeadersPolicyXSSProtection Prelude.Bool
responseHeadersPolicyXSSProtection_protection = Lens.lens (\ResponseHeadersPolicyXSSProtection' {protection} -> protection) (\s@ResponseHeadersPolicyXSSProtection' {} a -> s {protection = a} :: ResponseHeadersPolicyXSSProtection)

instance
  Data.FromXML
    ResponseHeadersPolicyXSSProtection
  where
  parseXML x =
    ResponseHeadersPolicyXSSProtection'
      Prelude.<$> (x Data..@? "ModeBlock")
      Prelude.<*> (x Data..@? "ReportUri")
      Prelude.<*> (x Data..@ "Override")
      Prelude.<*> (x Data..@ "Protection")

instance
  Prelude.Hashable
    ResponseHeadersPolicyXSSProtection
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyXSSProtection' {..} =
      _salt `Prelude.hashWithSalt` modeBlock
        `Prelude.hashWithSalt` reportUri
        `Prelude.hashWithSalt` override
        `Prelude.hashWithSalt` protection

instance
  Prelude.NFData
    ResponseHeadersPolicyXSSProtection
  where
  rnf ResponseHeadersPolicyXSSProtection' {..} =
    Prelude.rnf modeBlock
      `Prelude.seq` Prelude.rnf reportUri
      `Prelude.seq` Prelude.rnf override
      `Prelude.seq` Prelude.rnf protection

instance
  Data.ToXML
    ResponseHeadersPolicyXSSProtection
  where
  toXML ResponseHeadersPolicyXSSProtection' {..} =
    Prelude.mconcat
      [ "ModeBlock" Data.@= modeBlock,
        "ReportUri" Data.@= reportUri,
        "Override" Data.@= override,
        "Protection" Data.@= protection
      ]
