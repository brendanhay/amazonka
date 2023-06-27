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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyContentSecurityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyContentSecurityPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The policy directives and their values that CloudFront includes as
-- values for the @Content-Security-Policy@ HTTP response header.
--
-- For more information about the @Content-Security-Policy@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy Content-Security-Policy>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyContentSecurityPolicy' smart constructor.
data ResponseHeadersPolicyContentSecurityPolicy = ResponseHeadersPolicyContentSecurityPolicy'
  { -- | A Boolean that determines whether CloudFront overrides the
    -- @Content-Security-Policy@ HTTP response header received from the origin
    -- with the one specified in this response headers policy.
    override :: Prelude.Bool,
    -- | The policy directives and their values that CloudFront includes as
    -- values for the @Content-Security-Policy@ HTTP response header.
    contentSecurityPolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyContentSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'responseHeadersPolicyContentSecurityPolicy_override' - A Boolean that determines whether CloudFront overrides the
-- @Content-Security-Policy@ HTTP response header received from the origin
-- with the one specified in this response headers policy.
--
-- 'contentSecurityPolicy', 'responseHeadersPolicyContentSecurityPolicy_contentSecurityPolicy' - The policy directives and their values that CloudFront includes as
-- values for the @Content-Security-Policy@ HTTP response header.
newResponseHeadersPolicyContentSecurityPolicy ::
  -- | 'override'
  Prelude.Bool ->
  -- | 'contentSecurityPolicy'
  Prelude.Text ->
  ResponseHeadersPolicyContentSecurityPolicy
newResponseHeadersPolicyContentSecurityPolicy
  pOverride_
  pContentSecurityPolicy_ =
    ResponseHeadersPolicyContentSecurityPolicy'
      { override =
          pOverride_,
        contentSecurityPolicy =
          pContentSecurityPolicy_
      }

-- | A Boolean that determines whether CloudFront overrides the
-- @Content-Security-Policy@ HTTP response header received from the origin
-- with the one specified in this response headers policy.
responseHeadersPolicyContentSecurityPolicy_override :: Lens.Lens' ResponseHeadersPolicyContentSecurityPolicy Prelude.Bool
responseHeadersPolicyContentSecurityPolicy_override = Lens.lens (\ResponseHeadersPolicyContentSecurityPolicy' {override} -> override) (\s@ResponseHeadersPolicyContentSecurityPolicy' {} a -> s {override = a} :: ResponseHeadersPolicyContentSecurityPolicy)

-- | The policy directives and their values that CloudFront includes as
-- values for the @Content-Security-Policy@ HTTP response header.
responseHeadersPolicyContentSecurityPolicy_contentSecurityPolicy :: Lens.Lens' ResponseHeadersPolicyContentSecurityPolicy Prelude.Text
responseHeadersPolicyContentSecurityPolicy_contentSecurityPolicy = Lens.lens (\ResponseHeadersPolicyContentSecurityPolicy' {contentSecurityPolicy} -> contentSecurityPolicy) (\s@ResponseHeadersPolicyContentSecurityPolicy' {} a -> s {contentSecurityPolicy = a} :: ResponseHeadersPolicyContentSecurityPolicy)

instance
  Data.FromXML
    ResponseHeadersPolicyContentSecurityPolicy
  where
  parseXML x =
    ResponseHeadersPolicyContentSecurityPolicy'
      Prelude.<$> (x Data..@ "Override")
      Prelude.<*> (x Data..@ "ContentSecurityPolicy")

instance
  Prelude.Hashable
    ResponseHeadersPolicyContentSecurityPolicy
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyContentSecurityPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` override
        `Prelude.hashWithSalt` contentSecurityPolicy

instance
  Prelude.NFData
    ResponseHeadersPolicyContentSecurityPolicy
  where
  rnf ResponseHeadersPolicyContentSecurityPolicy' {..} =
    Prelude.rnf override
      `Prelude.seq` Prelude.rnf contentSecurityPolicy

instance
  Data.ToXML
    ResponseHeadersPolicyContentSecurityPolicy
  where
  toXML ResponseHeadersPolicyContentSecurityPolicy' {..} =
    Prelude.mconcat
      [ "Override" Data.@= override,
        "ContentSecurityPolicy"
          Data.@= contentSecurityPolicy
      ]
