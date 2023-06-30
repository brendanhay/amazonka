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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyReferrerPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyReferrerPolicy where

import Amazonka.CloudFront.Types.ReferrerPolicyList
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines whether CloudFront includes the @Referrer-Policy@ HTTP
-- response header and the header\'s value.
--
-- For more information about the @Referrer-Policy@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyReferrerPolicy' smart constructor.
data ResponseHeadersPolicyReferrerPolicy = ResponseHeadersPolicyReferrerPolicy'
  { -- | A Boolean that determines whether CloudFront overrides the
    -- @Referrer-Policy@ HTTP response header received from the origin with the
    -- one specified in this response headers policy.
    override :: Prelude.Bool,
    -- | The value of the @Referrer-Policy@ HTTP response header. Valid values
    -- are:
    --
    -- -   @no-referrer@
    --
    -- -   @no-referrer-when-downgrade@
    --
    -- -   @origin@
    --
    -- -   @origin-when-cross-origin@
    --
    -- -   @same-origin@
    --
    -- -   @strict-origin@
    --
    -- -   @strict-origin-when-cross-origin@
    --
    -- -   @unsafe-url@
    --
    -- For more information about these values, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
    -- in the MDN Web Docs.
    referrerPolicy :: ReferrerPolicyList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyReferrerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'responseHeadersPolicyReferrerPolicy_override' - A Boolean that determines whether CloudFront overrides the
-- @Referrer-Policy@ HTTP response header received from the origin with the
-- one specified in this response headers policy.
--
-- 'referrerPolicy', 'responseHeadersPolicyReferrerPolicy_referrerPolicy' - The value of the @Referrer-Policy@ HTTP response header. Valid values
-- are:
--
-- -   @no-referrer@
--
-- -   @no-referrer-when-downgrade@
--
-- -   @origin@
--
-- -   @origin-when-cross-origin@
--
-- -   @same-origin@
--
-- -   @strict-origin@
--
-- -   @strict-origin-when-cross-origin@
--
-- -   @unsafe-url@
--
-- For more information about these values, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
-- in the MDN Web Docs.
newResponseHeadersPolicyReferrerPolicy ::
  -- | 'override'
  Prelude.Bool ->
  -- | 'referrerPolicy'
  ReferrerPolicyList ->
  ResponseHeadersPolicyReferrerPolicy
newResponseHeadersPolicyReferrerPolicy
  pOverride_
  pReferrerPolicy_ =
    ResponseHeadersPolicyReferrerPolicy'
      { override =
          pOverride_,
        referrerPolicy = pReferrerPolicy_
      }

-- | A Boolean that determines whether CloudFront overrides the
-- @Referrer-Policy@ HTTP response header received from the origin with the
-- one specified in this response headers policy.
responseHeadersPolicyReferrerPolicy_override :: Lens.Lens' ResponseHeadersPolicyReferrerPolicy Prelude.Bool
responseHeadersPolicyReferrerPolicy_override = Lens.lens (\ResponseHeadersPolicyReferrerPolicy' {override} -> override) (\s@ResponseHeadersPolicyReferrerPolicy' {} a -> s {override = a} :: ResponseHeadersPolicyReferrerPolicy)

-- | The value of the @Referrer-Policy@ HTTP response header. Valid values
-- are:
--
-- -   @no-referrer@
--
-- -   @no-referrer-when-downgrade@
--
-- -   @origin@
--
-- -   @origin-when-cross-origin@
--
-- -   @same-origin@
--
-- -   @strict-origin@
--
-- -   @strict-origin-when-cross-origin@
--
-- -   @unsafe-url@
--
-- For more information about these values, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy Referrer-Policy>
-- in the MDN Web Docs.
responseHeadersPolicyReferrerPolicy_referrerPolicy :: Lens.Lens' ResponseHeadersPolicyReferrerPolicy ReferrerPolicyList
responseHeadersPolicyReferrerPolicy_referrerPolicy = Lens.lens (\ResponseHeadersPolicyReferrerPolicy' {referrerPolicy} -> referrerPolicy) (\s@ResponseHeadersPolicyReferrerPolicy' {} a -> s {referrerPolicy = a} :: ResponseHeadersPolicyReferrerPolicy)

instance
  Data.FromXML
    ResponseHeadersPolicyReferrerPolicy
  where
  parseXML x =
    ResponseHeadersPolicyReferrerPolicy'
      Prelude.<$> (x Data..@ "Override")
      Prelude.<*> (x Data..@ "ReferrerPolicy")

instance
  Prelude.Hashable
    ResponseHeadersPolicyReferrerPolicy
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyReferrerPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` override
        `Prelude.hashWithSalt` referrerPolicy

instance
  Prelude.NFData
    ResponseHeadersPolicyReferrerPolicy
  where
  rnf ResponseHeadersPolicyReferrerPolicy' {..} =
    Prelude.rnf override
      `Prelude.seq` Prelude.rnf referrerPolicy

instance
  Data.ToXML
    ResponseHeadersPolicyReferrerPolicy
  where
  toXML ResponseHeadersPolicyReferrerPolicy' {..} =
    Prelude.mconcat
      [ "Override" Data.@= override,
        "ReferrerPolicy" Data.@= referrerPolicy
      ]
