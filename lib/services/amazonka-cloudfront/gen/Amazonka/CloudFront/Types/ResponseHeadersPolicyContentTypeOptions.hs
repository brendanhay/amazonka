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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyContentTypeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyContentTypeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines whether CloudFront includes the @X-Content-Type-Options@ HTTP
-- response header with its value set to @nosniff@.
--
-- For more information about the @X-Content-Type-Options@ HTTP response
-- header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options X-Content-Type-Options>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyContentTypeOptions' smart constructor.
data ResponseHeadersPolicyContentTypeOptions = ResponseHeadersPolicyContentTypeOptions'
  { -- | A Boolean that determines whether CloudFront overrides the
    -- @X-Content-Type-Options@ HTTP response header received from the origin
    -- with the one specified in this response headers policy.
    override :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyContentTypeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'responseHeadersPolicyContentTypeOptions_override' - A Boolean that determines whether CloudFront overrides the
-- @X-Content-Type-Options@ HTTP response header received from the origin
-- with the one specified in this response headers policy.
newResponseHeadersPolicyContentTypeOptions ::
  -- | 'override'
  Prelude.Bool ->
  ResponseHeadersPolicyContentTypeOptions
newResponseHeadersPolicyContentTypeOptions pOverride_ =
  ResponseHeadersPolicyContentTypeOptions'
    { override =
        pOverride_
    }

-- | A Boolean that determines whether CloudFront overrides the
-- @X-Content-Type-Options@ HTTP response header received from the origin
-- with the one specified in this response headers policy.
responseHeadersPolicyContentTypeOptions_override :: Lens.Lens' ResponseHeadersPolicyContentTypeOptions Prelude.Bool
responseHeadersPolicyContentTypeOptions_override = Lens.lens (\ResponseHeadersPolicyContentTypeOptions' {override} -> override) (\s@ResponseHeadersPolicyContentTypeOptions' {} a -> s {override = a} :: ResponseHeadersPolicyContentTypeOptions)

instance
  Data.FromXML
    ResponseHeadersPolicyContentTypeOptions
  where
  parseXML x =
    ResponseHeadersPolicyContentTypeOptions'
      Prelude.<$> (x Data..@ "Override")

instance
  Prelude.Hashable
    ResponseHeadersPolicyContentTypeOptions
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyContentTypeOptions' {..} =
      _salt `Prelude.hashWithSalt` override

instance
  Prelude.NFData
    ResponseHeadersPolicyContentTypeOptions
  where
  rnf ResponseHeadersPolicyContentTypeOptions' {..} =
    Prelude.rnf override

instance
  Data.ToXML
    ResponseHeadersPolicyContentTypeOptions
  where
  toXML ResponseHeadersPolicyContentTypeOptions' {..} =
    Prelude.mconcat ["Override" Data.@= override]
