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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyFrameOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyFrameOptions where

import Amazonka.CloudFront.Types.FrameOptionsList
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines whether CloudFront includes the @X-Frame-Options@ HTTP
-- response header and the header\'s value.
--
-- For more information about the @X-Frame-Options@ HTTP response header,
-- see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyFrameOptions' smart constructor.
data ResponseHeadersPolicyFrameOptions = ResponseHeadersPolicyFrameOptions'
  { -- | A Boolean that determines whether CloudFront overrides the
    -- @X-Frame-Options@ HTTP response header received from the origin with the
    -- one specified in this response headers policy.
    override :: Prelude.Bool,
    -- | The value of the @X-Frame-Options@ HTTP response header. Valid values
    -- are @DENY@ and @SAMEORIGIN@.
    --
    -- For more information about these values, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
    -- in the MDN Web Docs.
    frameOption :: FrameOptionsList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyFrameOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'responseHeadersPolicyFrameOptions_override' - A Boolean that determines whether CloudFront overrides the
-- @X-Frame-Options@ HTTP response header received from the origin with the
-- one specified in this response headers policy.
--
-- 'frameOption', 'responseHeadersPolicyFrameOptions_frameOption' - The value of the @X-Frame-Options@ HTTP response header. Valid values
-- are @DENY@ and @SAMEORIGIN@.
--
-- For more information about these values, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
-- in the MDN Web Docs.
newResponseHeadersPolicyFrameOptions ::
  -- | 'override'
  Prelude.Bool ->
  -- | 'frameOption'
  FrameOptionsList ->
  ResponseHeadersPolicyFrameOptions
newResponseHeadersPolicyFrameOptions
  pOverride_
  pFrameOption_ =
    ResponseHeadersPolicyFrameOptions'
      { override =
          pOverride_,
        frameOption = pFrameOption_
      }

-- | A Boolean that determines whether CloudFront overrides the
-- @X-Frame-Options@ HTTP response header received from the origin with the
-- one specified in this response headers policy.
responseHeadersPolicyFrameOptions_override :: Lens.Lens' ResponseHeadersPolicyFrameOptions Prelude.Bool
responseHeadersPolicyFrameOptions_override = Lens.lens (\ResponseHeadersPolicyFrameOptions' {override} -> override) (\s@ResponseHeadersPolicyFrameOptions' {} a -> s {override = a} :: ResponseHeadersPolicyFrameOptions)

-- | The value of the @X-Frame-Options@ HTTP response header. Valid values
-- are @DENY@ and @SAMEORIGIN@.
--
-- For more information about these values, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options X-Frame-Options>
-- in the MDN Web Docs.
responseHeadersPolicyFrameOptions_frameOption :: Lens.Lens' ResponseHeadersPolicyFrameOptions FrameOptionsList
responseHeadersPolicyFrameOptions_frameOption = Lens.lens (\ResponseHeadersPolicyFrameOptions' {frameOption} -> frameOption) (\s@ResponseHeadersPolicyFrameOptions' {} a -> s {frameOption = a} :: ResponseHeadersPolicyFrameOptions)

instance
  Data.FromXML
    ResponseHeadersPolicyFrameOptions
  where
  parseXML x =
    ResponseHeadersPolicyFrameOptions'
      Prelude.<$> (x Data..@ "Override")
      Prelude.<*> (x Data..@ "FrameOption")

instance
  Prelude.Hashable
    ResponseHeadersPolicyFrameOptions
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyFrameOptions' {..} =
      _salt
        `Prelude.hashWithSalt` override
        `Prelude.hashWithSalt` frameOption

instance
  Prelude.NFData
    ResponseHeadersPolicyFrameOptions
  where
  rnf ResponseHeadersPolicyFrameOptions' {..} =
    Prelude.rnf override
      `Prelude.seq` Prelude.rnf frameOption

instance Data.ToXML ResponseHeadersPolicyFrameOptions where
  toXML ResponseHeadersPolicyFrameOptions' {..} =
    Prelude.mconcat
      [ "Override" Data.@= override,
        "FrameOption" Data.@= frameOption
      ]
