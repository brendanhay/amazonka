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
-- Module      : Amazonka.WAFV2.Types.Headers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Headers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.HeaderMatchPattern
import Amazonka.WAFV2.Types.MapMatchScope
import Amazonka.WAFV2.Types.OversizeHandling

-- | Inspect all headers in the web request. You can specify the parts of the
-- headers to inspect and you can narrow the set of headers to inspect by
-- including or excluding specific keys.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- If you want to inspect just the value of a single header, use the
-- @SingleHeader@ @FieldToMatch@ setting instead.
--
-- Example JSON:
-- @\"Headers\": { \"MatchPattern\": { \"All\": {} }, \"MatchScope\": \"KEY\", \"OversizeHandling\": \"MATCH\" }@
--
-- /See:/ 'newHeaders' smart constructor.
data Headers = Headers'
  { -- | The filter to use to identify the subset of headers to inspect in a web
    -- request.
    --
    -- You must specify exactly one setting: either @All@, @IncludedHeaders@,
    -- or @ExcludedHeaders@.
    --
    -- Example JSON:
    -- @\"MatchPattern\": { \"ExcludedHeaders\": {\"KeyToExclude1\", \"KeyToExclude2\"} }@
    matchPattern :: HeaderMatchPattern,
    -- | The parts of the headers to match with the rule inspection criteria. If
    -- you specify @All@, WAF inspects both keys and values.
    matchScope :: MapMatchScope,
    -- | What WAF should do if the headers of the request are larger than WAF can
    -- inspect. WAF does not support inspecting the entire contents of request
    -- headers when they exceed 8 KB (8192 bytes) or 200 total headers. The
    -- underlying host service forwards a maximum of 200 headers and at most 8
    -- KB of header contents to WAF.
    --
    -- The options for oversize handling are the following:
    --
    -- -   @CONTINUE@ - Inspect the headers normally, according to the rule
    --     inspection criteria.
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    oversizeHandling :: OversizeHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Headers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchPattern', 'headers_matchPattern' - The filter to use to identify the subset of headers to inspect in a web
-- request.
--
-- You must specify exactly one setting: either @All@, @IncludedHeaders@,
-- or @ExcludedHeaders@.
--
-- Example JSON:
-- @\"MatchPattern\": { \"ExcludedHeaders\": {\"KeyToExclude1\", \"KeyToExclude2\"} }@
--
-- 'matchScope', 'headers_matchScope' - The parts of the headers to match with the rule inspection criteria. If
-- you specify @All@, WAF inspects both keys and values.
--
-- 'oversizeHandling', 'headers_oversizeHandling' - What WAF should do if the headers of the request are larger than WAF can
-- inspect. WAF does not support inspecting the entire contents of request
-- headers when they exceed 8 KB (8192 bytes) or 200 total headers. The
-- underlying host service forwards a maximum of 200 headers and at most 8
-- KB of header contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the headers normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
newHeaders ::
  -- | 'matchPattern'
  HeaderMatchPattern ->
  -- | 'matchScope'
  MapMatchScope ->
  -- | 'oversizeHandling'
  OversizeHandling ->
  Headers
newHeaders
  pMatchPattern_
  pMatchScope_
  pOversizeHandling_ =
    Headers'
      { matchPattern = pMatchPattern_,
        matchScope = pMatchScope_,
        oversizeHandling = pOversizeHandling_
      }

-- | The filter to use to identify the subset of headers to inspect in a web
-- request.
--
-- You must specify exactly one setting: either @All@, @IncludedHeaders@,
-- or @ExcludedHeaders@.
--
-- Example JSON:
-- @\"MatchPattern\": { \"ExcludedHeaders\": {\"KeyToExclude1\", \"KeyToExclude2\"} }@
headers_matchPattern :: Lens.Lens' Headers HeaderMatchPattern
headers_matchPattern = Lens.lens (\Headers' {matchPattern} -> matchPattern) (\s@Headers' {} a -> s {matchPattern = a} :: Headers)

-- | The parts of the headers to match with the rule inspection criteria. If
-- you specify @All@, WAF inspects both keys and values.
headers_matchScope :: Lens.Lens' Headers MapMatchScope
headers_matchScope = Lens.lens (\Headers' {matchScope} -> matchScope) (\s@Headers' {} a -> s {matchScope = a} :: Headers)

-- | What WAF should do if the headers of the request are larger than WAF can
-- inspect. WAF does not support inspecting the entire contents of request
-- headers when they exceed 8 KB (8192 bytes) or 200 total headers. The
-- underlying host service forwards a maximum of 200 headers and at most 8
-- KB of header contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the headers normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
headers_oversizeHandling :: Lens.Lens' Headers OversizeHandling
headers_oversizeHandling = Lens.lens (\Headers' {oversizeHandling} -> oversizeHandling) (\s@Headers' {} a -> s {oversizeHandling = a} :: Headers)

instance Data.FromJSON Headers where
  parseJSON =
    Data.withObject
      "Headers"
      ( \x ->
          Headers'
            Prelude.<$> (x Data..: "MatchPattern")
            Prelude.<*> (x Data..: "MatchScope")
            Prelude.<*> (x Data..: "OversizeHandling")
      )

instance Prelude.Hashable Headers where
  hashWithSalt _salt Headers' {..} =
    _salt `Prelude.hashWithSalt` matchPattern
      `Prelude.hashWithSalt` matchScope
      `Prelude.hashWithSalt` oversizeHandling

instance Prelude.NFData Headers where
  rnf Headers' {..} =
    Prelude.rnf matchPattern
      `Prelude.seq` Prelude.rnf matchScope
      `Prelude.seq` Prelude.rnf oversizeHandling

instance Data.ToJSON Headers where
  toJSON Headers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MatchPattern" Data..= matchPattern),
            Prelude.Just ("MatchScope" Data..= matchScope),
            Prelude.Just
              ("OversizeHandling" Data..= oversizeHandling)
          ]
      )
