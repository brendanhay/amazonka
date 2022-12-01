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
-- Module      : Amazonka.WAFV2.Types.Cookies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Cookies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CookieMatchPattern
import Amazonka.WAFV2.Types.MapMatchScope
import Amazonka.WAFV2.Types.OversizeHandling

-- | Inspect the cookies in the web request. You can specify the parts of the
-- cookies to inspect and you can narrow the set of cookies to inspect by
-- including or excluding specific keys.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- Example JSON:
-- @\"Cookies\": { \"MatchPattern\": { \"All\": {} }, \"MatchScope\": \"KEY\", \"OversizeHandling\": \"MATCH\" }@
--
-- /See:/ 'newCookies' smart constructor.
data Cookies = Cookies'
  { -- | The filter to use to identify the subset of cookies to inspect in a web
    -- request.
    --
    -- You must specify exactly one setting: either @All@, @IncludedCookies@,
    -- or @ExcludedCookies@.
    --
    -- Example JSON:
    -- @\"MatchPattern\": { \"IncludedCookies\": {\"KeyToInclude1\", \"KeyToInclude2\", \"KeyToInclude3\"} }@
    matchPattern :: CookieMatchPattern,
    -- | The parts of the cookies to inspect with the rule inspection criteria.
    -- If you specify @All@, WAF inspects both keys and values.
    matchScope :: MapMatchScope,
    -- | What WAF should do if the cookies of the request are larger than WAF can
    -- inspect. WAF does not support inspecting the entire contents of request
    -- cookies when they exceed 8 KB (8192 bytes) or 200 total cookies. The
    -- underlying host service forwards a maximum of 200 cookies and at most 8
    -- KB of cookie contents to WAF.
    --
    -- The options for oversize handling are the following:
    --
    -- -   @CONTINUE@ - Inspect the cookies normally, according to the rule
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
-- Create a value of 'Cookies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchPattern', 'cookies_matchPattern' - The filter to use to identify the subset of cookies to inspect in a web
-- request.
--
-- You must specify exactly one setting: either @All@, @IncludedCookies@,
-- or @ExcludedCookies@.
--
-- Example JSON:
-- @\"MatchPattern\": { \"IncludedCookies\": {\"KeyToInclude1\", \"KeyToInclude2\", \"KeyToInclude3\"} }@
--
-- 'matchScope', 'cookies_matchScope' - The parts of the cookies to inspect with the rule inspection criteria.
-- If you specify @All@, WAF inspects both keys and values.
--
-- 'oversizeHandling', 'cookies_oversizeHandling' - What WAF should do if the cookies of the request are larger than WAF can
-- inspect. WAF does not support inspecting the entire contents of request
-- cookies when they exceed 8 KB (8192 bytes) or 200 total cookies. The
-- underlying host service forwards a maximum of 200 cookies and at most 8
-- KB of cookie contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the cookies normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
newCookies ::
  -- | 'matchPattern'
  CookieMatchPattern ->
  -- | 'matchScope'
  MapMatchScope ->
  -- | 'oversizeHandling'
  OversizeHandling ->
  Cookies
newCookies
  pMatchPattern_
  pMatchScope_
  pOversizeHandling_ =
    Cookies'
      { matchPattern = pMatchPattern_,
        matchScope = pMatchScope_,
        oversizeHandling = pOversizeHandling_
      }

-- | The filter to use to identify the subset of cookies to inspect in a web
-- request.
--
-- You must specify exactly one setting: either @All@, @IncludedCookies@,
-- or @ExcludedCookies@.
--
-- Example JSON:
-- @\"MatchPattern\": { \"IncludedCookies\": {\"KeyToInclude1\", \"KeyToInclude2\", \"KeyToInclude3\"} }@
cookies_matchPattern :: Lens.Lens' Cookies CookieMatchPattern
cookies_matchPattern = Lens.lens (\Cookies' {matchPattern} -> matchPattern) (\s@Cookies' {} a -> s {matchPattern = a} :: Cookies)

-- | The parts of the cookies to inspect with the rule inspection criteria.
-- If you specify @All@, WAF inspects both keys and values.
cookies_matchScope :: Lens.Lens' Cookies MapMatchScope
cookies_matchScope = Lens.lens (\Cookies' {matchScope} -> matchScope) (\s@Cookies' {} a -> s {matchScope = a} :: Cookies)

-- | What WAF should do if the cookies of the request are larger than WAF can
-- inspect. WAF does not support inspecting the entire contents of request
-- cookies when they exceed 8 KB (8192 bytes) or 200 total cookies. The
-- underlying host service forwards a maximum of 200 cookies and at most 8
-- KB of cookie contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the cookies normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
cookies_oversizeHandling :: Lens.Lens' Cookies OversizeHandling
cookies_oversizeHandling = Lens.lens (\Cookies' {oversizeHandling} -> oversizeHandling) (\s@Cookies' {} a -> s {oversizeHandling = a} :: Cookies)

instance Core.FromJSON Cookies where
  parseJSON =
    Core.withObject
      "Cookies"
      ( \x ->
          Cookies'
            Prelude.<$> (x Core..: "MatchPattern")
            Prelude.<*> (x Core..: "MatchScope")
            Prelude.<*> (x Core..: "OversizeHandling")
      )

instance Prelude.Hashable Cookies where
  hashWithSalt _salt Cookies' {..} =
    _salt `Prelude.hashWithSalt` matchPattern
      `Prelude.hashWithSalt` matchScope
      `Prelude.hashWithSalt` oversizeHandling

instance Prelude.NFData Cookies where
  rnf Cookies' {..} =
    Prelude.rnf matchPattern
      `Prelude.seq` Prelude.rnf matchScope
      `Prelude.seq` Prelude.rnf oversizeHandling

instance Core.ToJSON Cookies where
  toJSON Cookies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MatchPattern" Core..= matchPattern),
            Prelude.Just ("MatchScope" Core..= matchScope),
            Prelude.Just
              ("OversizeHandling" Core..= oversizeHandling)
          ]
      )
