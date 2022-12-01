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
-- Module      : Amazonka.WAFV2.Types.JsonMatchPattern
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.JsonMatchPattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.All

-- | The patterns to look for in the JSON body. WAF inspects the results of
-- these pattern matches against the rule inspection criteria. This is used
-- with the FieldToMatch option @JsonBody@.
--
-- /See:/ 'newJsonMatchPattern' smart constructor.
data JsonMatchPattern = JsonMatchPattern'
  { -- | Match all of the elements. See also @MatchScope@ in JsonBody.
    --
    -- You must specify either this setting or the @IncludedPaths@ setting, but
    -- not both.
    all :: Prelude.Maybe All,
    -- | Match only the specified include paths. See also @MatchScope@ in
    -- JsonBody.
    --
    -- Provide the include paths using JSON Pointer syntax. For example,
    -- @\"IncludedPaths\": [\"\/dogs\/0\/name\", \"\/dogs\/1\/name\"]@. For
    -- information about this syntax, see the Internet Engineering Task Force
    -- (IETF) documentation
    -- <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    -- You must specify either this setting or the @All@ setting, but not both.
    --
    -- Don\'t use this option to include all paths. Instead, use the @All@
    -- setting.
    includedPaths :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JsonMatchPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'all', 'jsonMatchPattern_all' - Match all of the elements. See also @MatchScope@ in JsonBody.
--
-- You must specify either this setting or the @IncludedPaths@ setting, but
-- not both.
--
-- 'includedPaths', 'jsonMatchPattern_includedPaths' - Match only the specified include paths. See also @MatchScope@ in
-- JsonBody.
--
-- Provide the include paths using JSON Pointer syntax. For example,
-- @\"IncludedPaths\": [\"\/dogs\/0\/name\", \"\/dogs\/1\/name\"]@. For
-- information about this syntax, see the Internet Engineering Task Force
-- (IETF) documentation
-- <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
-- You must specify either this setting or the @All@ setting, but not both.
--
-- Don\'t use this option to include all paths. Instead, use the @All@
-- setting.
newJsonMatchPattern ::
  JsonMatchPattern
newJsonMatchPattern =
  JsonMatchPattern'
    { all = Prelude.Nothing,
      includedPaths = Prelude.Nothing
    }

-- | Match all of the elements. See also @MatchScope@ in JsonBody.
--
-- You must specify either this setting or the @IncludedPaths@ setting, but
-- not both.
jsonMatchPattern_all :: Lens.Lens' JsonMatchPattern (Prelude.Maybe All)
jsonMatchPattern_all = Lens.lens (\JsonMatchPattern' {all} -> all) (\s@JsonMatchPattern' {} a -> s {all = a} :: JsonMatchPattern)

-- | Match only the specified include paths. See also @MatchScope@ in
-- JsonBody.
--
-- Provide the include paths using JSON Pointer syntax. For example,
-- @\"IncludedPaths\": [\"\/dogs\/0\/name\", \"\/dogs\/1\/name\"]@. For
-- information about this syntax, see the Internet Engineering Task Force
-- (IETF) documentation
-- <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
-- You must specify either this setting or the @All@ setting, but not both.
--
-- Don\'t use this option to include all paths. Instead, use the @All@
-- setting.
jsonMatchPattern_includedPaths :: Lens.Lens' JsonMatchPattern (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
jsonMatchPattern_includedPaths = Lens.lens (\JsonMatchPattern' {includedPaths} -> includedPaths) (\s@JsonMatchPattern' {} a -> s {includedPaths = a} :: JsonMatchPattern) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON JsonMatchPattern where
  parseJSON =
    Core.withObject
      "JsonMatchPattern"
      ( \x ->
          JsonMatchPattern'
            Prelude.<$> (x Core..:? "All")
            Prelude.<*> (x Core..:? "IncludedPaths")
      )

instance Prelude.Hashable JsonMatchPattern where
  hashWithSalt _salt JsonMatchPattern' {..} =
    _salt `Prelude.hashWithSalt` all
      `Prelude.hashWithSalt` includedPaths

instance Prelude.NFData JsonMatchPattern where
  rnf JsonMatchPattern' {..} =
    Prelude.rnf all
      `Prelude.seq` Prelude.rnf includedPaths

instance Core.ToJSON JsonMatchPattern where
  toJSON JsonMatchPattern' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("All" Core..=) Prelude.<$> all,
            ("IncludedPaths" Core..=) Prelude.<$> includedPaths
          ]
      )
