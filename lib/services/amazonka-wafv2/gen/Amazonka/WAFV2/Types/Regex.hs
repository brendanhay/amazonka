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
-- Module      : Amazonka.WAFV2.Types.Regex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Regex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A single regular expression. This is used in a RegexPatternSet.
--
-- /See:/ 'newRegex' smart constructor.
data Regex = Regex'
  { -- | The string representing the regular expression.
    regexString :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Regex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexString', 'regex_regexString' - The string representing the regular expression.
newRegex ::
  Regex
newRegex = Regex' {regexString = Prelude.Nothing}

-- | The string representing the regular expression.
regex_regexString :: Lens.Lens' Regex (Prelude.Maybe Prelude.Text)
regex_regexString = Lens.lens (\Regex' {regexString} -> regexString) (\s@Regex' {} a -> s {regexString = a} :: Regex)

instance Core.FromJSON Regex where
  parseJSON =
    Core.withObject
      "Regex"
      ( \x ->
          Regex' Prelude.<$> (x Core..:? "RegexString")
      )

instance Prelude.Hashable Regex where
  hashWithSalt _salt Regex' {..} =
    _salt `Prelude.hashWithSalt` regexString

instance Prelude.NFData Regex where
  rnf Regex' {..} = Prelude.rnf regexString

instance Core.ToJSON Regex where
  toJSON Regex' {..} =
    Core.object
      ( Prelude.catMaybes
          [("RegexString" Core..=) Prelude.<$> regexString]
      )
