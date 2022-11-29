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
-- Module      : Amazonka.LexV2Models.Types.SlotValueRegexFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotValueRegexFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a regular expression used to validate the value of a slot.
--
-- /See:/ 'newSlotValueRegexFilter' smart constructor.
data SlotValueRegexFilter = SlotValueRegexFilter'
  { -- | A regular expression used to validate the value of a slot.
    --
    -- Use a standard regular expression. Amazon Lex supports the following
    -- characters in the regular expression:
    --
    -- -   A-Z, a-z
    --
    -- -   0-9
    --
    -- -   Unicode characters (\"\\ u\<Unicode>\")
    --
    -- Represent Unicode characters with four digits, for example \"\\u0041\"
    -- or \"\\u005A\".
    --
    -- The following regular expression operators are not supported:
    --
    -- -   Infinite repeaters: *, +, or {x,} with no upper bound.
    --
    -- -   Wild card (.)
    pattern' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotValueRegexFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'slotValueRegexFilter_pattern' - A regular expression used to validate the value of a slot.
--
-- Use a standard regular expression. Amazon Lex supports the following
-- characters in the regular expression:
--
-- -   A-Z, a-z
--
-- -   0-9
--
-- -   Unicode characters (\"\\ u\<Unicode>\")
--
-- Represent Unicode characters with four digits, for example \"\\u0041\"
-- or \"\\u005A\".
--
-- The following regular expression operators are not supported:
--
-- -   Infinite repeaters: *, +, or {x,} with no upper bound.
--
-- -   Wild card (.)
newSlotValueRegexFilter ::
  -- | 'pattern''
  Prelude.Text ->
  SlotValueRegexFilter
newSlotValueRegexFilter pPattern_ =
  SlotValueRegexFilter' {pattern' = pPattern_}

-- | A regular expression used to validate the value of a slot.
--
-- Use a standard regular expression. Amazon Lex supports the following
-- characters in the regular expression:
--
-- -   A-Z, a-z
--
-- -   0-9
--
-- -   Unicode characters (\"\\ u\<Unicode>\")
--
-- Represent Unicode characters with four digits, for example \"\\u0041\"
-- or \"\\u005A\".
--
-- The following regular expression operators are not supported:
--
-- -   Infinite repeaters: *, +, or {x,} with no upper bound.
--
-- -   Wild card (.)
slotValueRegexFilter_pattern :: Lens.Lens' SlotValueRegexFilter Prelude.Text
slotValueRegexFilter_pattern = Lens.lens (\SlotValueRegexFilter' {pattern'} -> pattern') (\s@SlotValueRegexFilter' {} a -> s {pattern' = a} :: SlotValueRegexFilter)

instance Core.FromJSON SlotValueRegexFilter where
  parseJSON =
    Core.withObject
      "SlotValueRegexFilter"
      ( \x ->
          SlotValueRegexFilter'
            Prelude.<$> (x Core..: "pattern")
      )

instance Prelude.Hashable SlotValueRegexFilter where
  hashWithSalt _salt SlotValueRegexFilter' {..} =
    _salt `Prelude.hashWithSalt` pattern'

instance Prelude.NFData SlotValueRegexFilter where
  rnf SlotValueRegexFilter' {..} = Prelude.rnf pattern'

instance Core.ToJSON SlotValueRegexFilter where
  toJSON SlotValueRegexFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("pattern" Core..= pattern')]
      )
