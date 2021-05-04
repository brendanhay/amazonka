{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeRegexConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a regular expression used to validate the value of a slot.
--
-- /See:/ 'newSlotTypeRegexConfiguration' smart constructor.
data SlotTypeRegexConfiguration = SlotTypeRegexConfiguration'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeRegexConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'slotTypeRegexConfiguration_pattern' - A regular expression used to validate the value of a slot.
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
newSlotTypeRegexConfiguration ::
  -- | 'pattern''
  Prelude.Text ->
  SlotTypeRegexConfiguration
newSlotTypeRegexConfiguration pPattern_ =
  SlotTypeRegexConfiguration' {pattern' = pPattern_}

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
slotTypeRegexConfiguration_pattern :: Lens.Lens' SlotTypeRegexConfiguration Prelude.Text
slotTypeRegexConfiguration_pattern = Lens.lens (\SlotTypeRegexConfiguration' {pattern'} -> pattern') (\s@SlotTypeRegexConfiguration' {} a -> s {pattern' = a} :: SlotTypeRegexConfiguration)

instance Prelude.FromJSON SlotTypeRegexConfiguration where
  parseJSON =
    Prelude.withObject
      "SlotTypeRegexConfiguration"
      ( \x ->
          SlotTypeRegexConfiguration'
            Prelude.<$> (x Prelude..: "pattern")
      )

instance Prelude.Hashable SlotTypeRegexConfiguration

instance Prelude.NFData SlotTypeRegexConfiguration

instance Prelude.ToJSON SlotTypeRegexConfiguration where
  toJSON SlotTypeRegexConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("pattern" Prelude..= pattern')]
      )
