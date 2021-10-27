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
-- Module      : Network.AWS.LexV2Runtime.Types.Value
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Value where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of a slot.
--
-- /See:/ 'newValue' smart constructor.
data Value = Value'
  { -- | The text of the utterance from the user that was entered for the slot.
    originalValue :: Prelude.Maybe Prelude.Text,
    -- | A list of additional values that have been recognized for the slot.
    resolvedValues :: Prelude.Maybe [Prelude.Text],
    -- | The value that Amazon Lex V2 determines for the slot. The actual value
    -- depends on the setting of the value selection strategy for the bot. You
    -- can choose to use the value entered by the user, or you can have Amazon
    -- Lex V2 choose the first value in the @resolvedValues@ list.
    interpretedValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Value' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originalValue', 'value_originalValue' - The text of the utterance from the user that was entered for the slot.
--
-- 'resolvedValues', 'value_resolvedValues' - A list of additional values that have been recognized for the slot.
--
-- 'interpretedValue', 'value_interpretedValue' - The value that Amazon Lex V2 determines for the slot. The actual value
-- depends on the setting of the value selection strategy for the bot. You
-- can choose to use the value entered by the user, or you can have Amazon
-- Lex V2 choose the first value in the @resolvedValues@ list.
newValue ::
  -- | 'interpretedValue'
  Prelude.Text ->
  Value
newValue pInterpretedValue_ =
  Value'
    { originalValue = Prelude.Nothing,
      resolvedValues = Prelude.Nothing,
      interpretedValue = pInterpretedValue_
    }

-- | The text of the utterance from the user that was entered for the slot.
value_originalValue :: Lens.Lens' Value (Prelude.Maybe Prelude.Text)
value_originalValue = Lens.lens (\Value' {originalValue} -> originalValue) (\s@Value' {} a -> s {originalValue = a} :: Value)

-- | A list of additional values that have been recognized for the slot.
value_resolvedValues :: Lens.Lens' Value (Prelude.Maybe [Prelude.Text])
value_resolvedValues = Lens.lens (\Value' {resolvedValues} -> resolvedValues) (\s@Value' {} a -> s {resolvedValues = a} :: Value) Prelude.. Lens.mapping Lens.coerced

-- | The value that Amazon Lex V2 determines for the slot. The actual value
-- depends on the setting of the value selection strategy for the bot. You
-- can choose to use the value entered by the user, or you can have Amazon
-- Lex V2 choose the first value in the @resolvedValues@ list.
value_interpretedValue :: Lens.Lens' Value Prelude.Text
value_interpretedValue = Lens.lens (\Value' {interpretedValue} -> interpretedValue) (\s@Value' {} a -> s {interpretedValue = a} :: Value)

instance Core.FromJSON Value where
  parseJSON =
    Core.withObject
      "Value"
      ( \x ->
          Value'
            Prelude.<$> (x Core..:? "originalValue")
            Prelude.<*> (x Core..:? "resolvedValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "interpretedValue")
      )

instance Prelude.Hashable Value

instance Prelude.NFData Value

instance Core.ToJSON Value where
  toJSON Value' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("originalValue" Core..=) Prelude.<$> originalValue,
            ("resolvedValues" Core..=)
              Prelude.<$> resolvedValues,
            Prelude.Just
              ("interpretedValue" Core..= interpretedValue)
          ]
      )
