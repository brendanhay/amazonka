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
-- Module      : Amazonka.Textract.Types.NormalizedValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.NormalizedValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.ValueType

-- | Contains information relating to dates in a document, including the type
-- of value, and the value.
--
-- /See:/ 'newNormalizedValue' smart constructor.
data NormalizedValue = NormalizedValue'
  { -- | The value of the date, written as Year-Month-DayTHour:Minute:Second.
    value :: Prelude.Maybe Prelude.Text,
    -- | The normalized type of the value detected. In this case, DATE.
    valueType :: Prelude.Maybe ValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NormalizedValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'normalizedValue_value' - The value of the date, written as Year-Month-DayTHour:Minute:Second.
--
-- 'valueType', 'normalizedValue_valueType' - The normalized type of the value detected. In this case, DATE.
newNormalizedValue ::
  NormalizedValue
newNormalizedValue =
  NormalizedValue'
    { value = Prelude.Nothing,
      valueType = Prelude.Nothing
    }

-- | The value of the date, written as Year-Month-DayTHour:Minute:Second.
normalizedValue_value :: Lens.Lens' NormalizedValue (Prelude.Maybe Prelude.Text)
normalizedValue_value = Lens.lens (\NormalizedValue' {value} -> value) (\s@NormalizedValue' {} a -> s {value = a} :: NormalizedValue)

-- | The normalized type of the value detected. In this case, DATE.
normalizedValue_valueType :: Lens.Lens' NormalizedValue (Prelude.Maybe ValueType)
normalizedValue_valueType = Lens.lens (\NormalizedValue' {valueType} -> valueType) (\s@NormalizedValue' {} a -> s {valueType = a} :: NormalizedValue)

instance Data.FromJSON NormalizedValue where
  parseJSON =
    Data.withObject
      "NormalizedValue"
      ( \x ->
          NormalizedValue'
            Prelude.<$> (x Data..:? "Value")
            Prelude.<*> (x Data..:? "ValueType")
      )

instance Prelude.Hashable NormalizedValue where
  hashWithSalt _salt NormalizedValue' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` valueType

instance Prelude.NFData NormalizedValue where
  rnf NormalizedValue' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf valueType
