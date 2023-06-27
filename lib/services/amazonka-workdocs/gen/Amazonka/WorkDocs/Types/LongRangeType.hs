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
-- Module      : Amazonka.WorkDocs.Types.LongRangeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.LongRangeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter based on size (in bytes).
--
-- /See:/ 'newLongRangeType' smart constructor.
data LongRangeType = LongRangeType'
  { -- | The size end range (in bytes).
    endValue :: Prelude.Maybe Prelude.Integer,
    -- | The size start range (in bytes).
    startValue :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LongRangeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endValue', 'longRangeType_endValue' - The size end range (in bytes).
--
-- 'startValue', 'longRangeType_startValue' - The size start range (in bytes).
newLongRangeType ::
  LongRangeType
newLongRangeType =
  LongRangeType'
    { endValue = Prelude.Nothing,
      startValue = Prelude.Nothing
    }

-- | The size end range (in bytes).
longRangeType_endValue :: Lens.Lens' LongRangeType (Prelude.Maybe Prelude.Integer)
longRangeType_endValue = Lens.lens (\LongRangeType' {endValue} -> endValue) (\s@LongRangeType' {} a -> s {endValue = a} :: LongRangeType)

-- | The size start range (in bytes).
longRangeType_startValue :: Lens.Lens' LongRangeType (Prelude.Maybe Prelude.Integer)
longRangeType_startValue = Lens.lens (\LongRangeType' {startValue} -> startValue) (\s@LongRangeType' {} a -> s {startValue = a} :: LongRangeType)

instance Prelude.Hashable LongRangeType where
  hashWithSalt _salt LongRangeType' {..} =
    _salt
      `Prelude.hashWithSalt` endValue
      `Prelude.hashWithSalt` startValue

instance Prelude.NFData LongRangeType where
  rnf LongRangeType' {..} =
    Prelude.rnf endValue
      `Prelude.seq` Prelude.rnf startValue

instance Data.ToJSON LongRangeType where
  toJSON LongRangeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndValue" Data..=) Prelude.<$> endValue,
            ("StartValue" Data..=) Prelude.<$> startValue
          ]
      )
