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
-- Module      : Amazonka.QuickSight.Types.DateTimeParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A date-time parameter.
--
-- /See:/ 'newDateTimeParameter' smart constructor.
data DateTimeParameter = DateTimeParameter'
  { -- | A display name for the date-time parameter.
    name :: Prelude.Text,
    -- | The values for the date-time parameter.
    values :: [Data.Sensitive Data.POSIX]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dateTimeParameter_name' - A display name for the date-time parameter.
--
-- 'values', 'dateTimeParameter_values' - The values for the date-time parameter.
newDateTimeParameter ::
  -- | 'name'
  Prelude.Text ->
  DateTimeParameter
newDateTimeParameter pName_ =
  DateTimeParameter'
    { name = pName_,
      values = Prelude.mempty
    }

-- | A display name for the date-time parameter.
dateTimeParameter_name :: Lens.Lens' DateTimeParameter Prelude.Text
dateTimeParameter_name = Lens.lens (\DateTimeParameter' {name} -> name) (\s@DateTimeParameter' {} a -> s {name = a} :: DateTimeParameter)

-- | The values for the date-time parameter.
dateTimeParameter_values :: Lens.Lens' DateTimeParameter [Prelude.UTCTime]
dateTimeParameter_values = Lens.lens (\DateTimeParameter' {values} -> values) (\s@DateTimeParameter' {} a -> s {values = a} :: DateTimeParameter) Prelude.. Lens.coerced

instance Prelude.Hashable DateTimeParameter where
  hashWithSalt _salt DateTimeParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData DateTimeParameter where
  rnf DateTimeParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON DateTimeParameter where
  toJSON DateTimeParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
