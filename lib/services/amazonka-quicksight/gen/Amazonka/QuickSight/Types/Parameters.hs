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
-- Module      : Amazonka.QuickSight.Types.Parameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Parameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeParameter
import Amazonka.QuickSight.Types.DecimalParameter
import Amazonka.QuickSight.Types.IntegerParameter
import Amazonka.QuickSight.Types.StringParameter

-- | A list of Amazon QuickSight parameters and the list\'s override values.
--
-- /See:/ 'newParameters' smart constructor.
data Parameters = Parameters'
  { -- | The parameters that have a data type of date-time.
    dateTimeParameters :: Prelude.Maybe [DateTimeParameter],
    -- | The parameters that have a data type of decimal.
    decimalParameters :: Prelude.Maybe [DecimalParameter],
    -- | The parameters that have a data type of integer.
    integerParameters :: Prelude.Maybe [IntegerParameter],
    -- | The parameters that have a data type of string.
    stringParameters :: Prelude.Maybe [StringParameter]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeParameters', 'parameters_dateTimeParameters' - The parameters that have a data type of date-time.
--
-- 'decimalParameters', 'parameters_decimalParameters' - The parameters that have a data type of decimal.
--
-- 'integerParameters', 'parameters_integerParameters' - The parameters that have a data type of integer.
--
-- 'stringParameters', 'parameters_stringParameters' - The parameters that have a data type of string.
newParameters ::
  Parameters
newParameters =
  Parameters'
    { dateTimeParameters = Prelude.Nothing,
      decimalParameters = Prelude.Nothing,
      integerParameters = Prelude.Nothing,
      stringParameters = Prelude.Nothing
    }

-- | The parameters that have a data type of date-time.
parameters_dateTimeParameters :: Lens.Lens' Parameters (Prelude.Maybe [DateTimeParameter])
parameters_dateTimeParameters = Lens.lens (\Parameters' {dateTimeParameters} -> dateTimeParameters) (\s@Parameters' {} a -> s {dateTimeParameters = a} :: Parameters) Prelude.. Lens.mapping Lens.coerced

-- | The parameters that have a data type of decimal.
parameters_decimalParameters :: Lens.Lens' Parameters (Prelude.Maybe [DecimalParameter])
parameters_decimalParameters = Lens.lens (\Parameters' {decimalParameters} -> decimalParameters) (\s@Parameters' {} a -> s {decimalParameters = a} :: Parameters) Prelude.. Lens.mapping Lens.coerced

-- | The parameters that have a data type of integer.
parameters_integerParameters :: Lens.Lens' Parameters (Prelude.Maybe [IntegerParameter])
parameters_integerParameters = Lens.lens (\Parameters' {integerParameters} -> integerParameters) (\s@Parameters' {} a -> s {integerParameters = a} :: Parameters) Prelude.. Lens.mapping Lens.coerced

-- | The parameters that have a data type of string.
parameters_stringParameters :: Lens.Lens' Parameters (Prelude.Maybe [StringParameter])
parameters_stringParameters = Lens.lens (\Parameters' {stringParameters} -> stringParameters) (\s@Parameters' {} a -> s {stringParameters = a} :: Parameters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Parameters where
  hashWithSalt _salt Parameters' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeParameters
      `Prelude.hashWithSalt` decimalParameters
      `Prelude.hashWithSalt` integerParameters
      `Prelude.hashWithSalt` stringParameters

instance Prelude.NFData Parameters where
  rnf Parameters' {..} =
    Prelude.rnf dateTimeParameters `Prelude.seq`
      Prelude.rnf decimalParameters `Prelude.seq`
        Prelude.rnf integerParameters `Prelude.seq`
          Prelude.rnf stringParameters

instance Data.ToJSON Parameters where
  toJSON Parameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeParameters" Data..=)
              Prelude.<$> dateTimeParameters,
            ("DecimalParameters" Data..=)
              Prelude.<$> decimalParameters,
            ("IntegerParameters" Data..=)
              Prelude.<$> integerParameters,
            ("StringParameters" Data..=)
              Prelude.<$> stringParameters
          ]
      )
