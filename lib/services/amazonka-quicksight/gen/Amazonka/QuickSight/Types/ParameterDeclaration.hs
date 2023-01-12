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
-- Module      : Amazonka.QuickSight.Types.ParameterDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeParameterDeclaration
import Amazonka.QuickSight.Types.DecimalParameterDeclaration
import Amazonka.QuickSight.Types.IntegerParameterDeclaration
import Amazonka.QuickSight.Types.StringParameterDeclaration

-- | The declaration definition of a parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newParameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { -- | A parameter declaration for the @DateTime@ data type.
    dateTimeParameterDeclaration :: Prelude.Maybe DateTimeParameterDeclaration,
    -- | A parameter declaration for the @Decimal@ data type.
    decimalParameterDeclaration :: Prelude.Maybe DecimalParameterDeclaration,
    -- | A parameter declaration for the @Integer@ data type.
    integerParameterDeclaration :: Prelude.Maybe IntegerParameterDeclaration,
    -- | A parameter declaration for the @String@ data type.
    stringParameterDeclaration :: Prelude.Maybe StringParameterDeclaration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeParameterDeclaration', 'parameterDeclaration_dateTimeParameterDeclaration' - A parameter declaration for the @DateTime@ data type.
--
-- 'decimalParameterDeclaration', 'parameterDeclaration_decimalParameterDeclaration' - A parameter declaration for the @Decimal@ data type.
--
-- 'integerParameterDeclaration', 'parameterDeclaration_integerParameterDeclaration' - A parameter declaration for the @Integer@ data type.
--
-- 'stringParameterDeclaration', 'parameterDeclaration_stringParameterDeclaration' - A parameter declaration for the @String@ data type.
newParameterDeclaration ::
  ParameterDeclaration
newParameterDeclaration =
  ParameterDeclaration'
    { dateTimeParameterDeclaration =
        Prelude.Nothing,
      decimalParameterDeclaration = Prelude.Nothing,
      integerParameterDeclaration = Prelude.Nothing,
      stringParameterDeclaration = Prelude.Nothing
    }

-- | A parameter declaration for the @DateTime@ data type.
parameterDeclaration_dateTimeParameterDeclaration :: Lens.Lens' ParameterDeclaration (Prelude.Maybe DateTimeParameterDeclaration)
parameterDeclaration_dateTimeParameterDeclaration = Lens.lens (\ParameterDeclaration' {dateTimeParameterDeclaration} -> dateTimeParameterDeclaration) (\s@ParameterDeclaration' {} a -> s {dateTimeParameterDeclaration = a} :: ParameterDeclaration)

-- | A parameter declaration for the @Decimal@ data type.
parameterDeclaration_decimalParameterDeclaration :: Lens.Lens' ParameterDeclaration (Prelude.Maybe DecimalParameterDeclaration)
parameterDeclaration_decimalParameterDeclaration = Lens.lens (\ParameterDeclaration' {decimalParameterDeclaration} -> decimalParameterDeclaration) (\s@ParameterDeclaration' {} a -> s {decimalParameterDeclaration = a} :: ParameterDeclaration)

-- | A parameter declaration for the @Integer@ data type.
parameterDeclaration_integerParameterDeclaration :: Lens.Lens' ParameterDeclaration (Prelude.Maybe IntegerParameterDeclaration)
parameterDeclaration_integerParameterDeclaration = Lens.lens (\ParameterDeclaration' {integerParameterDeclaration} -> integerParameterDeclaration) (\s@ParameterDeclaration' {} a -> s {integerParameterDeclaration = a} :: ParameterDeclaration)

-- | A parameter declaration for the @String@ data type.
parameterDeclaration_stringParameterDeclaration :: Lens.Lens' ParameterDeclaration (Prelude.Maybe StringParameterDeclaration)
parameterDeclaration_stringParameterDeclaration = Lens.lens (\ParameterDeclaration' {stringParameterDeclaration} -> stringParameterDeclaration) (\s@ParameterDeclaration' {} a -> s {stringParameterDeclaration = a} :: ParameterDeclaration)

instance Data.FromJSON ParameterDeclaration where
  parseJSON =
    Data.withObject
      "ParameterDeclaration"
      ( \x ->
          ParameterDeclaration'
            Prelude.<$> (x Data..:? "DateTimeParameterDeclaration")
            Prelude.<*> (x Data..:? "DecimalParameterDeclaration")
            Prelude.<*> (x Data..:? "IntegerParameterDeclaration")
            Prelude.<*> (x Data..:? "StringParameterDeclaration")
      )

instance Prelude.Hashable ParameterDeclaration where
  hashWithSalt _salt ParameterDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeParameterDeclaration
      `Prelude.hashWithSalt` decimalParameterDeclaration
      `Prelude.hashWithSalt` integerParameterDeclaration
      `Prelude.hashWithSalt` stringParameterDeclaration

instance Prelude.NFData ParameterDeclaration where
  rnf ParameterDeclaration' {..} =
    Prelude.rnf dateTimeParameterDeclaration
      `Prelude.seq` Prelude.rnf decimalParameterDeclaration
      `Prelude.seq` Prelude.rnf integerParameterDeclaration
      `Prelude.seq` Prelude.rnf stringParameterDeclaration

instance Data.ToJSON ParameterDeclaration where
  toJSON ParameterDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeParameterDeclaration" Data..=)
              Prelude.<$> dateTimeParameterDeclaration,
            ("DecimalParameterDeclaration" Data..=)
              Prelude.<$> decimalParameterDeclaration,
            ("IntegerParameterDeclaration" Data..=)
              Prelude.<$> integerParameterDeclaration,
            ("StringParameterDeclaration" Data..=)
              Prelude.<$> stringParameterDeclaration
          ]
      )
