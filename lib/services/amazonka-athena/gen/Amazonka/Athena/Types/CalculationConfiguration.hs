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
-- Module      : Amazonka.Athena.Types.CalculationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information for the calculation.
--
-- /See:/ 'newCalculationConfiguration' smart constructor.
data CalculationConfiguration = CalculationConfiguration'
  { -- | A string that contains the code for the calculation.
    codeBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeBlock', 'calculationConfiguration_codeBlock' - A string that contains the code for the calculation.
newCalculationConfiguration ::
  CalculationConfiguration
newCalculationConfiguration =
  CalculationConfiguration'
    { codeBlock =
        Prelude.Nothing
    }

-- | A string that contains the code for the calculation.
calculationConfiguration_codeBlock :: Lens.Lens' CalculationConfiguration (Prelude.Maybe Prelude.Text)
calculationConfiguration_codeBlock = Lens.lens (\CalculationConfiguration' {codeBlock} -> codeBlock) (\s@CalculationConfiguration' {} a -> s {codeBlock = a} :: CalculationConfiguration)

instance Prelude.Hashable CalculationConfiguration where
  hashWithSalt _salt CalculationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` codeBlock

instance Prelude.NFData CalculationConfiguration where
  rnf CalculationConfiguration' {..} =
    Prelude.rnf codeBlock

instance Data.ToJSON CalculationConfiguration where
  toJSON CalculationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CodeBlock" Data..=) Prelude.<$> codeBlock]
      )
