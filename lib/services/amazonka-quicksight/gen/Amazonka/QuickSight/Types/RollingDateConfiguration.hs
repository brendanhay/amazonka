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
-- Module      : Amazonka.QuickSight.Types.RollingDateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RollingDateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The rolling date configuration of a date time filter.
--
-- /See:/ 'newRollingDateConfiguration' smart constructor.
data RollingDateConfiguration = RollingDateConfiguration'
  { -- | The data set that is used in the rolling date configuration.
    dataSetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The expression of the rolling date configuration.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollingDateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetIdentifier', 'rollingDateConfiguration_dataSetIdentifier' - The data set that is used in the rolling date configuration.
--
-- 'expression', 'rollingDateConfiguration_expression' - The expression of the rolling date configuration.
newRollingDateConfiguration ::
  -- | 'expression'
  Prelude.Text ->
  RollingDateConfiguration
newRollingDateConfiguration pExpression_ =
  RollingDateConfiguration'
    { dataSetIdentifier =
        Prelude.Nothing,
      expression = Data._Sensitive Lens.# pExpression_
    }

-- | The data set that is used in the rolling date configuration.
rollingDateConfiguration_dataSetIdentifier :: Lens.Lens' RollingDateConfiguration (Prelude.Maybe Prelude.Text)
rollingDateConfiguration_dataSetIdentifier = Lens.lens (\RollingDateConfiguration' {dataSetIdentifier} -> dataSetIdentifier) (\s@RollingDateConfiguration' {} a -> s {dataSetIdentifier = a} :: RollingDateConfiguration)

-- | The expression of the rolling date configuration.
rollingDateConfiguration_expression :: Lens.Lens' RollingDateConfiguration Prelude.Text
rollingDateConfiguration_expression = Lens.lens (\RollingDateConfiguration' {expression} -> expression) (\s@RollingDateConfiguration' {} a -> s {expression = a} :: RollingDateConfiguration) Prelude.. Data._Sensitive

instance Data.FromJSON RollingDateConfiguration where
  parseJSON =
    Data.withObject
      "RollingDateConfiguration"
      ( \x ->
          RollingDateConfiguration'
            Prelude.<$> (x Data..:? "DataSetIdentifier")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable RollingDateConfiguration where
  hashWithSalt _salt RollingDateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dataSetIdentifier
      `Prelude.hashWithSalt` expression

instance Prelude.NFData RollingDateConfiguration where
  rnf RollingDateConfiguration' {..} =
    Prelude.rnf dataSetIdentifier
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON RollingDateConfiguration where
  toJSON RollingDateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSetIdentifier" Data..=)
              Prelude.<$> dataSetIdentifier,
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
