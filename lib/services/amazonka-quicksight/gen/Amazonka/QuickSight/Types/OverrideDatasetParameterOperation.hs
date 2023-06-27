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
-- Module      : Amazonka.QuickSight.Types.OverrideDatasetParameterOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.OverrideDatasetParameterOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NewDefaultValues

-- | A transform operation that overrides the dataset parameter values that
-- are defined in another dataset.
--
-- /See:/ 'newOverrideDatasetParameterOperation' smart constructor.
data OverrideDatasetParameterOperation = OverrideDatasetParameterOperation'
  { -- | The new default values for the parameter.
    newDefaultValues' :: Prelude.Maybe NewDefaultValues,
    -- | The new name for the parameter.
    newParameterName' :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter to be overridden with different values.
    parameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverrideDatasetParameterOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newDefaultValues'', 'overrideDatasetParameterOperation_newDefaultValues' - The new default values for the parameter.
--
-- 'newParameterName'', 'overrideDatasetParameterOperation_newParameterName' - The new name for the parameter.
--
-- 'parameterName', 'overrideDatasetParameterOperation_parameterName' - The name of the parameter to be overridden with different values.
newOverrideDatasetParameterOperation ::
  -- | 'parameterName'
  Prelude.Text ->
  OverrideDatasetParameterOperation
newOverrideDatasetParameterOperation pParameterName_ =
  OverrideDatasetParameterOperation'
    { newDefaultValues' =
        Prelude.Nothing,
      newParameterName' = Prelude.Nothing,
      parameterName = pParameterName_
    }

-- | The new default values for the parameter.
overrideDatasetParameterOperation_newDefaultValues :: Lens.Lens' OverrideDatasetParameterOperation (Prelude.Maybe NewDefaultValues)
overrideDatasetParameterOperation_newDefaultValues = Lens.lens (\OverrideDatasetParameterOperation' {newDefaultValues'} -> newDefaultValues') (\s@OverrideDatasetParameterOperation' {} a -> s {newDefaultValues' = a} :: OverrideDatasetParameterOperation)

-- | The new name for the parameter.
overrideDatasetParameterOperation_newParameterName :: Lens.Lens' OverrideDatasetParameterOperation (Prelude.Maybe Prelude.Text)
overrideDatasetParameterOperation_newParameterName = Lens.lens (\OverrideDatasetParameterOperation' {newParameterName'} -> newParameterName') (\s@OverrideDatasetParameterOperation' {} a -> s {newParameterName' = a} :: OverrideDatasetParameterOperation)

-- | The name of the parameter to be overridden with different values.
overrideDatasetParameterOperation_parameterName :: Lens.Lens' OverrideDatasetParameterOperation Prelude.Text
overrideDatasetParameterOperation_parameterName = Lens.lens (\OverrideDatasetParameterOperation' {parameterName} -> parameterName) (\s@OverrideDatasetParameterOperation' {} a -> s {parameterName = a} :: OverrideDatasetParameterOperation)

instance
  Data.FromJSON
    OverrideDatasetParameterOperation
  where
  parseJSON =
    Data.withObject
      "OverrideDatasetParameterOperation"
      ( \x ->
          OverrideDatasetParameterOperation'
            Prelude.<$> (x Data..:? "NewDefaultValues")
            Prelude.<*> (x Data..:? "NewParameterName")
            Prelude.<*> (x Data..: "ParameterName")
      )

instance
  Prelude.Hashable
    OverrideDatasetParameterOperation
  where
  hashWithSalt
    _salt
    OverrideDatasetParameterOperation' {..} =
      _salt
        `Prelude.hashWithSalt` newDefaultValues'
        `Prelude.hashWithSalt` newParameterName'
        `Prelude.hashWithSalt` parameterName

instance
  Prelude.NFData
    OverrideDatasetParameterOperation
  where
  rnf OverrideDatasetParameterOperation' {..} =
    Prelude.rnf newDefaultValues'
      `Prelude.seq` Prelude.rnf newParameterName'
      `Prelude.seq` Prelude.rnf parameterName

instance
  Data.ToJSON
    OverrideDatasetParameterOperation
  where
  toJSON OverrideDatasetParameterOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NewDefaultValues" Data..=)
              Prelude.<$> newDefaultValues',
            ("NewParameterName" Data..=)
              Prelude.<$> newParameterName',
            Prelude.Just
              ("ParameterName" Data..= parameterName)
          ]
      )
