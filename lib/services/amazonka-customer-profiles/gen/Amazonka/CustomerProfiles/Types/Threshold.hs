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
-- Module      : Amazonka.CustomerProfiles.Types.Threshold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Threshold where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Operator
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The threshold for the calculated attribute.
--
-- /See:/ 'newThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The value of the threshold.
    value :: Prelude.Text,
    -- | The operator of the threshold.
    operator :: Operator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Threshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'threshold_value' - The value of the threshold.
--
-- 'operator', 'threshold_operator' - The operator of the threshold.
newThreshold ::
  -- | 'value'
  Prelude.Text ->
  -- | 'operator'
  Operator ->
  Threshold
newThreshold pValue_ pOperator_ =
  Threshold' {value = pValue_, operator = pOperator_}

-- | The value of the threshold.
threshold_value :: Lens.Lens' Threshold Prelude.Text
threshold_value = Lens.lens (\Threshold' {value} -> value) (\s@Threshold' {} a -> s {value = a} :: Threshold)

-- | The operator of the threshold.
threshold_operator :: Lens.Lens' Threshold Operator
threshold_operator = Lens.lens (\Threshold' {operator} -> operator) (\s@Threshold' {} a -> s {operator = a} :: Threshold)

instance Data.FromJSON Threshold where
  parseJSON =
    Data.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Prelude.<$> (x Data..: "Value")
            Prelude.<*> (x Data..: "Operator")
      )

instance Prelude.Hashable Threshold where
  hashWithSalt _salt Threshold' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` operator

instance Prelude.NFData Threshold where
  rnf Threshold' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON Threshold where
  toJSON Threshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Data..= value),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
