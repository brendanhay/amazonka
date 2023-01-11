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
-- Module      : Amazonka.FraudDetector.Types.FilterCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.FilterCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A conditional statement for filtering a list of past predictions.
--
-- /See:/ 'newFilterCondition' smart constructor.
data FilterCondition = FilterCondition'
  { -- | A statement containing a resource property and a value to specify filter
    -- condition.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'filterCondition_value' - A statement containing a resource property and a value to specify filter
-- condition.
newFilterCondition ::
  FilterCondition
newFilterCondition =
  FilterCondition' {value = Prelude.Nothing}

-- | A statement containing a resource property and a value to specify filter
-- condition.
filterCondition_value :: Lens.Lens' FilterCondition (Prelude.Maybe Prelude.Text)
filterCondition_value = Lens.lens (\FilterCondition' {value} -> value) (\s@FilterCondition' {} a -> s {value = a} :: FilterCondition)

instance Prelude.Hashable FilterCondition where
  hashWithSalt _salt FilterCondition' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData FilterCondition where
  rnf FilterCondition' {..} = Prelude.rnf value

instance Data.ToJSON FilterCondition where
  toJSON FilterCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("value" Data..=) Prelude.<$> value]
      )
