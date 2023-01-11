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
-- Module      : Amazonka.Pinpoint.Types.ResultRowValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ResultRowValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a single value and metadata about that value as part of an
-- array of query results for a standard metric that applies to an
-- application, campaign, or journey.
--
-- /See:/ 'newResultRowValue' smart constructor.
data ResultRowValue = ResultRowValue'
  { -- | The data type of the value specified by the Value property.
    type' :: Prelude.Text,
    -- | In a Values object, the value for the metric that the query retrieved
    -- data for. In a GroupedBys object, the value for the field that was used
    -- to group data in a result set that contains multiple results (Values
    -- objects).
    value :: Prelude.Text,
    -- | The friendly name of the metric whose value is specified by the Value
    -- property.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultRowValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resultRowValue_type' - The data type of the value specified by the Value property.
--
-- 'value', 'resultRowValue_value' - In a Values object, the value for the metric that the query retrieved
-- data for. In a GroupedBys object, the value for the field that was used
-- to group data in a result set that contains multiple results (Values
-- objects).
--
-- 'key', 'resultRowValue_key' - The friendly name of the metric whose value is specified by the Value
-- property.
newResultRowValue ::
  -- | 'type''
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  ResultRowValue
newResultRowValue pType_ pValue_ pKey_ =
  ResultRowValue'
    { type' = pType_,
      value = pValue_,
      key = pKey_
    }

-- | The data type of the value specified by the Value property.
resultRowValue_type :: Lens.Lens' ResultRowValue Prelude.Text
resultRowValue_type = Lens.lens (\ResultRowValue' {type'} -> type') (\s@ResultRowValue' {} a -> s {type' = a} :: ResultRowValue)

-- | In a Values object, the value for the metric that the query retrieved
-- data for. In a GroupedBys object, the value for the field that was used
-- to group data in a result set that contains multiple results (Values
-- objects).
resultRowValue_value :: Lens.Lens' ResultRowValue Prelude.Text
resultRowValue_value = Lens.lens (\ResultRowValue' {value} -> value) (\s@ResultRowValue' {} a -> s {value = a} :: ResultRowValue)

-- | The friendly name of the metric whose value is specified by the Value
-- property.
resultRowValue_key :: Lens.Lens' ResultRowValue Prelude.Text
resultRowValue_key = Lens.lens (\ResultRowValue' {key} -> key) (\s@ResultRowValue' {} a -> s {key = a} :: ResultRowValue)

instance Data.FromJSON ResultRowValue where
  parseJSON =
    Data.withObject
      "ResultRowValue"
      ( \x ->
          ResultRowValue'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Value")
            Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable ResultRowValue where
  hashWithSalt _salt ResultRowValue' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData ResultRowValue where
  rnf ResultRowValue' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf key
