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
-- Module      : Amazonka.OpenSearch.Types.Duration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.Duration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.TimeUnit
import qualified Amazonka.Prelude as Prelude

-- | The duration of a maintenance schedule. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | The unit of measurement for the duration of a maintenance schedule.
    unit :: Prelude.Maybe TimeUnit,
    -- | Integer to specify the value of a maintenance schedule duration.
    value :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Duration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'duration_unit' - The unit of measurement for the duration of a maintenance schedule.
--
-- 'value', 'duration_value' - Integer to specify the value of a maintenance schedule duration.
newDuration ::
  Duration
newDuration =
  Duration'
    { unit = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The unit of measurement for the duration of a maintenance schedule.
duration_unit :: Lens.Lens' Duration (Prelude.Maybe TimeUnit)
duration_unit = Lens.lens (\Duration' {unit} -> unit) (\s@Duration' {} a -> s {unit = a} :: Duration)

-- | Integer to specify the value of a maintenance schedule duration.
duration_value :: Lens.Lens' Duration (Prelude.Maybe Prelude.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

instance Data.FromJSON Duration where
  parseJSON =
    Data.withObject
      "Duration"
      ( \x ->
          Duration'
            Prelude.<$> (x Data..:? "Unit")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Duration where
  hashWithSalt _salt Duration' {..} =
    _salt
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Duration where
  rnf Duration' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Duration where
  toJSON Duration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Unit" Data..=) Prelude.<$> unit,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
