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
-- Module      : Amazonka.ElasticSearch.Types.Duration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.Duration where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types.TimeUnit
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies maintenance schedule duration: duration value and duration
-- unit. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | Integer to specify the value of a maintenance schedule duration. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    value :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the unit of a maintenance schedule duration. Valid value is
    -- HOURS. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    unit :: Prelude.Maybe TimeUnit
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
-- 'value', 'duration_value' - Integer to specify the value of a maintenance schedule duration. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'unit', 'duration_unit' - Specifies the unit of a maintenance schedule duration. Valid value is
-- HOURS. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newDuration ::
  Duration
newDuration =
  Duration'
    { value = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | Integer to specify the value of a maintenance schedule duration. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
duration_value :: Lens.Lens' Duration (Prelude.Maybe Prelude.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

-- | Specifies the unit of a maintenance schedule duration. Valid value is
-- HOURS. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
duration_unit :: Lens.Lens' Duration (Prelude.Maybe TimeUnit)
duration_unit = Lens.lens (\Duration' {unit} -> unit) (\s@Duration' {} a -> s {unit = a} :: Duration)

instance Core.FromJSON Duration where
  parseJSON =
    Core.withObject
      "Duration"
      ( \x ->
          Duration'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Unit")
      )

instance Prelude.Hashable Duration

instance Prelude.NFData Duration

instance Core.ToJSON Duration where
  toJSON Duration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Unit" Core..=) Prelude.<$> unit
          ]
      )
