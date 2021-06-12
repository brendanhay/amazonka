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
-- Module      : Network.AWS.ElasticSearch.Types.Duration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.Duration where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.TimeUnit
import qualified Network.AWS.Lens as Lens

-- | Specifies maintenance schedule duration: duration value and duration
-- unit. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | Specifies the unit of a maintenance schedule duration. Valid value is
    -- HOURS. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    unit :: Core.Maybe TimeUnit,
    -- | Integer to specify the value of a maintenance schedule duration. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    value :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Duration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'duration_unit' - Specifies the unit of a maintenance schedule duration. Valid value is
-- HOURS. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'value', 'duration_value' - Integer to specify the value of a maintenance schedule duration. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newDuration ::
  Duration
newDuration =
  Duration'
    { unit = Core.Nothing,
      value = Core.Nothing
    }

-- | Specifies the unit of a maintenance schedule duration. Valid value is
-- HOURS. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
duration_unit :: Lens.Lens' Duration (Core.Maybe TimeUnit)
duration_unit = Lens.lens (\Duration' {unit} -> unit) (\s@Duration' {} a -> s {unit = a} :: Duration)

-- | Integer to specify the value of a maintenance schedule duration. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
duration_value :: Lens.Lens' Duration (Core.Maybe Core.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

instance Core.FromJSON Duration where
  parseJSON =
    Core.withObject
      "Duration"
      ( \x ->
          Duration'
            Core.<$> (x Core..:? "Unit") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable Duration

instance Core.NFData Duration

instance Core.ToJSON Duration where
  toJSON Duration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Unit" Core..=) Core.<$> unit,
            ("Value" Core..=) Core.<$> value
          ]
      )
