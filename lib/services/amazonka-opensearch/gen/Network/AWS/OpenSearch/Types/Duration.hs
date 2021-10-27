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
-- Module      : Network.AWS.OpenSearch.Types.Duration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.Duration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.TimeUnit
import qualified Network.AWS.Prelude as Prelude

-- | The maintenance schedule duration: duration value and duration unit. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | Integer to specify the value of a maintenance schedule duration. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
    -- for more information.
    value :: Prelude.Maybe Prelude.Natural,
    -- | The unit of a maintenance schedule duration. Valid value is HOURS. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
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
-- 'value', 'duration_value' - Integer to specify the value of a maintenance schedule duration. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- 'unit', 'duration_unit' - The unit of a maintenance schedule duration. Valid value is HOURS. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
newDuration ::
  Duration
newDuration =
  Duration'
    { value = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | Integer to specify the value of a maintenance schedule duration. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
duration_value :: Lens.Lens' Duration (Prelude.Maybe Prelude.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

-- | The unit of a maintenance schedule duration. Valid value is HOURS. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
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
