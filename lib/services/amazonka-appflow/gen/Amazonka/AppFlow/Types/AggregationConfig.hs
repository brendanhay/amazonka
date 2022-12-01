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
-- Module      : Amazonka.AppFlow.Types.AggregationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.AggregationConfig where

import Amazonka.AppFlow.Types.AggregationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The aggregation settings that you can use to customize the output format
-- of your flow data.
--
-- /See:/ 'newAggregationConfig' smart constructor.
data AggregationConfig = AggregationConfig'
  { -- | Specifies whether Amazon AppFlow aggregates the flow records into a
    -- single file, or leave them unaggregated.
    aggregationType :: Prelude.Maybe AggregationType,
    -- | The desired file size, in MB, for each output file that Amazon AppFlow
    -- writes to the flow destination. For each file, Amazon AppFlow attempts
    -- to achieve the size that you specify. The actual file sizes might differ
    -- from this target based on the number and size of the records that each
    -- file contains.
    targetFileSize :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationType', 'aggregationConfig_aggregationType' - Specifies whether Amazon AppFlow aggregates the flow records into a
-- single file, or leave them unaggregated.
--
-- 'targetFileSize', 'aggregationConfig_targetFileSize' - The desired file size, in MB, for each output file that Amazon AppFlow
-- writes to the flow destination. For each file, Amazon AppFlow attempts
-- to achieve the size that you specify. The actual file sizes might differ
-- from this target based on the number and size of the records that each
-- file contains.
newAggregationConfig ::
  AggregationConfig
newAggregationConfig =
  AggregationConfig'
    { aggregationType =
        Prelude.Nothing,
      targetFileSize = Prelude.Nothing
    }

-- | Specifies whether Amazon AppFlow aggregates the flow records into a
-- single file, or leave them unaggregated.
aggregationConfig_aggregationType :: Lens.Lens' AggregationConfig (Prelude.Maybe AggregationType)
aggregationConfig_aggregationType = Lens.lens (\AggregationConfig' {aggregationType} -> aggregationType) (\s@AggregationConfig' {} a -> s {aggregationType = a} :: AggregationConfig)

-- | The desired file size, in MB, for each output file that Amazon AppFlow
-- writes to the flow destination. For each file, Amazon AppFlow attempts
-- to achieve the size that you specify. The actual file sizes might differ
-- from this target based on the number and size of the records that each
-- file contains.
aggregationConfig_targetFileSize :: Lens.Lens' AggregationConfig (Prelude.Maybe Prelude.Integer)
aggregationConfig_targetFileSize = Lens.lens (\AggregationConfig' {targetFileSize} -> targetFileSize) (\s@AggregationConfig' {} a -> s {targetFileSize = a} :: AggregationConfig)

instance Core.FromJSON AggregationConfig where
  parseJSON =
    Core.withObject
      "AggregationConfig"
      ( \x ->
          AggregationConfig'
            Prelude.<$> (x Core..:? "aggregationType")
            Prelude.<*> (x Core..:? "targetFileSize")
      )

instance Prelude.Hashable AggregationConfig where
  hashWithSalt _salt AggregationConfig' {..} =
    _salt `Prelude.hashWithSalt` aggregationType
      `Prelude.hashWithSalt` targetFileSize

instance Prelude.NFData AggregationConfig where
  rnf AggregationConfig' {..} =
    Prelude.rnf aggregationType
      `Prelude.seq` Prelude.rnf targetFileSize

instance Core.ToJSON AggregationConfig where
  toJSON AggregationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("aggregationType" Core..=)
              Prelude.<$> aggregationType,
            ("targetFileSize" Core..=)
              Prelude.<$> targetFileSize
          ]
      )
