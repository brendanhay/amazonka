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
-- Module      : Amazonka.Lambda.Types.ScalingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ScalingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | (Amazon SQS only) The scaling configuration for the event source. To
-- remove the configuration, pass an empty value.
--
-- /See:/ 'newScalingConfig' smart constructor.
data ScalingConfig = ScalingConfig'
  { -- | Limits the number of concurrent instances that the Amazon SQS event
    -- source can invoke.
    maximumConcurrency :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumConcurrency', 'scalingConfig_maximumConcurrency' - Limits the number of concurrent instances that the Amazon SQS event
-- source can invoke.
newScalingConfig ::
  ScalingConfig
newScalingConfig =
  ScalingConfig'
    { maximumConcurrency =
        Prelude.Nothing
    }

-- | Limits the number of concurrent instances that the Amazon SQS event
-- source can invoke.
scalingConfig_maximumConcurrency :: Lens.Lens' ScalingConfig (Prelude.Maybe Prelude.Natural)
scalingConfig_maximumConcurrency = Lens.lens (\ScalingConfig' {maximumConcurrency} -> maximumConcurrency) (\s@ScalingConfig' {} a -> s {maximumConcurrency = a} :: ScalingConfig)

instance Data.FromJSON ScalingConfig where
  parseJSON =
    Data.withObject
      "ScalingConfig"
      ( \x ->
          ScalingConfig'
            Prelude.<$> (x Data..:? "MaximumConcurrency")
      )

instance Prelude.Hashable ScalingConfig where
  hashWithSalt _salt ScalingConfig' {..} =
    _salt `Prelude.hashWithSalt` maximumConcurrency

instance Prelude.NFData ScalingConfig where
  rnf ScalingConfig' {..} =
    Prelude.rnf maximumConcurrency

instance Data.ToJSON ScalingConfig where
  toJSON ScalingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumConcurrency" Data..=)
              Prelude.<$> maximumConcurrency
          ]
      )
