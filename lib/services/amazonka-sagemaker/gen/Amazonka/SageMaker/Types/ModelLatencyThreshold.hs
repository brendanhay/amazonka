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
-- Module      : Amazonka.SageMaker.Types.ModelLatencyThreshold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelLatencyThreshold where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The model latency threshold.
--
-- /See:/ 'newModelLatencyThreshold' smart constructor.
data ModelLatencyThreshold = ModelLatencyThreshold'
  { -- | The model latency percentile threshold.
    percentile :: Prelude.Maybe Prelude.Text,
    -- | The model latency percentile value in milliseconds.
    valueInMilliseconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelLatencyThreshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentile', 'modelLatencyThreshold_percentile' - The model latency percentile threshold.
--
-- 'valueInMilliseconds', 'modelLatencyThreshold_valueInMilliseconds' - The model latency percentile value in milliseconds.
newModelLatencyThreshold ::
  ModelLatencyThreshold
newModelLatencyThreshold =
  ModelLatencyThreshold'
    { percentile =
        Prelude.Nothing,
      valueInMilliseconds = Prelude.Nothing
    }

-- | The model latency percentile threshold.
modelLatencyThreshold_percentile :: Lens.Lens' ModelLatencyThreshold (Prelude.Maybe Prelude.Text)
modelLatencyThreshold_percentile = Lens.lens (\ModelLatencyThreshold' {percentile} -> percentile) (\s@ModelLatencyThreshold' {} a -> s {percentile = a} :: ModelLatencyThreshold)

-- | The model latency percentile value in milliseconds.
modelLatencyThreshold_valueInMilliseconds :: Lens.Lens' ModelLatencyThreshold (Prelude.Maybe Prelude.Int)
modelLatencyThreshold_valueInMilliseconds = Lens.lens (\ModelLatencyThreshold' {valueInMilliseconds} -> valueInMilliseconds) (\s@ModelLatencyThreshold' {} a -> s {valueInMilliseconds = a} :: ModelLatencyThreshold)

instance Data.FromJSON ModelLatencyThreshold where
  parseJSON =
    Data.withObject
      "ModelLatencyThreshold"
      ( \x ->
          ModelLatencyThreshold'
            Prelude.<$> (x Data..:? "Percentile")
            Prelude.<*> (x Data..:? "ValueInMilliseconds")
      )

instance Prelude.Hashable ModelLatencyThreshold where
  hashWithSalt _salt ModelLatencyThreshold' {..} =
    _salt
      `Prelude.hashWithSalt` percentile
      `Prelude.hashWithSalt` valueInMilliseconds

instance Prelude.NFData ModelLatencyThreshold where
  rnf ModelLatencyThreshold' {..} =
    Prelude.rnf percentile
      `Prelude.seq` Prelude.rnf valueInMilliseconds

instance Data.ToJSON ModelLatencyThreshold where
  toJSON ModelLatencyThreshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Percentile" Data..=) Prelude.<$> percentile,
            ("ValueInMilliseconds" Data..=)
              Prelude.<$> valueInMilliseconds
          ]
      )
