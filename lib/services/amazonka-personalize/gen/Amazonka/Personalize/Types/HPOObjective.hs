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
-- Module      : Amazonka.Personalize.Types.HPOObjective
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.HPOObjective where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metric to optimize during hyperparameter optimization (HPO).
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
--
-- /See:/ 'newHPOObjective' smart constructor.
data HPOObjective = HPOObjective'
  { -- | The type of the metric. Valid values are @Maximize@ and @Minimize@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A regular expression for finding the metric in the training job logs.
    metricRegex :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HPOObjective' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'hPOObjective_type' - The type of the metric. Valid values are @Maximize@ and @Minimize@.
--
-- 'metricName', 'hPOObjective_metricName' - The name of the metric.
--
-- 'metricRegex', 'hPOObjective_metricRegex' - A regular expression for finding the metric in the training job logs.
newHPOObjective ::
  HPOObjective
newHPOObjective =
  HPOObjective'
    { type' = Prelude.Nothing,
      metricName = Prelude.Nothing,
      metricRegex = Prelude.Nothing
    }

-- | The type of the metric. Valid values are @Maximize@ and @Minimize@.
hPOObjective_type :: Lens.Lens' HPOObjective (Prelude.Maybe Prelude.Text)
hPOObjective_type = Lens.lens (\HPOObjective' {type'} -> type') (\s@HPOObjective' {} a -> s {type' = a} :: HPOObjective)

-- | The name of the metric.
hPOObjective_metricName :: Lens.Lens' HPOObjective (Prelude.Maybe Prelude.Text)
hPOObjective_metricName = Lens.lens (\HPOObjective' {metricName} -> metricName) (\s@HPOObjective' {} a -> s {metricName = a} :: HPOObjective)

-- | A regular expression for finding the metric in the training job logs.
hPOObjective_metricRegex :: Lens.Lens' HPOObjective (Prelude.Maybe Prelude.Text)
hPOObjective_metricRegex = Lens.lens (\HPOObjective' {metricRegex} -> metricRegex) (\s@HPOObjective' {} a -> s {metricRegex = a} :: HPOObjective)

instance Data.FromJSON HPOObjective where
  parseJSON =
    Data.withObject
      "HPOObjective"
      ( \x ->
          HPOObjective'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "metricName")
            Prelude.<*> (x Data..:? "metricRegex")
      )

instance Prelude.Hashable HPOObjective where
  hashWithSalt _salt HPOObjective' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metricRegex

instance Prelude.NFData HPOObjective where
  rnf HPOObjective' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf metricRegex

instance Data.ToJSON HPOObjective where
  toJSON HPOObjective' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("type" Data..=) Prelude.<$> type',
            ("metricName" Data..=) Prelude.<$> metricName,
            ("metricRegex" Data..=) Prelude.<$> metricRegex
          ]
      )
