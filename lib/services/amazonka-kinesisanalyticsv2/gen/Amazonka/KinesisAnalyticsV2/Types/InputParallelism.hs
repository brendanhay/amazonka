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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputParallelism
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputParallelism where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, describes the number
-- of in-application streams to create for a given streaming source.
--
-- /See:/ 'newInputParallelism' smart constructor.
data InputParallelism = InputParallelism'
  { -- | The number of in-application streams to create.
    count :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputParallelism' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'inputParallelism_count' - The number of in-application streams to create.
newInputParallelism ::
  InputParallelism
newInputParallelism =
  InputParallelism' {count = Prelude.Nothing}

-- | The number of in-application streams to create.
inputParallelism_count :: Lens.Lens' InputParallelism (Prelude.Maybe Prelude.Natural)
inputParallelism_count = Lens.lens (\InputParallelism' {count} -> count) (\s@InputParallelism' {} a -> s {count = a} :: InputParallelism)

instance Core.FromJSON InputParallelism where
  parseJSON =
    Core.withObject
      "InputParallelism"
      ( \x ->
          InputParallelism' Prelude.<$> (x Core..:? "Count")
      )

instance Prelude.Hashable InputParallelism where
  hashWithSalt _salt InputParallelism' {..} =
    _salt `Prelude.hashWithSalt` count

instance Prelude.NFData InputParallelism where
  rnf InputParallelism' {..} = Prelude.rnf count

instance Core.ToJSON InputParallelism where
  toJSON InputParallelism' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Count" Core..=) Prelude.<$> count]
      )
