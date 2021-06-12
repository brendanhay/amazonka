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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelism
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputParallelism where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the number of in-application streams to create for a given
-- streaming source. For information about parallelism, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- /See:/ 'newInputParallelism' smart constructor.
data InputParallelism = InputParallelism'
  { -- | Number of in-application streams to create. For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits>.
    count :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputParallelism' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'inputParallelism_count' - Number of in-application streams to create. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits>.
newInputParallelism ::
  InputParallelism
newInputParallelism =
  InputParallelism' {count = Core.Nothing}

-- | Number of in-application streams to create. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits>.
inputParallelism_count :: Lens.Lens' InputParallelism (Core.Maybe Core.Natural)
inputParallelism_count = Lens.lens (\InputParallelism' {count} -> count) (\s@InputParallelism' {} a -> s {count = a} :: InputParallelism)

instance Core.FromJSON InputParallelism where
  parseJSON =
    Core.withObject
      "InputParallelism"
      ( \x ->
          InputParallelism' Core.<$> (x Core..:? "Count")
      )

instance Core.Hashable InputParallelism

instance Core.NFData InputParallelism

instance Core.ToJSON InputParallelism where
  toJSON InputParallelism' {..} =
    Core.object
      (Core.catMaybes [("Count" Core..=) Core.<$> count])
