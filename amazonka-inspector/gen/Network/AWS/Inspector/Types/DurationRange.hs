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
-- Module      : Network.AWS.Inspector.Types.DurationRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.DurationRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used in the AssessmentTemplateFilter data type.
--
-- /See:/ 'newDurationRange' smart constructor.
data DurationRange = DurationRange'
  { -- | The minimum value of the duration range. Must be greater than zero.
    minSeconds :: Core.Maybe Core.Natural,
    -- | The maximum value of the duration range. Must be less than or equal to
    -- 604800 seconds (1 week).
    maxSeconds :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DurationRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSeconds', 'durationRange_minSeconds' - The minimum value of the duration range. Must be greater than zero.
--
-- 'maxSeconds', 'durationRange_maxSeconds' - The maximum value of the duration range. Must be less than or equal to
-- 604800 seconds (1 week).
newDurationRange ::
  DurationRange
newDurationRange =
  DurationRange'
    { minSeconds = Core.Nothing,
      maxSeconds = Core.Nothing
    }

-- | The minimum value of the duration range. Must be greater than zero.
durationRange_minSeconds :: Lens.Lens' DurationRange (Core.Maybe Core.Natural)
durationRange_minSeconds = Lens.lens (\DurationRange' {minSeconds} -> minSeconds) (\s@DurationRange' {} a -> s {minSeconds = a} :: DurationRange)

-- | The maximum value of the duration range. Must be less than or equal to
-- 604800 seconds (1 week).
durationRange_maxSeconds :: Lens.Lens' DurationRange (Core.Maybe Core.Natural)
durationRange_maxSeconds = Lens.lens (\DurationRange' {maxSeconds} -> maxSeconds) (\s@DurationRange' {} a -> s {maxSeconds = a} :: DurationRange)

instance Core.Hashable DurationRange

instance Core.NFData DurationRange

instance Core.ToJSON DurationRange where
  toJSON DurationRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("minSeconds" Core..=) Core.<$> minSeconds,
            ("maxSeconds" Core..=) Core.<$> maxSeconds
          ]
      )
