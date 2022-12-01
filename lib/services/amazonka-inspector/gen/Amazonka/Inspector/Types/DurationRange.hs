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
-- Module      : Amazonka.Inspector.Types.DurationRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.DurationRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | This data type is used in the AssessmentTemplateFilter data type.
--
-- /See:/ 'newDurationRange' smart constructor.
data DurationRange = DurationRange'
  { -- | The minimum value of the duration range. Must be greater than zero.
    minSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum value of the duration range. Must be less than or equal to
    -- 604800 seconds (1 week).
    maxSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { minSeconds = Prelude.Nothing,
      maxSeconds = Prelude.Nothing
    }

-- | The minimum value of the duration range. Must be greater than zero.
durationRange_minSeconds :: Lens.Lens' DurationRange (Prelude.Maybe Prelude.Natural)
durationRange_minSeconds = Lens.lens (\DurationRange' {minSeconds} -> minSeconds) (\s@DurationRange' {} a -> s {minSeconds = a} :: DurationRange)

-- | The maximum value of the duration range. Must be less than or equal to
-- 604800 seconds (1 week).
durationRange_maxSeconds :: Lens.Lens' DurationRange (Prelude.Maybe Prelude.Natural)
durationRange_maxSeconds = Lens.lens (\DurationRange' {maxSeconds} -> maxSeconds) (\s@DurationRange' {} a -> s {maxSeconds = a} :: DurationRange)

instance Prelude.Hashable DurationRange where
  hashWithSalt _salt DurationRange' {..} =
    _salt `Prelude.hashWithSalt` minSeconds
      `Prelude.hashWithSalt` maxSeconds

instance Prelude.NFData DurationRange where
  rnf DurationRange' {..} =
    Prelude.rnf minSeconds
      `Prelude.seq` Prelude.rnf maxSeconds

instance Core.ToJSON DurationRange where
  toJSON DurationRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minSeconds" Core..=) Prelude.<$> minSeconds,
            ("maxSeconds" Core..=) Prelude.<$> maxSeconds
          ]
      )
