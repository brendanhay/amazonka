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
-- Module      : Amazonka.Pinpoint.Types.StartCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.StartCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.EventStartCondition
import Amazonka.Pinpoint.Types.SegmentCondition
import qualified Amazonka.Prelude as Prelude

-- | Specifies the conditions for the first activity in a journey. This
-- activity and its conditions determine which users are participants in a
-- journey.
--
-- /See:/ 'newStartCondition' smart constructor.
data StartCondition = StartCondition'
  { eventStartCondition :: Prelude.Maybe EventStartCondition,
    -- | The segment that\'s associated with the first activity in the journey.
    -- This segment determines which users are participants in the journey.
    segmentStartCondition :: Prelude.Maybe SegmentCondition,
    -- | The custom description of the condition.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventStartCondition', 'startCondition_eventStartCondition' - Undocumented member.
--
-- 'segmentStartCondition', 'startCondition_segmentStartCondition' - The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
--
-- 'description', 'startCondition_description' - The custom description of the condition.
newStartCondition ::
  StartCondition
newStartCondition =
  StartCondition'
    { eventStartCondition =
        Prelude.Nothing,
      segmentStartCondition = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Undocumented member.
startCondition_eventStartCondition :: Lens.Lens' StartCondition (Prelude.Maybe EventStartCondition)
startCondition_eventStartCondition = Lens.lens (\StartCondition' {eventStartCondition} -> eventStartCondition) (\s@StartCondition' {} a -> s {eventStartCondition = a} :: StartCondition)

-- | The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
startCondition_segmentStartCondition :: Lens.Lens' StartCondition (Prelude.Maybe SegmentCondition)
startCondition_segmentStartCondition = Lens.lens (\StartCondition' {segmentStartCondition} -> segmentStartCondition) (\s@StartCondition' {} a -> s {segmentStartCondition = a} :: StartCondition)

-- | The custom description of the condition.
startCondition_description :: Lens.Lens' StartCondition (Prelude.Maybe Prelude.Text)
startCondition_description = Lens.lens (\StartCondition' {description} -> description) (\s@StartCondition' {} a -> s {description = a} :: StartCondition)

instance Core.FromJSON StartCondition where
  parseJSON =
    Core.withObject
      "StartCondition"
      ( \x ->
          StartCondition'
            Prelude.<$> (x Core..:? "EventStartCondition")
            Prelude.<*> (x Core..:? "SegmentStartCondition")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable StartCondition where
  hashWithSalt _salt StartCondition' {..} =
    _salt `Prelude.hashWithSalt` eventStartCondition
      `Prelude.hashWithSalt` segmentStartCondition
      `Prelude.hashWithSalt` description

instance Prelude.NFData StartCondition where
  rnf StartCondition' {..} =
    Prelude.rnf eventStartCondition
      `Prelude.seq` Prelude.rnf segmentStartCondition
      `Prelude.seq` Prelude.rnf description

instance Core.ToJSON StartCondition where
  toJSON StartCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventStartCondition" Core..=)
              Prelude.<$> eventStartCondition,
            ("SegmentStartCondition" Core..=)
              Prelude.<$> segmentStartCondition,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
