{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.StartCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.StartCondition where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventStartCondition
import Network.AWS.Pinpoint.Types.SegmentCondition
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the conditions for the first activity in a journey. This
-- activity and its conditions determine which users are participants in a
-- journey.
--
-- /See:/ 'newStartCondition' smart constructor.
data StartCondition = StartCondition'
  { eventStartCondition :: Prelude.Maybe EventStartCondition,
    -- | The custom description of the condition.
    description :: Prelude.Maybe Prelude.Text,
    -- | The segment that\'s associated with the first activity in the journey.
    -- This segment determines which users are participants in the journey.
    segmentStartCondition :: Prelude.Maybe SegmentCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'description', 'startCondition_description' - The custom description of the condition.
--
-- 'segmentStartCondition', 'startCondition_segmentStartCondition' - The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
newStartCondition ::
  StartCondition
newStartCondition =
  StartCondition'
    { eventStartCondition =
        Prelude.Nothing,
      description = Prelude.Nothing,
      segmentStartCondition = Prelude.Nothing
    }

-- | Undocumented member.
startCondition_eventStartCondition :: Lens.Lens' StartCondition (Prelude.Maybe EventStartCondition)
startCondition_eventStartCondition = Lens.lens (\StartCondition' {eventStartCondition} -> eventStartCondition) (\s@StartCondition' {} a -> s {eventStartCondition = a} :: StartCondition)

-- | The custom description of the condition.
startCondition_description :: Lens.Lens' StartCondition (Prelude.Maybe Prelude.Text)
startCondition_description = Lens.lens (\StartCondition' {description} -> description) (\s@StartCondition' {} a -> s {description = a} :: StartCondition)

-- | The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
startCondition_segmentStartCondition :: Lens.Lens' StartCondition (Prelude.Maybe SegmentCondition)
startCondition_segmentStartCondition = Lens.lens (\StartCondition' {segmentStartCondition} -> segmentStartCondition) (\s@StartCondition' {} a -> s {segmentStartCondition = a} :: StartCondition)

instance Prelude.FromJSON StartCondition where
  parseJSON =
    Prelude.withObject
      "StartCondition"
      ( \x ->
          StartCondition'
            Prelude.<$> (x Prelude..:? "EventStartCondition")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SegmentStartCondition")
      )

instance Prelude.Hashable StartCondition

instance Prelude.NFData StartCondition

instance Prelude.ToJSON StartCondition where
  toJSON StartCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventStartCondition" Prelude..=)
              Prelude.<$> eventStartCondition,
            ("Description" Prelude..=) Prelude.<$> description,
            ("SegmentStartCondition" Prelude..=)
              Prelude.<$> segmentStartCondition
          ]
      )
