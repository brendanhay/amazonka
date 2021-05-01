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
-- Module      : Network.AWS.Pinpoint.Types.EventFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventFilter where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for an event that causes a campaign to be sent or
-- a journey activity to be performed.
--
-- /See:/ 'newEventFilter' smart constructor.
data EventFilter = EventFilter'
  { -- | The type of event that causes the campaign to be sent or the journey
    -- activity to be performed. Valid values are: SYSTEM, sends the campaign
    -- or performs the activity when a system event occurs; and, ENDPOINT,
    -- sends the campaign or performs the activity when an endpoint event
    -- (Events resource) occurs.
    filterType :: FilterType,
    -- | The dimensions for the event filter to use for the campaign or the
    -- journey activity.
    dimensions :: EventDimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterType', 'eventFilter_filterType' - The type of event that causes the campaign to be sent or the journey
-- activity to be performed. Valid values are: SYSTEM, sends the campaign
-- or performs the activity when a system event occurs; and, ENDPOINT,
-- sends the campaign or performs the activity when an endpoint event
-- (Events resource) occurs.
--
-- 'dimensions', 'eventFilter_dimensions' - The dimensions for the event filter to use for the campaign or the
-- journey activity.
newEventFilter ::
  -- | 'filterType'
  FilterType ->
  -- | 'dimensions'
  EventDimensions ->
  EventFilter
newEventFilter pFilterType_ pDimensions_ =
  EventFilter'
    { filterType = pFilterType_,
      dimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent or the journey
-- activity to be performed. Valid values are: SYSTEM, sends the campaign
-- or performs the activity when a system event occurs; and, ENDPOINT,
-- sends the campaign or performs the activity when an endpoint event
-- (Events resource) occurs.
eventFilter_filterType :: Lens.Lens' EventFilter FilterType
eventFilter_filterType = Lens.lens (\EventFilter' {filterType} -> filterType) (\s@EventFilter' {} a -> s {filterType = a} :: EventFilter)

-- | The dimensions for the event filter to use for the campaign or the
-- journey activity.
eventFilter_dimensions :: Lens.Lens' EventFilter EventDimensions
eventFilter_dimensions = Lens.lens (\EventFilter' {dimensions} -> dimensions) (\s@EventFilter' {} a -> s {dimensions = a} :: EventFilter)

instance Prelude.FromJSON EventFilter where
  parseJSON =
    Prelude.withObject
      "EventFilter"
      ( \x ->
          EventFilter'
            Prelude.<$> (x Prelude..: "FilterType")
            Prelude.<*> (x Prelude..: "Dimensions")
      )

instance Prelude.Hashable EventFilter

instance Prelude.NFData EventFilter

instance Prelude.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FilterType" Prelude..= filterType),
            Prelude.Just ("Dimensions" Prelude..= dimensions)
          ]
      )
