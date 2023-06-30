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
-- Module      : Amazonka.Pinpoint.Types.EventFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EventFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EventDimensions
import Amazonka.Pinpoint.Types.FilterType
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON EventFilter where
  parseJSON =
    Data.withObject
      "EventFilter"
      ( \x ->
          EventFilter'
            Prelude.<$> (x Data..: "FilterType")
            Prelude.<*> (x Data..: "Dimensions")
      )

instance Prelude.Hashable EventFilter where
  hashWithSalt _salt EventFilter' {..} =
    _salt
      `Prelude.hashWithSalt` filterType
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData EventFilter where
  rnf EventFilter' {..} =
    Prelude.rnf filterType
      `Prelude.seq` Prelude.rnf dimensions

instance Data.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FilterType" Data..= filterType),
            Prelude.Just ("Dimensions" Data..= dimensions)
          ]
      )
