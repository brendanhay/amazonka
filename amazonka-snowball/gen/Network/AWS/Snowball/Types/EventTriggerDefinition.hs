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
-- Module      : Network.AWS.Snowball.Types.EventTriggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EventTriggerDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The container for the EventTriggerDefinition$EventResourceARN.
--
-- /See:/ 'newEventTriggerDefinition' smart constructor.
data EventTriggerDefinition = EventTriggerDefinition'
  { -- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
    -- an AWS Lambda function\'s event trigger associated with this job.
    eventResourceARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventTriggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventResourceARN', 'eventTriggerDefinition_eventResourceARN' - The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
-- an AWS Lambda function\'s event trigger associated with this job.
newEventTriggerDefinition ::
  EventTriggerDefinition
newEventTriggerDefinition =
  EventTriggerDefinition'
    { eventResourceARN =
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
-- an AWS Lambda function\'s event trigger associated with this job.
eventTriggerDefinition_eventResourceARN :: Lens.Lens' EventTriggerDefinition (Core.Maybe Core.Text)
eventTriggerDefinition_eventResourceARN = Lens.lens (\EventTriggerDefinition' {eventResourceARN} -> eventResourceARN) (\s@EventTriggerDefinition' {} a -> s {eventResourceARN = a} :: EventTriggerDefinition)

instance Core.FromJSON EventTriggerDefinition where
  parseJSON =
    Core.withObject
      "EventTriggerDefinition"
      ( \x ->
          EventTriggerDefinition'
            Core.<$> (x Core..:? "EventResourceARN")
      )

instance Core.Hashable EventTriggerDefinition

instance Core.NFData EventTriggerDefinition

instance Core.ToJSON EventTriggerDefinition where
  toJSON EventTriggerDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventResourceARN" Core..=)
              Core.<$> eventResourceARN
          ]
      )
