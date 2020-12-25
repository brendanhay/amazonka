{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.EventTriggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EventTriggerDefinition
  ( EventTriggerDefinition (..),

    -- * Smart constructor
    mkEventTriggerDefinition,

    -- * Lenses
    etdEventResourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.ResourceARN as Types

-- | The container for the 'EventTriggerDefinition$EventResourceARN' .
--
-- /See:/ 'mkEventTriggerDefinition' smart constructor.
newtype EventTriggerDefinition = EventTriggerDefinition'
  { -- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
    eventResourceARN :: Core.Maybe Types.ResourceARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EventTriggerDefinition' value with any optional fields omitted.
mkEventTriggerDefinition ::
  EventTriggerDefinition
mkEventTriggerDefinition =
  EventTriggerDefinition' {eventResourceARN = Core.Nothing}

-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
--
-- /Note:/ Consider using 'eventResourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etdEventResourceARN :: Lens.Lens' EventTriggerDefinition (Core.Maybe Types.ResourceARN)
etdEventResourceARN = Lens.field @"eventResourceARN"
{-# DEPRECATED etdEventResourceARN "Use generic-lens or generic-optics with 'eventResourceARN' instead." #-}

instance Core.FromJSON EventTriggerDefinition where
  toJSON EventTriggerDefinition {..} =
    Core.object
      ( Core.catMaybes
          [("EventResourceARN" Core..=) Core.<$> eventResourceARN]
      )

instance Core.FromJSON EventTriggerDefinition where
  parseJSON =
    Core.withObject "EventTriggerDefinition" Core.$
      \x ->
        EventTriggerDefinition' Core.<$> (x Core..:? "EventResourceARN")
