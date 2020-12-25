{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelActivity
  ( ChannelActivity (..),

    -- * Smart constructor
    mkChannelActivity,

    -- * Lenses
    caName,
    caChannelName,
    caNext,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The activity that determines the source of the messages to be processed.
--
-- /See:/ 'mkChannelActivity' smart constructor.
data ChannelActivity = ChannelActivity'
  { -- | The name of the channel activity.
    name :: Types.ActivityName,
    -- | The name of the channel from which the messages are processed.
    channelName :: Types.ChannelName,
    -- | The next activity in the pipeline.
    next :: Core.Maybe Types.ActivityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelActivity' value with any optional fields omitted.
mkChannelActivity ::
  -- | 'name'
  Types.ActivityName ->
  -- | 'channelName'
  Types.ChannelName ->
  ChannelActivity
mkChannelActivity name channelName =
  ChannelActivity' {name, channelName, next = Core.Nothing}

-- | The name of the channel activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' ChannelActivity Types.ActivityName
caName = Lens.field @"name"
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the channel from which the messages are processed.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caChannelName :: Lens.Lens' ChannelActivity Types.ChannelName
caChannelName = Lens.field @"channelName"
{-# DEPRECATED caChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNext :: Lens.Lens' ChannelActivity (Core.Maybe Types.ActivityName)
caNext = Lens.field @"next"
{-# DEPRECATED caNext "Use generic-lens or generic-optics with 'next' instead." #-}

instance Core.FromJSON ChannelActivity where
  toJSON ChannelActivity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("channelName" Core..= channelName),
            ("next" Core..=) Core.<$> next
          ]
      )

instance Core.FromJSON ChannelActivity where
  parseJSON =
    Core.withObject "ChannelActivity" Core.$
      \x ->
        ChannelActivity'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "channelName")
          Core.<*> (x Core..:? "next")
