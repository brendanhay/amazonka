{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
  ( MediaPackageOutputDestinationSettings (..),

    -- * Smart constructor
    mkMediaPackageOutputDestinationSettings,

    -- * Lenses
    mpodsChannelId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | MediaPackage Output Destination Settings
--
-- /See:/ 'mkMediaPackageOutputDestinationSettings' smart constructor.
newtype MediaPackageOutputDestinationSettings = MediaPackageOutputDestinationSettings'
  { -- | ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
    channelId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MediaPackageOutputDestinationSettings' value with any optional fields omitted.
mkMediaPackageOutputDestinationSettings ::
  MediaPackageOutputDestinationSettings
mkMediaPackageOutputDestinationSettings =
  MediaPackageOutputDestinationSettings' {channelId = Core.Nothing}

-- | ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpodsChannelId :: Lens.Lens' MediaPackageOutputDestinationSettings (Core.Maybe Core.Text)
mpodsChannelId = Lens.field @"channelId"
{-# DEPRECATED mpodsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Core.FromJSON MediaPackageOutputDestinationSettings where
  toJSON MediaPackageOutputDestinationSettings {..} =
    Core.object
      (Core.catMaybes [("channelId" Core..=) Core.<$> channelId])

instance Core.FromJSON MediaPackageOutputDestinationSettings where
  parseJSON =
    Core.withObject "MediaPackageOutputDestinationSettings" Core.$
      \x ->
        MediaPackageOutputDestinationSettings'
          Core.<$> (x Core..:? "channelId")
